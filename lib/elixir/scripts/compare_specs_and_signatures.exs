#!/usr/bin/env elixir

# Compares public @spec declarations against the type signatures the compiler
# infers (checker "ExCk" chunk), to find (a) specs that should be updated for
# documentation/dialyzer purposes and (b) incorrect inference.
#
# How it decides (mechanically, in the Descr lattice)
# ---------------------------------------------------
# @spec typespecs are TRANSLATED into Module.Types.Descr descrs with precision
# tracking (SpecToDescr): non_neg_integer() maps to integer() marked
# approximate, user/remote types are expanded recursively, `when` constraints
# substituted. Then each function gets a verdict computed with subtype?/equal?
# rather than by comparing pretty-printed strings:
#
#   :equivalent        same domain and return (up to the lattice) -> dropped
#   :inference_wider   spec <= inferred; expected conservatism    -> dropped
#   :spec_wider_return inferred return strictly inside the spec return with an
#                      EXACT translation -> spec-tightening candidate
#   :contradiction     spec and inferred returns are disjoint -> someone is
#                      definitely wrong
#   :mixed             none of the above -> needs judgment
#
# Return comparison is SLICE-WISE: for each spec clause, the effective
# inferred return is the union of returns of inferred clauses whose domains
# intersect that spec clause's domain (inferred clauses are overlapping upper
# bounds, so this is the checker's actual prediction for calls in that slice).
#
# Executable tie-breaker
# ----------------------
# For residue functions in a pure-module allowlist, concrete WITNESSES are
# executed in sandboxed tasks: rejection-sampled argument values inside the
# translated spec domain, plus (args, result) pairs mined from the module's
# own DOCTESTS (maintainer-blessed inputs). Outcomes:
#
#   result outside an exactly-translated spec return
#     -> SPEC PROVEN WRONG (mechanical certainty; documentation bug)
#   result outside the inferred effective return (upper bound)
#     -> INFERENCE PROVEN WRONG (checker bug)
#   witness consistent with one side only -> evidence attached to the residue
#
# Only the remaining residue (typically a few % of functions) is emitted for
# LLM/human review, with verdicts, precision flags, and witness evidence
# attached -- the reviewer no longer judges string equivalence.
#
# Usage
# -----
#   ./bin/elixir lib/elixir/scripts/compare_specs_and_signatures.exs [options]
#
#   --app APP         Standard library app to scan (repeatable).
#                     Default: elixir eex ex_unit iex logger mix
#   --module MODULE   Restrict to a module, e.g. Enum (repeatable).
#   --format FORMAT   report (default) | json | prompt
#   --output PATH     Write output to PATH instead of stdout.
#   --limit N         Limit number of modules after filtering.
#   --no-exec         Disable the executable tie-breaker.
#   --exclusions PATH Acknowledged-findings file: one Module.function/arity
#                     per line, anything after "#" is a comment. Enables
#                     STRICT gating: every residue entry must be listed or
#                     the run fails. Entries that no longer match anything
#                     are reported as stale (warning, not fatal).
#   --help            This help.
#
# Exit status: 1 if any PROVEN finding exists (spec or inference contradicted
# by an executed witness, or a lattice contradiction); with --exclusions
# additionally 1 if any NEW (unlisted) residue entry appears; 0 otherwise.
# Witness sampling is PRNG-seeded, so results are deterministic on a given
# build. CI entry point: `make check_specs`.

apps_default = [:elixir, :eex, :ex_unit, :iex, :logger, :mix]

{opts, positional, invalid} =
  OptionParser.parse(
    System.argv(),
    strict: [
      app: :keep,
      module: :keep,
      format: :string,
      output: :string,
      limit: :integer,
      no_exec: :boolean,
      exclusions: :string,
      help: :boolean
    ]
  )

if invalid != [] do
  formatted = Enum.map_join(invalid, ", ", fn {key, value} -> "--#{key}=#{inspect(value)}" end)
  raise "invalid options: #{formatted}"
end

if opts[:help] do
  IO.puts("see the header of #{__ENV__.file} for usage")
  System.halt(0)
end

if positional != [] do
  raise "unexpected positional arguments: #{Enum.map_join(positional, ", ", &inspect/1)}"
end

format = opts[:format] || "report"

unless format in ["report", "json", "prompt"] do
  raise "expected --format to be one of: report, json, prompt"
end

unless Code.ensure_loaded?(Module.Types.Descr) do
  raise "Module.Types.Descr is not available; run with the local ./bin/elixir"
end

exclusions =
  case opts[:exclusions] do
    nil ->
      nil

    path ->
      path
      |> File.read!()
      |> String.split("\n")
      |> Enum.map(fn line -> line |> String.split("#", parts: 2) |> hd() |> String.trim() end)
      |> Enum.reject(&(&1 == ""))
      |> MapSet.new()
  end

# Deterministic witness sampling (Enum.shuffle in Witness.sample_in), so the
# gate cannot flap on a given build.
:rand.seed(:exsss, {20_260_709, 15_539, 1})

# ---------------------------------------------------------------------------
# Typespec (erl abstract type AST) -> descr, with precision tracking
# ---------------------------------------------------------------------------

defmodule SpecToDescr do
  import Module.Types.Descr

  @moduledoc false

  # translate/3 returns {descr, exact? :: boolean}. exact? = false means the
  # descr over-approximates the spec somewhere inside (descr cannot express
  # integer refinements, sized binaries, charlists-as-strings, ...).
  # Throws {:untranslatable, why} when no sound mapping exists.
  #
  # INVARIANT: every inexact translation must OVER-approximate the spec, never
  # under-approximate. Verdict soundness depends on it: disjointness
  # (:contradiction) and witness non-membership (:proven_spec_wrong) conclusions
  # transfer from an over-approximation to the real spec type, but an
  # under-approximation fabricates them. If a construct cannot be soundly
  # over-approximated, throw :untranslatable instead.

  def translate(type, env, depth \\ 8)

  def translate(_type, _env, 0), do: {term(), false}

  def translate({:ann_type, _, [_var, t]}, env, d), do: translate(t, env, d)
  def translate({:atom, _, a}, _env, _d), do: {atom([a]), true}
  def translate({:integer, _, _n}, _env, _d), do: {integer(), false}
  def translate({:op, _, _, _}, _env, _d), do: {integer(), false}
  def translate({:op, _, _, _, _}, _env, _d), do: {integer(), false}
  def translate({:var, _, _}, _env, _d), do: {term(), false}

  def translate({:remote_type, _, [{:atom, _, mod}, {:atom, _, name}, args]}, env, d) do
    expand_user(mod, name, args, env, d)
  end

  def translate({:user_type, _, name, args}, env, d) do
    expand_user(env.module, name, args, env, d)
  end

  def translate({:type, _, name, args}, env, d), do: builtin(name, args, env, d)
  def translate(other, _env, _d), do: throw({:untranslatable, other})

  defp builtin(:union, args, env, d) do
    Enum.reduce(args, {none(), true}, fn t, {acc, ex} ->
      {dt, ex2} = translate(t, env, d)
      {opt_union(acc, dt), ex and ex2}
    end)
  end

  defp builtin(:integer, [], _e, _d), do: {integer(), true}
  defp builtin(:non_neg_integer, [], _e, _d), do: {integer(), false}
  defp builtin(:pos_integer, [], _e, _d), do: {integer(), false}
  defp builtin(:neg_integer, [], _e, _d), do: {integer(), false}
  defp builtin(:range, _, _e, _d), do: {integer(), false}
  defp builtin(:float, [], _e, _d), do: {float(), true}
  defp builtin(:number, [], _e, _d), do: {opt_union(integer(), float()), true}
  defp builtin(:atom, [], _e, _d), do: {atom(), true}
  defp builtin(:module, [], _e, _d), do: {atom(), true}
  defp builtin(:node, [], _e, _d), do: {atom(), true}
  defp builtin(:boolean, [], _e, _d), do: {boolean(), true}
  defp builtin(:binary, [], _e, _d), do: {binary(), true}

  defp builtin(:binary, [_, _] = args, _e, _d) do
    case args do
      [{:integer, _, 0}, {:integer, _, 0}] -> {binary(), false}
      _ -> {bitstring(), false}
    end
  end

  defp builtin(:bitstring, [], _e, _d), do: {bitstring(), true}
  defp builtin(:nonempty_binary, [], _e, _d), do: {binary(), false}
  defp builtin(:nonempty_bitstring, [], _e, _d), do: {bitstring(), false}
  defp builtin(:pid, [], _e, _d), do: {pid(), true}
  defp builtin(:port, [], _e, _d), do: {port(), true}
  defp builtin(:reference, [], _e, _d), do: {reference(), true}

  defp builtin(:identifier, [], _e, _d),
    do: {opt_union(pid(), opt_union(port(), reference())), true}

  defp builtin(:iodata, [], _e, _d), do: {opt_union(binary(), iolist_over()), false}
  defp builtin(:iolist, [], _e, _d), do: {iolist_over(), false}
  defp builtin(:term, [], _e, _d), do: {term(), true}
  defp builtin(:any, [], _e, _d), do: {term(), true}
  defp builtin(:none, [], _e, _d), do: {none(), true}
  defp builtin(:no_return, [], _e, _d), do: {none(), true}
  defp builtin(:dynamic, [], _e, _d), do: {dynamic(), true}
  defp builtin(nil, [], _e, _d), do: {empty_list(), true}

  defp builtin(:list, [], _e, _d), do: {proper_list(term()), true}

  defp builtin(:list, [t], env, d) do
    {dt, ex} = translate(t, env, d)
    {proper_list(dt), ex}
  end

  defp builtin(:nonempty_list, [], _e, _d), do: {non_empty_list(term()), true}

  defp builtin(:nonempty_list, [t], env, d) do
    {dt, ex} = translate(t, env, d)
    {non_empty_list(dt), ex}
  end

  defp builtin(:maybe_improper_list, [], _e, _d),
    do: {opt_union(empty_list(), non_empty_list(term(), term())), true}

  defp builtin(:maybe_improper_list, [t, l], env, d) do
    {dt, e1} = translate(t, env, d)
    {dl, e2} = translate(l, env, d)
    {opt_union(empty_list(), non_empty_list(dt, opt_union(dl, empty_list()))), e1 and e2}
  end

  defp builtin(:nonempty_improper_list, [t, l], env, d) do
    {dt, e1} = translate(t, env, d)
    {dl, e2} = translate(l, env, d)
    {non_empty_list(dt, dl), e1 and e2}
  end

  defp builtin(:nonempty_maybe_improper_list, [t, l], env, d) do
    {dt, e1} = translate(t, env, d)
    {dl, e2} = translate(l, env, d)
    {non_empty_list(dt, opt_union(dl, empty_list())), e1 and e2}
  end

  defp builtin(:keyword, [], _e, _d), do: {proper_list(tuple([atom(), term()])), false}

  defp builtin(:keyword, [t], env, d) do
    {dt, _} = translate(t, env, d)
    {proper_list(tuple([atom(), dt])), false}
  end

  defp builtin(:string, [], _e, _d), do: {proper_list(integer()), false}
  defp builtin(:nonempty_string, [], _e, _d), do: {non_empty_list(integer()), false}
  defp builtin(:charlist, [], _e, _d), do: {proper_list(integer()), false}
  defp builtin(:nonempty_charlist, [], _e, _d), do: {non_empty_list(integer()), false}

  defp builtin(:tuple, [], _e, _d), do: {tuple(), true}
  defp builtin(:tuple, :any, _e, _d), do: {tuple(), true}

  defp builtin(:tuple, args, env, d) do
    {elems, exact} =
      Enum.map_reduce(args, true, fn t, ex ->
        {dt, ex2} = translate(t, env, d)
        {dt, ex and ex2}
      end)

    {tuple(elems), exact}
  end

  # {:type, _, :map, []} is the empty-map literal %{} (map() arrives as :any)
  defp builtin(:map, [], _e, _d), do: {empty_map(), true}
  defp builtin(:map, :any, _e, _d), do: {open_map(), true}

  defp builtin(:map, assocs, env, d) do
    {fields, exact} =
      Enum.map_reduce(assocs, true, fn
        {:type, _, :map_field_exact, [{:atom, _, k}, v]}, ex ->
          {dv, ex2} = translate(v, env, d)
          {{k, dv}, ex and ex2}

        {:type, _, :map_field_assoc, [{:atom, _, k}, v]}, ex ->
          {dv, ex2} = translate(v, env, d)
          {{k, if_set(dv)}, ex and ex2}

        {:type, _, _assoc_kind, [_k, v]}, ex ->
          # wide/non-literal key: over-approximate by opening the map
          {_dv, _} = translate(v, env, d)
          {:open_rest, ex and false}
      end)

    open? = Enum.any?(fields, &(&1 == :open_rest))
    fields = Enum.reject(fields, &(&1 == :open_rest))

    if open? do
      {open_map(fields), exact}
    else
      {closed_map(fields), exact}
    end
  end

  defp builtin(:fun, [], _e, _d), do: {fun(), true}

  defp builtin(:fun, [{:type, _, :product, args}, ret], env, d) do
    {dargs, e1} =
      Enum.map_reduce(args, true, fn t, ex ->
        {dt, ex2} = translate(t, env, d)
        {dt, ex and ex2}
      end)

    {dret, e2} = translate(ret, env, d)
    {fun(dargs, dret), e1 and e2}
  end

  defp builtin(:fun, [{:type, _, :any}, _ret], _env, _d), do: {fun(), false}
  defp builtin(:function, [], _e, _d), do: {fun(), true}
  defp builtin(:mfa, [], _e, _d), do: {tuple([atom(), atom(), integer()]), false}
  defp builtin(:arity, [], _e, _d), do: {integer(), false}
  defp builtin(:byte, [], _e, _d), do: {integer(), false}
  defp builtin(:char, [], _e, _d), do: {integer(), false}
  defp builtin(:timeout, [], _e, _d), do: {opt_union(integer(), atom([:infinity])), false}
  defp builtin(:struct, [], _e, _d), do: {open_map([{:__struct__, atom()}]), true}
  defp builtin(name, _args, _env, _d), do: throw({:untranslatable, name})

  defp proper_list(t), do: opt_union(empty_list(), non_empty_list(t))

  # Over-approximation of iolist() ==
  # maybe_improper_list(byte | binary | iolist, binary | []): elements widened
  # to term() (descr cannot express the recursion), terminator kept exact.
  # Includes improper lists like [?a | "bc"] -- an earlier proper_list(term())
  # mapping under-approximated and could fabricate contradictions.
  defp iolist_over do
    opt_union(
      empty_list(),
      non_empty_list(term(), opt_union(binary(), empty_list()))
    )
  end

  defp expand_user(mod, name, args, env, d) do
    key = {mod, name, length(args)}

    if key in env.stack do
      # recursive type: sound over-approximation
      {term(), false}
    else
      case fetch_type(mod, name, length(args)) do
        {:ok, {params, body}} ->
          subst =
            params
            |> Enum.zip(args)
            |> Map.new(fn {{:var, _, pname}, arg} -> {pname, arg} end)

          body = substitute(body, subst)
          env2 = %{env | module: mod, stack: [key | env.stack]}
          translate(body, env2, d - 1)

        :error ->
          throw({:untranslatable, {:missing_type, mod, name, length(args)}})
      end
    end
  end

  # `when var: type` specs arrive as bounded_fun.
  def debound({:type, l, :bounded_fun, [fun, constraints]}) do
    subst =
      Map.new(constraints, fn
        {:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, name}, t]]} -> {name, t}
      end)

    {:type, _, :fun, [{:type, lp, :product, args}, ret]} = fun
    args = Enum.map(args, &substitute(&1, subst))
    ret = substitute(ret, subst)
    {:type, l, :fun, [{:type, lp, :product, args}, ret]}
  end

  def debound(spec), do: spec

  defp substitute({:var, _, name} = v, subst), do: Map.get(subst, name, v)

  defp substitute({:type, l, n, args}, s) when is_list(args),
    do: {:type, l, n, Enum.map(args, &substitute(&1, s))}

  defp substitute({:user_type, l, n, args}, s),
    do: {:user_type, l, n, Enum.map(args, &substitute(&1, s))}

  defp substitute({:remote_type, l, [m, n, args]}, s),
    do: {:remote_type, l, [m, n, Enum.map(args, &substitute(&1, s))]}

  defp substitute({:ann_type, l, [v, t]}, s), do: {:ann_type, l, [v, substitute(t, s)]}
  defp substitute(other, _s), do: other

  defp fetch_type(mod, name, arity) do
    with {:ok, types} <- Code.Typespec.fetch_types(mod),
         {_kind, {^name, body, params}} <-
           Enum.find(types, fn {kind, {n, _b, p}} ->
             kind in [:type, :typep, :opaque] and n == name and length(p) == arity
           end) do
      {:ok, {params, body}}
    else
      _ -> :error
    end
  end
end

# ---------------------------------------------------------------------------
# Membership of real Elixir terms in descrs (kind-token exact)
# ---------------------------------------------------------------------------

defmodule TermMember do
  import Module.Types.Descr

  @moduledoc false

  # Returns true/false, or throws {:unverifiable, why} for terms descr cannot
  # encode exactly (functions with arrow constraints, non-atom-keyed maps).
  def member?(v, t) do
    cond do
      t == :term -> true
      not is_map(t) -> throw({:unverifiable, "non-map descr"})
      is_map_key(t, :dynamic) -> member?(v, Map.fetch!(t, :dynamic))
      is_list(v) and v != [] -> list_member?(v, t)
      true -> subtype?(single(v), t)
    end
  end

  defp list_member?(v, t) do
    case t do
      %{list: bdd} ->
        {elems, terminator} = decompose(v)
        eval_bdd(bdd, elems, terminator)

      %{} ->
        false
    end
  end

  defp eval_bdd(:bdd_top, _e, _t), do: true
  defp eval_bdd(:bdd_bot, _e, _t), do: false
  defp eval_bdd({_h, elem, last}, e, t), do: literal(elem, last, e, t)

  defp eval_bdd({_h, {_lh, elem, last}, c, u, d}, e, t) do
    if literal(elem, last, e, t) do
      eval_bdd(c, e, t) or eval_bdd(u, e, t)
    else
      eval_bdd(u, e, t) or eval_bdd(d, e, t)
    end
  end

  defp eval_bdd(other, _e, _t), do: throw({:unverifiable, "unknown BDD node #{inspect(other)}"})

  defp literal(elem, last, elems, terminator) do
    Enum.all?(elems, &member?(&1, elem)) and member?(terminator, last)
  end

  defp decompose([h | t]) when is_list(t) and t != [] do
    {elems, terminator} = decompose(t)
    {[h | elems], terminator}
  end

  defp decompose([h | t]) when t == [], do: {[h], []}
  defp decompose([h | t]), do: {[h], t}

  defp single(v) when is_boolean(v), do: atom([v])
  defp single(v) when is_atom(v), do: atom([v])
  defp single(v) when is_integer(v), do: integer()
  defp single(v) when is_float(v), do: float()
  defp single(v) when is_binary(v), do: binary()
  defp single(v) when is_bitstring(v), do: opt_difference(bitstring(), binary())
  defp single(v) when is_pid(v), do: pid()
  defp single(v) when is_port(v), do: port()
  defp single(v) when is_reference(v), do: reference()
  defp single([]), do: empty_list()
  # A function VALUE has no singleton descr (fun(arity) is the arity top, so
  # membership via subtype? would be too strict and fabricate negative
  # verdicts). Treat any function-containing term as unverifiable.
  defp single(v) when is_function(v), do: throw({:unverifiable, "function value"})
  defp single(v) when is_tuple(v), do: tuple(Enum.map(Tuple.to_list(v), &single/1))

  defp single(v) when is_struct(v) do
    fields = v |> Map.from_struct() |> Enum.map(fn {k, w} -> {k, single(w)} end)
    closed_map([{:__struct__, atom([v.__struct__])} | fields])
  end

  defp single(v) when is_map(v) do
    if Enum.all?(Map.keys(v), &is_atom/1) do
      closed_map(Enum.map(v, fn {k, w} -> {k, single(w)} end))
    else
      throw({:unverifiable, "non-atom-keyed map"})
    end
  end

  defp single(v), do: throw({:unverifiable, "unencodable #{inspect(v)}"})
end

# ---------------------------------------------------------------------------
# Verdicts (slice-wise)
# ---------------------------------------------------------------------------

defmodule Verdict do
  import Module.Types.Descr

  @moduledoc false

  # spec_clauses: [{arg_descrs, exact_args?, ret_descr, exact_ret?}]
  # iclauses: [{arg_descrs, ret_descr}] as stored in the chunk (may be gradual)
  #
  # Returns {verdict, details} where details carry the per-slice data used by
  # reporting and the witness executor.

  def compare(spec_clauses, iclauses, arity) do
    idom =
      Enum.reduce(iclauses, List.duplicate(none(), arity), fn {args, _}, acc ->
        Enum.zip_with(Enum.map(args, &upper_bound/1), acc, &opt_union/2)
      end)

    slices =
      for {sargs, exact_args, sret, exact_ret} <- spec_clauses do
        effective = effective_return(sargs, iclauses)
        sret_u = upper_bound(sret)

        # Domains: inference legitimately narrows aspirational spec domains
        # (e.g. Enumerable.t() is term(); inference lists the shapes the body
        # handles) and the checker warning on out-of-inferred-domain calls is
        # usually a true positive. So domains never gate the return verdict;
        # they are only reported, and only a DISJOINT domain position (spec
        # and inference cannot both be right about any call) escalates.
        dom_s_sub_i =
          Enum.zip(sargs, idom)
          |> Enum.all?(fn {s, i} -> subtype?(upper_bound(s), i) end)

        dom_disjoint =
          Enum.zip(sargs, idom)
          |> Enum.any?(fn {s, i} ->
            su = upper_bound(s)
            not empty?(su) and not empty?(i) and disjoint?(su, i)
          end)

        slice_verdict =
          cond do
            dom_disjoint ->
              :contradiction

            not empty?(sret_u) and not empty?(effective) and disjoint?(sret_u, effective) ->
              :contradiction

            subtype?(sret_u, effective) and subtype?(effective, sret_u) ->
              :equivalent

            subtype?(effective, sret_u) and exact_ret ->
              :spec_wider_return

            subtype?(sret_u, effective) or subtype?(effective, sret_u) ->
              :inference_wider

            true ->
              :mixed
          end

        %{
          verdict: slice_verdict,
          spec_args: sargs,
          spec_ret: sret_u,
          effective_ret: effective,
          exact_args: exact_args,
          exact_ret: exact_ret,
          domain_subset: dom_s_sub_i
        }
      end

    {combine(Enum.map(slices, & &1.verdict)), slices}
  end

  # Effective inferred return for calls inside `sargs`: union of returns of
  # inferred clauses whose domains intersect it positionwise. Inferred clauses
  # are overlapping upper bounds, so this is the checker's actual prediction
  # for that slice.
  defp effective_return(sargs, iclauses) do
    iclauses
    |> Enum.filter(fn {iargs, _ret} ->
      Enum.zip(sargs, iargs)
      |> Enum.all?(fn {s, i} -> not disjoint?(upper_bound(s), upper_bound(i)) end)
    end)
    |> Enum.reduce(none(), fn {_args, ret}, acc -> opt_union(upper_bound(ret), acc) end)
  end

  @order [:contradiction, :mixed, :spec_wider_return, :inference_wider, :equivalent]

  defp combine(verdicts) do
    Enum.find(@order, :equivalent, &(&1 in verdicts))
  end
end

# ---------------------------------------------------------------------------
# Witnesses: pool sampling + doctest mining, sandboxed execution
# ---------------------------------------------------------------------------

defmodule Witness do
  @moduledoc false

  # Pure modules whose functions are safe to call with arbitrary in-domain
  # values (no side effects beyond CPU/memory; raises are fine).
  @pure_modules [
    Access,
    Base,
    Bitwise,
    Date.Range,
    Enum,
    Float,
    Function,
    Integer,
    Keyword,
    List,
    Map,
    MapSet,
    Path,
    Range,
    String,
    Tuple,
    URI,
    Version
  ]

  # Atom-creating or otherwise unsafe functions within pure modules.
  @deny [
    {String, :to_atom},
    {List, :to_atom},
    {Function, :capture}
  ]

  def executable?(module, fun) do
    extra =
      case System.get_env("COMPARE_SPECS_EXEC_ALSO") do
        nil -> []
        names -> names |> String.split(",") |> Enum.map(&Module.concat([&1]))
      end

    (module in @pure_modules or module in extra) and {module, fun} not in @deny
  end

  def pool do
    [
      :x,
      :ok,
      :error,
      :infinity,
      true,
      false,
      nil,
      0,
      1,
      -1,
      2,
      3,
      17,
      255,
      -42,
      0.0,
      1.5,
      -3.25,
      "",
      "a",
      "hello world",
      <<0, 255>>,
      <<3::3>>,
      [],
      [1, 2, 3],
      [:x, :y],
      ["a", "b"],
      [a: 1, b: 2],
      [{:k, "v"}],
      {},
      {1},
      {:ok, 1},
      {1, 2, 3},
      %{},
      %{a: 1},
      %{a: 1, b: "x"},
      %{"k" => 1},
      self(),
      make_ref(),
      MapSet.new([1, 2]),
      1..5,
      fn -> :stub0 end,
      fn x -> x end,
      fn _, _ -> :stub2 end
    ]
  end

  # Rejection-sample a value inside `descr` (throws treated as non-member).
  def sample_in(descr) do
    pool()
    |> Enum.shuffle()
    |> Enum.find(fn v ->
      try do
        TermMember.member?(v, descr)
      catch
        {:unverifiable, _} -> false
      end
    end)
  end

  def call(module, fun, args) do
    task =
      Task.async(fn ->
        try do
          {:ok, apply(module, fun, args)}
        rescue
          e -> {:raised, e.__struct__}
        catch
          kind, v -> {:raised, {kind, inspect(v, limit: 3)}}
        end
      end)

    case Task.yield(task, 1000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> :timeout
    end
  end

  # -- Doctest mining -------------------------------------------------------
  #
  # Extract (args, result) pairs for `fun/arity` from the module's doctests:
  # evaluate each iex> block step by step (binding accumulates within a
  # block); when a step is syntactically a direct `Module.fun(args...)` call,
  # record the evaluated arguments and the step's result.

  def doctest_witnesses(module, fun, arity, cap \\ 6) do
    case Code.fetch_docs(module) do
      {:docs_v1, _, _, _, _, _, docs} ->
        docs
        |> Enum.flat_map(fn
          {{:function, ^fun, ^arity}, _, _, %{"en" => text}, _} -> extract_blocks(text)
          _ -> []
        end)
        |> Enum.flat_map(&eval_block(&1, module, fun, arity))
        |> Enum.take(cap)

      _ ->
        []
    end
  end

  defp extract_blocks(text) do
    text
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> chunk_iex_blocks([], [])
  end

  defp chunk_iex_blocks([], current, blocks), do: finish_block(current, blocks) |> Enum.reverse()

  defp chunk_iex_blocks([line | rest], current, blocks) do
    cond do
      String.starts_with?(line, "iex>") or String.starts_with?(line, "iex(") ->
        expr = line |> String.replace(~r/^iex(\(\d+\))?>\s?/, "")
        chunk_iex_blocks(rest, [{:step, expr} | current], blocks)

      String.starts_with?(line, "...>") and current != [] ->
        cont = String.replace_prefix(line, "...>", "")

        case current do
          [{:step, prev} | tail] ->
            chunk_iex_blocks(rest, [{:step, prev <> "\n" <> cont} | tail], blocks)

          _ ->
            chunk_iex_blocks(rest, current, blocks)
        end

      line == "" ->
        chunk_iex_blocks(rest, [], finish_block(current, blocks))

      true ->
        # expected-result line: irrelevant, we take the evaluated value
        chunk_iex_blocks(rest, current, blocks)
    end
  end

  defp finish_block([], blocks), do: blocks
  defp finish_block(current, blocks), do: [Enum.reverse(current) | blocks]

  defp eval_block(steps, module, fun, arity) do
    {witnesses, _binding} =
      Enum.reduce_while(steps, {[], []}, fn {:step, expr}, {ws, binding} ->
        case safe_eval(expr, binding) do
          {:ok, value, binding2} ->
            ws =
              case direct_call_args(expr, module, fun, arity, binding) do
                {:ok, args} -> [{args, value, :doctest} | ws]
                :no -> ws
              end

            {:cont, {ws, binding2}}

          :error ->
            {:halt, {ws, binding}}
        end
      end)

    Enum.reverse(witnesses)
  end

  defp safe_eval(expr, binding) do
    task =
      Task.async(fn ->
        try do
          {value, binding2} = Code.eval_string(expr, binding)
          {:ok, value, binding2}
        rescue
          _ -> :error
        catch
          _, _ -> :error
        end
      end)

    case Task.yield(task, 1000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> :error
    end
  end

  defp direct_call_args(expr, module, fun, arity, binding) do
    with {:ok, ast} <- Code.string_to_quoted(expr),
         {{:., _, [{:__aliases__, _, parts}, ^fun]}, _, args} when length(args) == arity <- ast,
         true <- Module.concat(parts) == module,
         {:ok, values} <- eval_args(args, binding) do
      {:ok, values}
    else
      _ -> :no
    end
  end

  defp eval_args(args, binding) do
    values =
      Enum.map(args, fn arg ->
        case safe_eval(Macro.to_string(arg), binding) do
          {:ok, v, _} -> {:ok, v}
          :error -> :error
        end
      end)

    if Enum.all?(values, &match?({:ok, _}, &1)) do
      {:ok, Enum.map(values, fn {:ok, v} -> v end)}
    else
      :error
    end
  end

  # -- Verdict upgrading ----------------------------------------------------
  #
  # Runs witnesses for a residue entry and classifies the evidence.
  # Returns %{proven_spec_wrong: [..], proven_inference_wrong: [..],
  #           consistent: n, unverifiable: n}

  def evaluate(module, fun, arity, slices) do
    if executable?(module, fun) do
      random = random_witnesses(module, fun, slices)
      doctest = doctest_witnesses(module, fun, arity)

      Enum.reduce(random ++ doctest, empty_evidence(), fn {args, result, source}, ev ->
        classify(ev, module, fun, args, result, source, slices)
      end)
    else
      empty_evidence()
    end
  end

  defp empty_evidence do
    %{proven_spec_wrong: [], proven_inference_wrong: [], consistent: 0, unverifiable: 0}
  end

  defp random_witnesses(module, fun, slices, rounds \\ 12) do
    Enum.flat_map(slices, fn slice ->
      Enum.flat_map(1..rounds, fn _ ->
        args = Enum.map(slice.spec_args, &sample_in(upper_bound_of(&1)))

        with false <- Enum.any?(args, &is_nil/1),
             {:ok, result} <- call(module, fun, args) do
          [{args, result, :random}]
        else
          _ -> []
        end
      end)
    end)
    |> Enum.uniq_by(fn {args, _, _} -> args end)
  end

  defp upper_bound_of(descr), do: Module.Types.Descr.upper_bound(descr)

  defp classify(ev, _module, _fun, args, result, source, slices) do
    # Find the slices whose spec domain contains the args (positionwise).
    matching =
      Enum.filter(slices, fn slice ->
        Enum.zip(args, slice.spec_args)
        |> Enum.all?(fn {v, d} ->
          try do
            TermMember.member?(v, upper_bound_of(d))
          catch
            {:unverifiable, _} -> false
          end
        end)
      end)

    if matching == [] do
      %{ev | unverifiable: ev.unverifiable + 1}
    else
      in_spec_ret =
        Enum.any?(matching, fn s ->
          try do
            TermMember.member?(result, s.spec_ret)
          catch
            {:unverifiable, _} -> true
          end
        end)

      in_effective =
        Enum.any?(matching, fn s ->
          try do
            TermMember.member?(result, s.effective_ret)
          catch
            {:unverifiable, _} -> true
          end
        end)

      exact_ret? = Enum.all?(matching, & &1.exact_ret)

      w = %{args: args, result: result, source: source}

      cond do
        not in_spec_ret and exact_ret? ->
          %{ev | proven_spec_wrong: dedup_add(ev.proven_spec_wrong, w)}

        not in_effective ->
          %{ev | proven_inference_wrong: dedup_add(ev.proven_inference_wrong, w)}

        true ->
          %{ev | consistent: ev.consistent + 1}
      end
    end
  end

  defp dedup_add(list, _w) when length(list) >= 3, do: list
  defp dedup_add(list, w), do: list ++ [w]
end

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

defmodule Main do
  import Module.Types.Descr

  @moduledoc false

  def run(opts) do
    modules = enumerate_modules(opts)
    {:ok, cache} = Module.ParallelChecker.start_link()

    results =
      try do
        Enum.map(modules, &analyze_module(&1, cache, opts))
      after
        Module.ParallelChecker.stop(cache)
      end

    render(results, opts)
  end

  defp enumerate_modules(opts) do
    repo_root = Path.expand(Path.join(__DIR__, "../../.."))

    selected_apps =
      case opts[:apps] do
        nil -> opts[:apps_default]
        list -> Enum.map(list, &String.to_atom/1)
      end

    selected_modules =
      (opts[:modules] || [])
      |> Enum.map(fn name ->
        case name do
          "Elixir." <> _ -> String.to_atom(name)
          other -> Module.concat([other])
        end
      end)
      |> MapSet.new()

    modules =
      selected_apps
      |> Enum.flat_map(fn app ->
        app_file = Path.join([repo_root, "lib", Atom.to_string(app), "ebin", "#{app}.app"])

        case :file.consult(String.to_charlist(app_file)) do
          {:ok, [{:application, ^app, properties}]} -> Keyword.get(properties, :modules, [])
          _ -> raise "could not read #{app_file}"
        end
      end)
      |> Enum.uniq()
      |> Enum.filter(fn module ->
        match?(~c"Elixir." ++ _, Atom.to_charlist(module)) and
          (MapSet.size(selected_modules) == 0 or MapSet.member?(selected_modules, module))
      end)
      |> Enum.sort()

    # --module may also name modules outside the stdlib app lists (e.g. for
    # canary runs against synthetic modules on the code path).
    extra =
      selected_modules
      |> MapSet.difference(MapSet.new(modules))
      |> Enum.filter(&Code.ensure_loaded?/1)

    modules = Enum.sort(modules ++ extra)
    modules = if opts[:limit], do: Enum.take(modules, opts[:limit]), else: modules
    if modules == [], do: raise("no modules matched the given filters")
    modules
  end

  defp analyze_module(module, cache, opts) do
    exported = MapSet.new(module.__info__(:functions))

    specs =
      case Code.Typespec.fetch_specs(module) do
        {:ok, specs} ->
          Enum.filter(specs, fn {{n, a}, _} -> MapSet.member?(exported, {n, a}) end)

        :error ->
          []
      end

    env = %{module: module, stack: []}

    entries =
      specs
      |> Enum.sort()
      |> Enum.map(fn {{name, arity}, spec_clauses} ->
        analyze_function(module, name, arity, spec_clauses, cache, env, opts)
      end)

    %{module: module, entries: entries}
  end

  defp analyze_function(module, name, arity, spec_clauses, cache, env, opts) do
    base = %{module: module, function: name, arity: arity}

    case Module.ParallelChecker.fetch_export(cache, module, name, arity, true) do
      {:ok, _mode, _deprecated, {:infer, _dom, iclauses}} when iclauses != [] ->
        try do
          translated =
            Enum.map(spec_clauses, fn spec ->
              {:type, _, :fun, [{:type, _, :product, args}, ret]} = SpecToDescr.debound(spec)

              {dargs, exact_args} =
                Enum.map_reduce(args, true, fn t, ex ->
                  {d, ex2} = SpecToDescr.translate(t, env)
                  {d, ex and ex2}
                end)

              {dret, exact_ret} = SpecToDescr.translate(ret, env)
              {dargs, exact_args, dret, exact_ret}
            end)

          {verdict, slices} = Verdict.compare(translated, iclauses, arity)

          entry =
            base
            |> Map.put(:verdict, verdict)
            |> Map.put(:slices, slices)
            |> Map.put(:iclauses, iclauses)
            |> Map.put(:spec_strings, spec_strings(name, spec_clauses))

          if verdict in [:contradiction, :spec_wider_return, :mixed] and opts[:exec] do
            evidence = Witness.evaluate(module, name, arity, slices)
            entry = Map.put(entry, :evidence, evidence)

            cond do
              evidence.proven_spec_wrong != [] -> %{entry | verdict: :proven_spec_wrong}
              evidence.proven_inference_wrong != [] -> %{entry | verdict: :proven_inference_wrong}
              true -> entry
            end
          else
            entry
          end
        catch
          {:untranslatable, why} ->
            Map.merge(base, %{verdict: :untranslatable, why: inspect(why, limit: 5)})
        end

      _ ->
        Map.put(base, :verdict, :no_signature)
    end
  end

  defp spec_strings(name, clauses) do
    Enum.map(clauses, fn c ->
      name |> Code.Typespec.spec_to_quoted(c) |> Macro.to_string()
    end)
  end

  # -- Rendering ------------------------------------------------------------

  @auto [:equivalent, :inference_wider, :no_signature]
  @residue [:contradiction, :mixed, :spec_wider_return]
  @proven [:proven_spec_wrong, :proven_inference_wrong]

  defp render(results, opts) do
    all = Enum.flat_map(results, & &1.entries)
    stats = Enum.frequencies_by(all, & &1.verdict)
    residue = Enum.filter(all, &(&1.verdict in @residue))
    proven = Enum.filter(all, &(&1.verdict in @proven))

    exclusions = opts[:exclusions]
    excluded? = &(exclusions != nil and MapSet.member?(exclusions, mfa_string(&1)))
    new_residue = Enum.reject(residue, excluded?)
    new_proven = Enum.reject(proven, excluded?)

    stale =
      if exclusions do
        current = MapSet.new(residue ++ proven, &mfa_string/1)
        exclusions |> MapSet.difference(current) |> Enum.sort()
      else
        []
      end

    gate = %{
      exclusions: exclusions,
      new_residue: new_residue,
      new_proven: new_proven,
      stale: stale
    }

    out =
      case opts[:format] do
        "report" -> render_report(stats, proven, residue, all, gate)
        "json" -> JSON.encode_to_iodata!(render_json(stats, proven, residue))
        "prompt" -> render_prompt(stats, proven, residue)
      end

    case opts[:output] do
      nil -> IO.binwrite(out)
      path -> File.write!(path, out)
    end

    # Proven witness findings AND lattice contradictions are mechanical
    # certainties (the over-approximation invariant in SpecToDescr makes
    # disjointness conclusions transfer to the real spec type), so both fail
    # the run for CI purposes. With --exclusions, any residue entry not
    # explicitly acknowledged in the file also fails the run; stale
    # exclusions only warn.
    contradictions = Enum.filter(new_residue, &(&1.verdict == :contradiction))
    strict_fail = exclusions != nil and new_residue != []
    if new_proven != [] or contradictions != [] or strict_fail, do: System.halt(1)
  end

  defp mfa_string(e), do: "#{inspect(e.module)}.#{e.function}/#{e.arity}"

  defp entry_json(e) do
    %{
      mfa: "#{inspect(e.module)}.#{e.function}/#{e.arity}",
      verdict: e.verdict,
      spec: e[:spec_strings] || [],
      inferred:
        Enum.map(e[:iclauses] || [], fn {args, ret} ->
          %{
            args: Enum.map(args, &to_quoted_string/1),
            return: to_quoted_string(ret)
          }
        end),
      slices:
        Enum.map(e[:slices] || [], fn s ->
          %{
            verdict: s.verdict,
            spec_return: to_quoted_string(s.spec_ret),
            inferred_effective_return: to_quoted_string(s.effective_ret),
            translation_exact: %{args: s.exact_args, return: s.exact_ret}
          }
        end),
      evidence: render_evidence(e[:evidence])
    }
  end

  defp render_evidence(nil), do: nil

  defp render_evidence(ev) do
    %{
      consistent_witnesses: ev.consistent,
      unverifiable: ev.unverifiable,
      proven_spec_wrong: Enum.map(ev.proven_spec_wrong, &witness_json/1),
      proven_inference_wrong: Enum.map(ev.proven_inference_wrong, &witness_json/1)
    }
  end

  defp witness_json(w) do
    %{
      source: w.source,
      args: Enum.map(w.args, &inspect(&1, limit: 10)),
      result: inspect(w.result, limit: 10)
    }
  end

  defp render_json(stats, proven, residue) do
    %{
      elixir: System.version(),
      format_version: 2,
      stats: stats,
      proven: Enum.map(proven, &entry_json/1),
      residue: Enum.map(residue, &entry_json/1)
    }
  end

  defp render_report(stats, proven, residue, all, gate) do
    total = length(all)
    auto = Enum.count(all, &(&1.verdict in @auto))

    gate_section =
      case gate.exclusions do
        nil ->
          ""

        exclusions ->
          new_lines =
            for e <- gate.new_proven ++ gate.new_residue do
              "  NEW (not in exclusions, FAILS the gate): #{mfa_string(e)} (#{e.verdict})\n"
            end

          stale_lines =
            for mfa <- gate.stale do
              "  stale exclusion (entry no longer matches, remove it): #{mfa}\n"
            end

          summary =
            "gate: #{MapSet.size(exclusions)} exclusions -- " <>
              "#{length(gate.new_proven) + length(gate.new_residue)} new, " <>
              "#{length(gate.stale)} stale\n"

          Enum.join([summary | new_lines ++ stale_lines])
      end

    # With an exclusions file, acknowledged entries are only counted (header
    # stats keep the full numbers) -- full details are printed for NEW
    # (gate-failing) entries alone, keeping CI output focused on what changed.
    {detail_proven, detail_residue} =
      if gate.exclusions do
        {gate.new_proven, gate.new_residue}
      else
        {proven, residue}
      end

    header = """
    compare_specs_and_signatures: #{total} spec'd functions
    auto-triaged: #{auto} (#{percent(auto, total)}) -- #{inspect(Map.take(stats, @auto))}
    residue:      #{length(residue)} -- #{inspect(Map.take(stats, @residue))}
    proven:       #{length(proven)}
    untranslatable: #{Map.get(stats, :untranslatable, 0)}
    #{gate_section}
    """

    proven_section =
      for e <- detail_proven do
        ev = e.evidence

        witnesses =
          (ev.proven_spec_wrong ++ ev.proven_inference_wrong)
          |> Enum.map_join("\n", fn w ->
            "      #{inspect(e.module)}.#{e.function}(#{Enum.map_join(w.args, ", ", &inspect(&1, limit: 8))}) => #{inspect(w.result, limit: 8)}   [#{w.source}]"
          end)

        """
        !! #{e.verdict}: #{inspect(e.module)}.#{e.function}/#{e.arity}
            spec:     #{Enum.join(e.spec_strings, " ||| ")}
            inferred: #{inferred_string(e)}
        #{witnesses}
        """
      end

    residue_section =
      for e <- Enum.sort_by(detail_residue, &verdict_rank/1) do
        slice_notes =
          e.slices
          |> Enum.filter(&(&1.verdict in @residue))
          |> Enum.map_join("\n", fn s ->
            approx = if s.exact_ret, do: "", else: "  [ret approx]"

            "      #{s.verdict}#{approx}: spec ret #{to_quoted_string(s.spec_ret)} vs inferred #{to_quoted_string(s.effective_ret)}"
          end)

        evidence_note =
          case e[:evidence] do
            %{consistent: n} when n > 0 -> "      witnesses: #{n} consistent executions\n"
            _ -> ""
          end

        """
        #{e.verdict}: #{inspect(e.module)}.#{e.function}/#{e.arity}
            spec:     #{Enum.join(e.spec_strings, " ||| ")}
            inferred: #{inferred_string(e)}
        #{slice_notes}
        #{evidence_note}
        """
      end

    [header, Enum.join(proven_section, "\n"), Enum.join(residue_section, "\n")]
  end

  defp inferred_string(e) do
    Enum.map_join(e.iclauses, " ||| ", fn {args, ret} ->
      "(#{Enum.map_join(args, ", ", &to_quoted_string/1)}) -> #{to_quoted_string(ret)}"
    end)
  end

  defp verdict_rank(%{verdict: :contradiction}), do: 0
  defp verdict_rank(%{verdict: :spec_wider_return}), do: 1
  defp verdict_rank(_), do: 2

  defp percent(_n, 0), do: "n/a"
  defp percent(n, total), do: "#{Float.round(n * 100 / total, 1)}%"

  defp render_prompt(stats, proven, residue) do
    json = JSON.encode_to_iodata!(render_json(stats, proven, residue))

    [
      """
      You are reviewing the RESIDUE of a mechanical comparison between Elixir
      @spec declarations and compiler-inferred type signatures. Everything
      mechanically decidable has already been decided:

      - functions where spec and inference agree, or where inference is merely
        wider (expected conservatism), are NOT included;
      - "verdict" was computed with semantic subtyping in the checker's own
        type lattice, slice-wise per spec clause;
      - "translation_exact" flags where the spec had to be approximated
        (e.g. non_neg_integer() -> integer()); do not draw conclusions that
        depend on precision the translation lost;
      - "evidence" contains REAL executed calls: entries under
        proven_spec_wrong/proven_inference_wrong are mechanical certainties,
        already confirmed -- explain them, do not re-litigate them.

      For each entry, judge:
      1. contradiction: which side is wrong, and what should change?
      2. spec_wider_return: is tightening the spec desirable documentation-wise
         (e.g. a function that never returns [] declared as returning keyword())
         or is the wider spec intentional API contract?
      3. mixed: characterize the difference and whether it is actionable.

      Be concise; order by severity; reference module.function/arity.

      DATA:
      """,
      json
    ]
  end
end

Main.run(%{
  apps_default: apps_default,
  apps: opts[:app],
  modules: Keyword.get_values(opts, :module),
  format: format,
  output: opts[:output],
  limit: opts[:limit],
  exec: !opts[:no_exec],
  exclusions: exclusions
})
