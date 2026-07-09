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
#                      EXACT translation -> dropped: specs are public
#                      contracts, deliberately wider than the implementation
#                      (still reported in stats/JSON as tightening hints)
#   :contradiction     spec and inferred returns are disjoint, or the checker
#                      rejects every spec-conforming call -> someone is
#                      definitely wrong
#   :mixed             none of the above -> needs judgment
#
# Return comparison is SLICE-WISE and uses the checker's own application
# rule (mirroring Module.Types.Apply.apply_infer/2): for each spec clause,
# the inferred signature is applied at the spec's argument types -- clauses
# with positionwise non-disjoint domains contribute their returns; no
# matching clause means the checker would warn on every call in that slice.
# Each function is additionally probed with all arguments set to
# Descr.dynamic() (the fully-unknown gradual caller) and the resulting
# return's relation to the spec return is reported. Inferred applications
# always produce dynamic()-wrapped returns regardless of stack.mode; mode
# only affects strong signatures, which do not occur in ExCk chunks.
#
# Only the residue (typically a few % of functions) is emitted for human
# review, with verdicts and precision flags attached -- the reviewer no
# longer judges string equivalence. The analysis is fully static: no stdlib
# function is executed.
#
# Spec-domain body check (informational, does not gate)
# -----------------------------------------------------
# Additionally, the compiler's type checker is re-run over each module's
# definitions (Module.Types.warnings/7) with every spec'd function's
# argument domain taken from its TRANSLATED SPEC instead of the default
# list of dynamics. This reports (a) the warnings the checker emits when
# arguments are assumed spec-typed -- e.g. clauses or code paths that
# cannot match spec-conforming inputs -- and (b) the return type inferred
# under that assumption, compared against the declared spec return.
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
#   --no-check-bodies Skip the spec-domain body check (see below).
#   --bodies-domain MODE
#                     dynamic (default): re-check bodies with arguments set
#                     to dynamic(spec_type) under the checker's :dynamic
#                     mode. static: raw spec types under :static mode --
#                     stricter, more (and more speculative) warnings.
#   --exclusions PATH Acknowledged-findings file: one Module.function/arity
#                     per line, anything after "#" is a comment. Enables
#                     STRICT gating: every residue AND untranslatable entry
#                     must be listed or the run fails (an untranslatable spec
#                     is lost coverage, not a pass). Entries that no longer
#                     match anything are reported as stale (warning, not
#                     fatal).
#   --help            This help.
#
# Exit status: 1 if any lattice contradiction exists (spec and inference
# cannot both be right); with --exclusions additionally 1 if any NEW
# (unlisted) residue or untranslatable entry appears; 0 otherwise. The
# analysis is deterministic. CI entry point: `make check_specs`.

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
      check_bodies: :boolean,
      bodies_domain: :string,
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

bodies_domain = opts[:bodies_domain] || "dynamic"

unless bodies_domain in ["dynamic", "static"] do
  raise "expected --bodies-domain to be one of: dynamic, static"
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
  # under-approximate. Verdict soundness depends on it: a disjointness
  # (:contradiction) conclusion transfers from an over-approximation to the
  # real spec type, but an under-approximation fabricates it. If a construct
  # cannot be soundly over-approximated, throw :untranslatable instead.

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

  # Erlang record type #name{...}: a record value is a tuple whose first
  # element is the record name. Field types are not resolvable from the beam
  # (record definitions are compile-time), so over-approximate the remaining
  # elements (sound per the INVARIANT above).
  defp builtin(:record, [{:atom, _, name} | _field_overrides], _e, _d),
    do: {open_tuple([atom([name])]), false}

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
          # Argument ASTs are written in the CALLER's module context, but the
          # body is translated with env.module switched to the callee. A bare
          # user_type inside an argument (e.g. Keyword.t(ansidata) in IO.ANSI)
          # would be wrongly resolved in the callee's namespace -- or silently
          # capture a same-named callee type. Qualify arguments as remote
          # types against the caller module before substituting.
          args = Enum.map(args, &qualify(&1, env.module))

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

  defp qualify({:user_type, l, n, args}, module) do
    {:remote_type, l, [{:atom, l, module}, {:atom, l, n}, Enum.map(args, &qualify(&1, module))]}
  end

  defp qualify({:type, l, n, args}, module) when is_list(args),
    do: {:type, l, n, Enum.map(args, &qualify(&1, module))}

  defp qualify({:remote_type, l, [m, n, args]}, module),
    do: {:remote_type, l, [m, n, Enum.map(args, &qualify(&1, module))]}

  defp qualify({:ann_type, l, [v, t]}, module), do: {:ann_type, l, [v, qualify(t, module)]}
  defp qualify(other, _module), do: other

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
             kind in [:type, :typep, :opaque, :nominal] and n == name and length(p) == arity
           end) do
      {:ok, {params, body}}
    else
      _ -> :error
    end
  end
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
  # reporting.

  # Mirrors Module.Types.Apply @max_clauses.
  @max_clauses 16

  def compare(spec_clauses, iclauses, arity) do
    idom =
      Enum.reduce(iclauses, List.duplicate(none(), arity), fn {args, _}, acc ->
        Enum.zip_with(Enum.map(args, &upper_bound/1), acc, &opt_union/2)
      end)

    slices =
      for {sargs, exact_args, sret, exact_ret} <- spec_clauses do
        applied = checker_apply(iclauses, sargs)
        sret_u = upper_bound(sret)

        effective =
          case applied do
            {:ok, ret} -> upper_bound(ret)
            :badapply -> none()
          end

        # Domains: inference legitimately narrows aspirational spec domains
        # (e.g. Enumerable.t() is term(); inference lists the shapes the body
        # handles) and the checker warning on out-of-inferred-domain calls is
        # usually a true positive. So domains never gate the return verdict;
        # they are only reported, and only a DISJOINT domain position (spec
        # and inference cannot both be right about any call) escalates -- as
        # does :badapply on a non-empty spec domain, where the checker's own
        # application rule rejects EVERY spec-conforming call.
        dom_s_sub_i =
          Enum.zip(sargs, idom)
          |> Enum.all?(fn {s, i} -> subtype?(upper_bound(s), i) end)

        dom_disjoint =
          Enum.zip(sargs, idom)
          |> Enum.any?(fn {s, i} ->
            su = upper_bound(s)
            not empty?(su) and not empty?(i) and disjoint?(su, i)
          end)

        badapply? =
          applied == :badapply and Enum.all?(sargs, &(not empty?(upper_bound(&1))))

        slice_verdict =
          cond do
            dom_disjoint or badapply? ->
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

    dynamic_probe = dynamic_args_probe(iclauses, spec_clauses, arity)
    {combine(Enum.map(slices, & &1.verdict)), slices, dynamic_probe}
  end

  # The checker's own application of an inferred signature, mirroring
  # Module.Types.Apply.apply_infer/2: clauses whose domains are positionwise
  # NON-DISJOINT from the argument types contribute their returns; no
  # matching clause means the checker warns on every such call (:badapply);
  # more than @max_clauses matching collapse to dynamic(). Inferred
  # applications always wrap the result in dynamic() regardless of
  # stack.mode -- the mode only affects strong signatures (via
  # Apply.return/3), which never appear in ExCk chunks, so running "in
  # static mode" degenerates to the same computation here.
  def checker_apply(iclauses, args_types) do
    matching =
      Enum.filter(iclauses, fn {iargs, _ret} -> zip_not_disjoint?(args_types, iargs) end)

    cond do
      matching == [] ->
        :badapply

      length(matching) > @max_clauses ->
        {:ok, dynamic()}

      true ->
        {:ok, matching |> Enum.map(&elem(&1, 1)) |> Enum.reduce(&opt_union/2) |> dynamic()}
    end
  end

  defp zip_not_disjoint?([a | as], [e | es]),
    do: not disjoint?(a, e) and zip_not_disjoint?(as, es)

  defp zip_not_disjoint?([], []), do: true

  # The "wrap all arguments in dynamic()" probe: what the checker infers for
  # a call site where nothing is known about the arguments (the common
  # gradual caller). dynamic() is non-disjoint from every non-empty domain,
  # so every inferred clause applies. Reported per function, not gated: the
  # inferred domain may legitimately cover inputs outside the spec domain
  # (defensive clauses), so a wider dynamic-args return is not a spec
  # violation by itself.
  defp dynamic_args_probe(iclauses, spec_clauses, arity) do
    spec_ret_union =
      Enum.reduce(spec_clauses, none(), fn {_, _, sret, _}, acc ->
        opt_union(upper_bound(sret), acc)
      end)

    case checker_apply(iclauses, List.duplicate(dynamic(), arity)) do
      :badapply ->
        %{return: none(), relation: :badapply}

      {:ok, ret} ->
        ret_u = upper_bound(ret)

        relation =
          cond do
            subtype?(ret_u, spec_ret_union) and subtype?(spec_ret_union, ret_u) -> :equal
            subtype?(ret_u, spec_ret_union) -> :inside_spec
            subtype?(spec_ret_union, ret_u) -> :wider_than_spec
            disjoint?(ret_u, spec_ret_union) -> :disjoint
            true -> :incomparable
          end

        %{return: ret_u, relation: relation}
    end
  end

  @order [:contradiction, :mixed, :spec_wider_return, :inference_wider, :equivalent]

  defp combine(verdicts) do
    Enum.find(@order, :equivalent, &(&1 in verdicts))
  end
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
        analyze_function(module, name, arity, spec_clauses, cache, env)
      end)

    {entries, body_warnings} = check_bodies(module, entries, cache, opts)
    %{module: module, entries: entries, body_warnings: body_warnings}
  end

  # -- Spec-domain body check ------------------------------------------------
  #
  # Re-runs the compiler's type checker over the module's definitions with
  # each spec'd function's argument domain taken from its translated spec
  # instead of the default list of dynamics (Module.Types.warnings/7).
  # Informational: results are reported but never gate.

  defp check_bodies(module, entries, cache, opts) do
    with true <- opts[:check_bodies],
         {:ok, defs, attrs, file} <- module_debug_info(module) do
      mode = opts[:bodies_domain]

      domains =
        for %{slices: slices} = e <- entries, slices != [], into: %{} do
          args_union =
            slices
            |> Enum.map(& &1.spec_args)
            |> Enum.zip_with(fn types -> Enum.reduce(types, &opt_union/2) end)

          args =
            case mode do
              :dynamic -> Enum.map(args_union, &dynamic/1)
              :static -> args_union
            end

          {{e.function, e.arity}, {mode, args}}
        end

      {warnings, sigs} =
        Module.Types.warnings(module, file, attrs, defs, :all, cache, fn fun_arity ->
          case domains do
            %{^fun_arity => domain} -> domain
            %{} -> :default
          end
        end)

      entries =
        Enum.map(entries, fn e ->
          fun_arity = {e[:function], e[:arity]}

          with true <- is_map_key(domains, fun_arity),
               {_kind, {:infer, _dom, [_ | _] = clauses}, _mapping} <- Map.get(sigs, fun_arity) do
            ret = clauses |> Enum.map(fn {_args, ret} -> ret end) |> Enum.reduce(&opt_union/2)
            Map.put(e, :body_check, %{return: ret, relation: body_relation(ret, e)})
          else
            _ -> e
          end
        end)

      {entries, Enum.map(warnings, &format_body_warning/1)}
    else
      _ -> {entries, []}
    end
  end

  defp body_relation(ret, e) do
    spec_ret = Enum.reduce(e.slices, none(), fn s, acc -> opt_union(s.spec_ret, acc) end)
    ret_u = upper_bound(ret)

    cond do
      subtype?(ret_u, spec_ret) and subtype?(spec_ret, ret_u) -> :equal
      subtype?(ret_u, spec_ret) -> :inside_spec
      subtype?(spec_ret, ret_u) -> :wider_than_spec
      not empty?(ret_u) and not empty?(spec_ret) and disjoint?(ret_u, spec_ret) -> :disjoint
      true -> :incomparable
    end
  end

  defp module_debug_info(module) do
    with [_ | _] = path <- :code.which(module),
         {:ok, binary} <- File.read(path),
         {:ok, {_, [debug_info: {:debug_info_v1, backend, data}]}} <-
           :beam_lib.chunks(binary, [:debug_info]),
         {:ok, %{definitions: defs, attributes: attrs, file: file}} <-
           backend.debug_info(:elixir_v1, module, data, []) do
      {:ok, defs, attrs, file}
    else
      _ -> :error
    end
  end

  # Warning entries are {module, warning, {file, meta, {mod, fun, arity}}}
  # as stored by Module.Types.Helpers.
  defp format_body_warning({warning_module, warning, location}) do
    message =
      try do
        %{message: message} = warning_module.format_diagnostic(warning)
        message |> IO.iodata_to_binary() |> String.split("\n") |> hd()
      rescue
        _ -> inspect(warning, limit: 5)
      end

    {mfa, position} =
      case location do
        {file, meta, {mod, fun, arity}} ->
          line = if is_list(meta), do: Keyword.get(meta, :line), else: nil

          {"#{inspect(mod)}.#{fun}/#{arity}",
           "#{Path.relative_to_cwd(to_string(file))}:#{line || "?"}"}

        _ ->
          {"?", "?"}
      end

    %{mfa: mfa, message: message, location: position}
  end

  defp analyze_function(module, name, arity, spec_clauses, cache, env) do
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

          {verdict, slices, dynamic_probe} = Verdict.compare(translated, iclauses, arity)

          base
          |> Map.put(:verdict, verdict)
          |> Map.put(:slices, slices)
          |> Map.put(:dynamic_probe, dynamic_probe)
          |> Map.put(:iclauses, iclauses)
          |> Map.put(:spec_strings, spec_strings(name, spec_clauses))
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

  # :spec_wider_return (inference strictly inside the spec return) is
  # auto-triaged: specs are public contracts and are often deliberately
  # wider than the implementation (e.g. atom() instead of specific atoms)
  # to keep room for evolution. It stays a distinct verdict in the stats
  # and JSON so tightening candidates remain discoverable.
  @auto [:equivalent, :inference_wider, :spec_wider_return, :no_signature]
  @residue [:contradiction, :mixed]

  defp render(results, opts) do
    all = Enum.flat_map(results, & &1.entries)
    stats = Enum.frequencies_by(all, & &1.verdict)
    residue = Enum.filter(all, &(&1.verdict in @residue))
    untranslatable = Enum.filter(all, &(&1.verdict == :untranslatable))

    exclusions = opts[:exclusions]
    excluded? = &(exclusions != nil and MapSet.member?(exclusions, mfa_string(&1)))
    new_residue = Enum.reject(residue, excluded?)
    new_untranslatable = Enum.reject(untranslatable, excluded?)

    stale =
      if exclusions do
        current = MapSet.new(residue ++ untranslatable, &mfa_string/1)
        exclusions |> MapSet.difference(current) |> Enum.sort()
      else
        []
      end

    gate = %{
      exclusions: exclusions,
      new_residue: new_residue,
      new_untranslatable: new_untranslatable,
      stale: stale
    }

    hints = Enum.filter(all, &(&1.verdict == :spec_wider_return))

    body_warnings =
      results
      |> Enum.flat_map(&(&1[:body_warnings] || []))
      |> Enum.sort_by(&{&1.mfa, &1.location})

    out =
      case opts[:format] do
        "report" ->
          render_report(stats, residue, untranslatable, all, gate, body_warnings)

        "json" ->
          JSON.encode_to_iodata!(
            render_json(stats, residue, untranslatable, hints, body_warnings)
          )

        "prompt" ->
          render_prompt(stats, residue, untranslatable, body_warnings)
      end

    case opts[:output] do
      nil -> IO.binwrite(out)
      path -> File.write!(path, out)
    end

    # Lattice contradictions are mechanical certainties (the
    # over-approximation invariant in SpecToDescr makes disjointness
    # conclusions transfer to the real spec type), so they fail the run for
    # CI purposes. With --exclusions, any residue OR untranslatable entry not
    # explicitly acknowledged in the file also fails the run -- a spec the
    # translator cannot handle is lost coverage, not a pass; stale exclusions
    # only warn.
    contradictions = Enum.filter(new_residue, &(&1.verdict == :contradiction))
    strict_fail = exclusions != nil and (new_residue != [] or new_untranslatable != [])
    if contradictions != [] or strict_fail, do: System.halt(1)
  end

  defp mfa_string(e), do: "#{inspect(e.module)}.#{e.function}/#{e.arity}"

  defp entry_json(e) do
    %{
      mfa: mfa_string(e),
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
      dynamic_args:
        case e[:dynamic_probe] do
          nil -> nil
          probe -> %{return: to_quoted_string(probe.return), relation: probe.relation}
        end,
      body_check:
        case e[:body_check] do
          nil -> nil
          bc -> %{return: to_quoted_string(bc.return), relation: bc.relation}
        end
    }
  end

  defp untranslatable_json(e) do
    %{mfa: mfa_string(e), why: e[:why]}
  end

  # tightening_hints carries the auto-triaged :spec_wider_return entries:
  # inference proved the return strictly narrower than an exactly-translated
  # spec. Not gated (specs are deliberately wide contracts), but listed so
  # documentation-tightening candidates remain identifiable.
  defp render_json(stats, residue, untranslatable, hints, body_warnings) do
    %{
      elixir: System.version(),
      format_version: 3,
      stats: stats,
      residue: Enum.map(residue, &entry_json/1),
      untranslatable: Enum.map(untranslatable, &untranslatable_json/1),
      tightening_hints: Enum.map(hints, &entry_json/1),
      body_warnings: body_warnings
    }
  end

  defp render_report(stats, residue, untranslatable, all, gate, body_warnings) do
    total = length(all)
    auto = Enum.count(all, &(&1.verdict in @auto))

    gate_section =
      case gate.exclusions do
        nil ->
          ""

        exclusions ->
          new_lines =
            for e <- gate.new_residue ++ gate.new_untranslatable do
              "  NEW (not in exclusions, FAILS the gate): #{mfa_string(e)} (#{e.verdict})\n"
            end

          stale_lines =
            for mfa <- gate.stale do
              "  stale exclusion (entry no longer matches, remove it): #{mfa}\n"
            end

          summary =
            "gate: #{MapSet.size(exclusions)} exclusions -- " <>
              "#{length(gate.new_residue) + length(gate.new_untranslatable)} new, " <>
              "#{length(gate.stale)} stale\n"

          Enum.join([summary | new_lines ++ stale_lines])
      end

    # With an exclusions file, acknowledged entries are only counted (header
    # stats keep the full numbers) -- full details are printed for NEW
    # (gate-failing) entries alone, keeping CI output focused on what changed.
    {detail_residue, detail_untranslatable} =
      if gate.exclusions do
        {gate.new_residue, gate.new_untranslatable}
      else
        {residue, untranslatable}
      end

    header = """
    compare_specs_and_signatures: #{total} spec'd functions
    auto-triaged: #{auto} (#{percent(auto, total)}) -- #{inspect(Map.take(stats, @auto))}
    residue:      #{length(residue)} -- #{inspect(Map.take(stats, @residue))}
    untranslatable: #{Map.get(stats, :untranslatable, 0)}
    #{gate_section}
    """

    untranslatable_section =
      for e <- detail_untranslatable do
        "untranslatable: #{mfa_string(e)}\n    why: #{e[:why]}\n"
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

        probe_note =
          case e[:dynamic_probe] do
            nil ->
              ""

            probe ->
              "      dynamic args: #{to_quoted_string(probe.return)} (#{probe.relation})\n"
          end

        body_note =
          case e[:body_check] do
            nil ->
              ""

            bc ->
              "      body under spec domain: #{to_quoted_string(bc.return)} (#{bc.relation})\n"
          end

        """
        #{e.verdict}: #{mfa_string(e)}
            spec:     #{Enum.join(e.spec_strings, " ||| ")}
            inferred: #{inferred_string(e)}
        #{slice_notes}
        #{probe_note}#{body_note}
        """
      end

    body_section =
      case body_warnings do
        [] ->
          []

        warnings ->
          lines =
            Enum.map_join(warnings, "\n", fn w ->
              "  #{w.mfa} (#{w.location}): #{w.message}"
            end)

          [
            """

            SPEC-DOMAIN BODY CHECK (informational, not gated): \
            #{length(warnings)} warning(s) when checking bodies against spec-typed arguments
            #{lines}
            """
          ]
      end

    [header, Enum.join(residue_section, "\n"), Enum.join(untranslatable_section, "\n")] ++
      body_section
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

  defp render_prompt(stats, residue, untranslatable, body_warnings) do
    json = JSON.encode_to_iodata!(render_json(stats, residue, untranslatable, [], body_warnings))

    [
      """
      You are reviewing the RESIDUE of a mechanical comparison between Elixir
      @spec declarations and compiler-inferred type signatures. Everything
      mechanically decidable has already been decided:

      - functions where spec and inference agree, where inference is merely
        wider (expected conservatism), or where inference is strictly inside
        the spec (specs are deliberately wide public contracts) are NOT
        included;
      - "verdict" was computed by applying the inferred signature with the
        checker's own application rule, slice-wise per spec clause, using
        semantic subtyping in the checker's type lattice;
      - "translation_exact" flags where the spec had to be approximated
        (e.g. non_neg_integer() -> integer()); do not draw conclusions that
        depend on precision the translation lost;
      - "dynamic_args" is the checker's return prediction for a call with
        fully-unknown arguments and its relation to the spec return.

      For each entry, judge:
      1. contradiction: which side is wrong, and what should change?
      2. mixed: characterize the difference and whether it is actionable.
      3. untranslatable: which typespec construct is unsupported and whether
         a sound over-approximation could be added to the translator.

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
  check_bodies: opts[:check_bodies] != false,
  bodies_domain: String.to_atom(bodies_domain),
  exclusions: exclusions
})
