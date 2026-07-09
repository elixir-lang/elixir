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
# Only the residue (typically a few % of functions) is emitted for human
# review, with verdicts and precision flags attached -- the reviewer no
# longer judges string equivalence. The analysis is fully static: no stdlib
# function is executed.
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
             kind in [:type, :typep, :opaque] and n == name and length(p) == arity
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
        Enum.map(modules, &analyze_module(&1, cache))
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

  defp analyze_module(module, cache) do
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

    %{module: module, entries: entries}
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

          {verdict, slices} = Verdict.compare(translated, iclauses, arity)

          base
          |> Map.put(:verdict, verdict)
          |> Map.put(:slices, slices)
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

  @auto [:equivalent, :inference_wider, :no_signature]
  @residue [:contradiction, :mixed, :spec_wider_return]

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

    out =
      case opts[:format] do
        "report" -> render_report(stats, residue, untranslatable, all, gate)
        "json" -> JSON.encode_to_iodata!(render_json(stats, residue, untranslatable))
        "prompt" -> render_prompt(stats, residue)
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
        end)
    }
  end

  defp untranslatable_json(e) do
    %{mfa: mfa_string(e), why: e[:why]}
  end

  defp render_json(stats, residue, untranslatable) do
    %{
      elixir: System.version(),
      format_version: 3,
      stats: stats,
      residue: Enum.map(residue, &entry_json/1),
      untranslatable: Enum.map(untranslatable, &untranslatable_json/1)
    }
  end

  defp render_report(stats, residue, untranslatable, all, gate) do
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

        """
        #{e.verdict}: #{mfa_string(e)}
            spec:     #{Enum.join(e.spec_strings, " ||| ")}
            inferred: #{inferred_string(e)}
        #{slice_notes}
        """
      end

    [header, Enum.join(residue_section, "\n"), Enum.join(untranslatable_section, "\n")]
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

  defp render_prompt(stats, residue) do
    json = JSON.encode_to_iodata!(render_json(stats, residue, []))

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
        depend on precision the translation lost.

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
  exclusions: exclusions
})
