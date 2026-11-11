# Elixir side of the harness. Runs inside the target build's VM.
#
#   elixir worker.exs                 line protocol: one JSON case per stdin line, one JSON verdict per stdout line
#   elixir worker.exs --repro case.json SIG
#                                     last line REPRO yes|no|undecided: the case fails with signature SIG or not
#
# A case is {"src": module source defining CaseMod, "calls": [{"f": name, "args": "[literal, ...]"}]}.
# Oracles:
#   crash__exception__<Mod.fun>  an exception with a Module.Types frame while compiling the module
#   hang__compile__typecheck     compiling takes longer than the time limit
#   sound__domain__<kind>        a call returned, but no clause of the inferred signature (ExCk chunk) accepts the args
#   sound__ret__<kind>           a call returned a value outside the upper bound of the inferred return type
#   warn__clause__<kind>         a warning says a def/case/fn/else clause never runs, but its probe ran
#   warn__expr__<area>           a warning says an expression always fails (incompatible types given to ...,
#                                expected a map ..., ...), but it evaluated without raising
#   head__<domain|ret|clause>__<area>  the case's clause heads lifted into CaseHead (pattern + guard -> bound vars,
#                                plus a catch-all) and applied to many values: oracle 1/3 on them
#   gradual__<wrap|noinfer>__<area>  warn__clause/warn__expr on a variant that runs the same: some expressions
#                                hidden behind CaseDyn.id/1 (dynamic()), or compiled with infer_signatures off
#   expr__<ret|warn|clause>__<op>  one expression (stdlib call, tuple/map/list op, case/fn/cond) typed with
#                                chosen variable types (exact, widened, unions, differences, dynamic) and
#                                evaluated with member values: result outside its type, a certain-failure
#                                warning, or a never-matches claim on the clause that ran (H.Expr)
# Soundness contract: gradual-set-theoretic-types.md:10, "the inferred and assigned by the type system align
# with the behaviour of the program".

# Structs the generated cases use (gen.rs): defined here so case modules and repro inputs can name them.
defmodule CaseS do
  defstruct a: nil, b: 0
end

defmodule CaseT do
  defstruct a: :x
end

# Oracle 7: an identity the checker knows nothing about (not in an application whose signatures are
# inferred), so `CaseDyn.id(e)` has type dynamic() and the same value as `e`.
defmodule CaseDyn do
  def id(x), do: x
end

defmodule H.Mem do
  import Module.Types.Descr

  # A type that contains the runtime value `v`. It is the exact singleton (at Descr granularity:
  # integers, floats and binaries are not told apart) when precise?(v); otherwise it is a
  # superset, and only disjointness may be concluded from it.
  def of(v) when is_atom(v), do: atom([v])
  def of(v) when is_integer(v), do: integer()
  def of(v) when is_float(v), do: float()
  def of(v) when is_binary(v), do: binary()
  def of(v) when is_bitstring(v), do: bitstring_no_binary()
  def of(v) when is_pid(v), do: pid()
  def of(v) when is_reference(v), do: reference()
  def of(v) when is_port(v), do: port()
  def of([]), do: empty_list()
  def of(v) when is_list(v), do: of_list(v, [])
  def of(v) when is_tuple(v), do: tuple(Enum.map(Tuple.to_list(v), &of/1))

  def of(v) when is_map(v) do
    {atoms, others} = Enum.split_with(Map.to_list(v), fn {k, _} -> is_atom(k) end)
    fields = for {k, x} <- atoms, do: {k, {of(x), false}}

    domains =
      others
      |> Enum.group_by(fn {k, _} -> domain(k) end, fn {_, x} -> of(x) end)
      |> Enum.map(fn {d, ts} -> {[d], Enum.reduce(ts, &opt_union/2)} end)

    closed_map(fields ++ domains)
  end

  def of(v) when is_function(v), do: fun(elem(:erlang.fun_info(v, :arity), 1))

  defp domain(k) when is_integer(k), do: :integer
  defp domain(k) when is_float(k), do: :float
  defp domain(k) when is_binary(k), do: :binary
  defp domain(k) when is_bitstring(k), do: :bitstring_no_binary
  defp domain(k) when is_tuple(k), do: :tuple
  defp domain(k) when is_list(k), do: :list
  defp domain(k) when is_map(k), do: :map
  defp domain(k) when is_function(k), do: :fun
  defp domain(k) when is_pid(k), do: :pid
  defp domain(k) when is_port(k), do: :port
  defp domain(k) when is_reference(k), do: :reference

  defp of_list([h | t], acc), do: of_list(t, [of(h) | acc])
  defp of_list([], acc), do: non_empty_list(Enum.reduce(acc, &opt_union/2))
  defp of_list(tail, acc), do: non_empty_list(Enum.reduce(acc, &opt_union/2), of(tail))

  # of(v) is exactly {v} at Descr granularity. Lists (no length in list types), maps with
  # non-atom keys (domain keys are optional) and funs (fun(n) is the top of arity n) are not.
  def precise?(v) when is_atom(v) or is_number(v) or is_binary(v), do: true
  def precise?([]), do: true
  def precise?(v) when is_tuple(v), do: v |> Tuple.to_list() |> Enum.all?(&precise?/1)
  def precise?(v) when is_map(v), do: Enum.all?(Map.to_list(v), fn {k, x} -> is_atom(k) and precise?(x) end)
  def precise?(_), do: false

  # true: v is in t; false: v is certainly not in t (also when imprecise: disjoint).
  def member?(v, t) do
    t = upper_bound(t)
    if precise?(v), do: subtype?(of(v), t), else: not disjoint?(of(v), t)
  end

  def kind(v) when is_atom(v), do: "atom"
  def kind(v) when is_integer(v), do: "integer"
  def kind(v) when is_float(v), do: "float"
  def kind(v) when is_binary(v), do: "binary"
  def kind(v) when is_list(v), do: "list"
  def kind(v) when is_tuple(v), do: "tuple"
  def kind(v) when is_map(v), do: "map"
  def kind(v) when is_function(v), do: "fun"
  def kind(_), do: "other"
end

defmodule H.Alg do
  # Oracle 6: set algebra on the case's own types. Leaves are the types the checker inferred for
  # the case (signature arguments and returns, static upper bounds and gradual) plus the types of the
  # values the calls saw; random expressions combine them with union/intersection/difference/negation,
  # once with the opt_* and once with the bare_* operations. Descr types denote sets of values, so:
  #   - bare and opt agree (both "compute the union/... of two descrs", descr.ex): equal?
  #   - a value v whose type of(v) is an exact singleton (H.Mem.precise?) is in A op B iff the
  #     boolean op of "v in A", "v in B" holds (static types only)
  #   - projections keep members: elem(v, i) in tuple_fetch(E, i), v[k] in map_fetch_key(E, k),
  #     Map.put(v, k, x) in map_put_key(E, k, of(x)), Tuple.insert_at/delete_at, Map.delete
  # Signatures: algebra__bareopt__<kind>, algebra__member__<kind of v>, algebra__proj__<op>.
  alias Module.Types.Descr, as: D

  @n_static 10
  @per_expr 10
  @n_wide 8
  @n_gradual 4
  @max_values 20
  @universe [:a, :b, :ok, nil, true, 0, 1.0, "k", [], {}, {:ok, 1}, {1, :a}, {:a, :b, :c}, %{}, %{a: 1},
             %{a: :b, b: 0}, %{c: {:ok, 1}}, {:error, %{a: 1}}, %{a: %{b: :c}}, {%{}, {}}]

  def check(src, sig_types, values) do
    :rand.seed(:exsss, {:erlang.phash2(src), 7, 11})
    precise = values |> Enum.flat_map(&subvalues(&1, 2)) |> Enum.uniq() |> Enum.filter(&H.Mem.precise?/1)
    precise = precise |> Enum.flat_map(&mutants/1) |> Enum.uniq() |> Enum.take(@max_values)
    precise = Enum.take(Enum.uniq(precise ++ @universe), @max_values + 8)
    vtypes = values |> Enum.uniq() |> Enum.take(12) |> Enum.map(&H.Mem.of/1)

    # value types widened at random places (optional/dropped fields, open maps and tuples, unions)
    wide = for _ <- 1..@n_wide, precise != [], do: widen(Enum.random(precise), 3)
    static = Enum.uniq(Enum.map(sig_types, &D.upper_bound/1) ++ vtypes ++ wide ++ base())
    static = Enum.reject(static, &(&1 == D.term()))
    gradual = Enum.uniq(Enum.filter(sig_types, &D.gradual?/1) ++ Enum.map(Enum.take(static, 6), &D.dynamic/1))
    static = List.to_tuple(static)
    gradual = List.to_tuple(gradual ++ Tuple.to_list(static))

    # leaf memberships, computed on demand (the process dictionary caches {:alg_m, leaf, value index})
    for {k, _} <- Process.get(), match?({:alg_m, _, _}, k), do: Process.delete(k)
    leaf_ms = {static, List.to_tuple(Enum.map(precise, &H.Mem.of/1))}

    res =
      Enum.reduce_while(1..@n_static, {0, 0}, fn _, {ne, nm} ->
        e = expr(tuple_size(static), 3)
        case check_static(e, static, leaf_ms, precise) do
          {:ok, m} -> {:cont, {ne + 1, nm + m}}
          fail -> {:halt, fail}
        end
      end)

    with {ne, nm} <- res do
      Enum.reduce_while(1..@n_gradual, {ne, nm}, fn _, {ne, nm} ->
        e = expr(tuple_size(gradual), 2)
        case bareopt(e, gradual) do
          :ok -> {:cont, {ne + 1, nm}}
          fail -> {:halt, fail}
        end
      end)
    end
  end

  defp base do
    [D.atom([:a, :b]), D.integer(), D.open_map(), D.tuple(), D.open_tuple([D.atom([:ok])]),
     D.closed_map(a: {D.atom([:a]), true}), D.open_map(a: {D.term(), false}), D.closed_map(a: {D.integer(), false}, b: {D.term(), true}),
     D.tuple([D.term(), D.term()]), D.non_empty_list(D.term()), D.empty_list(), D.binary()]
  end

  def widen(v, d) when is_atom(v) do
    case :rand.uniform(4) do
      1 -> D.atom()
      2 -> D.atom([v, :zz])
      _ -> D.atom([v])
    end
  end

  def widen(v, d) when is_tuple(v) and d > 0 do
    es = for x <- Tuple.to_list(v), do: if(:rand.uniform(4) == 1, do: D.term(), else: widen(x, d - 1))

    case :rand.uniform(4) do
      1 -> D.open_tuple(Enum.take(es, :rand.uniform(length(es) + 1) - 1))
      _ -> D.tuple(es)
    end
  end

  def widen(v, d) when is_map(v) and d > 0 do
    fields =
      for {k, x} <- Map.to_list(v), :rand.uniform(5) > 1,
          do: {k, {if(:rand.uniform(4) == 1, do: D.term(), else: widen(x, d - 1)), :rand.uniform(3) == 1}}

    fields = if :rand.uniform(4) == 1, do: [{Enum.random([:a, :b, :c, :zz]), {widen(Enum.random([:a, 1, {}, %{}]), 1), :rand.uniform(2) == 1}} | fields], else: fields
    fields = fields |> Enum.reverse() |> Enum.uniq_by(&elem(&1, 0))
    if :rand.uniform(3) == 1, do: D.open_map(fields), else: D.closed_map(fields)
  end

  def widen(v, _d) when is_integer(v), do: Enum.random([D.integer(), D.opt_union(D.integer(), D.float())])
  def widen(v, d) do
    t = H.Mem.of(v)
    if d > 0 and :rand.uniform(3) == 1, do: D.opt_union(t, widen(Enum.random([:a, 1, {}, %{}]), d - 1)), else: t
  end

  defp subvalues(v, d) when d > 0 and is_tuple(v), do: [v | Enum.flat_map(Tuple.to_list(v), &subvalues(&1, d - 1))]
  defp subvalues(v, d) when d > 0 and is_map(v), do: [v | Enum.flat_map(Map.values(v), &subvalues(&1, d - 1))]
  defp subvalues([h | t] = v, d) when d > 0, do: [v | subvalues(h, d - 1) ++ subvalues(t, d - 1)]
  defp subvalues(v, _), do: [v]

  defp mutants(v) when is_map(v) do
    ks = Map.keys(v)
    [v, Map.put(v, :a, :b)] ++ Enum.map(Enum.take(ks, 2), &Map.delete(v, &1))
  end

  defp mutants(v) when is_tuple(v) and tuple_size(v) > 0, do: [v, Tuple.delete_at(v, 0)]
  defp mutants(v), do: [v]

  # {:leaf, i} | {:u, a, b} | {:i, a, b} | {:d, a, b} | {:n, a}
  defp expr(n, d) do
    if d == 0 or :rand.uniform(4) == 1 do
      {:leaf, :rand.uniform(n) - 1}
    else
      case :rand.uniform(7) do
        k when k <= 2 -> {:u, expr(n, d - 1), expr(n, d - 1)}
        k when k <= 4 -> {:i, expr(n, d - 1), expr(n, d - 1)}
        k when k <= 6 -> {:d, expr(n, d - 1), expr(n, d - 1)}
        _ -> {:n, expr(n, d - 1)}
      end
    end
  end

  defp ev({:leaf, i}, leaves, _), do: elem(leaves, i)
  defp ev({:u, a, b}, l, :opt), do: D.opt_union(ev(a, l, :opt), ev(b, l, :opt))
  defp ev({:i, a, b}, l, :opt), do: D.opt_intersection(ev(a, l, :opt), ev(b, l, :opt))
  defp ev({:d, a, b}, l, :opt), do: D.opt_difference(ev(a, l, :opt), ev(b, l, :opt))
  defp ev({:n, a}, l, :opt), do: D.opt_negation(ev(a, l, :opt))
  defp ev({:u, a, b}, l, :bare), do: D.bare_union(ev(a, l, :bare), ev(b, l, :bare))
  defp ev({:i, a, b}, l, :bare), do: D.bare_intersection(ev(a, l, :bare), ev(b, l, :bare))
  defp ev({:d, a, b}, l, :bare), do: D.bare_difference(ev(a, l, :bare), ev(b, l, :bare))
  defp ev({:n, a}, l, :bare), do: D.bare_negation(ev(a, l, :bare))

  defp truth({:leaf, i}, {leaves, vts}, j) do
    case Process.get({:alg_m, i, j}) do
      nil ->
        m = D.subtype?(elem(vts, j), elem(leaves, i))
        Process.put({:alg_m, i, j}, m)
        m

      m ->
        m
    end
  end

  defp truth({:u, a, b}, ms, j), do: truth(a, ms, j) or truth(b, ms, j)
  defp truth({:i, a, b}, ms, j), do: truth(a, ms, j) and truth(b, ms, j)
  defp truth({:d, a, b}, ms, j), do: truth(a, ms, j) and not truth(b, ms, j)
  defp truth({:n, a}, ms, j), do: not truth(a, ms, j)

  def show({:leaf, i}), do: "L#{i}"
  def show({:u, a, b}), do: "(#{show(a)} or #{show(b)})"
  def show({:i, a, b}), do: "(#{show(a)} and #{show(b)})"
  def show({:d, a, b}), do: "(#{show(a)} and not #{show(b)})"
  def show({:n, a}), do: "not #{show(a)}"

  defp leaves_used({:leaf, i}), do: [i]
  defp leaves_used({_, a}), do: leaves_used(a)
  defp leaves_used({_, a, b}), do: leaves_used(a) ++ leaves_used(b)

  defp describe(e, leaves) do
    ls = e |> leaves_used() |> Enum.uniq() |> Enum.sort()
    "expression #{show(e)} where\n" <>
      Enum.map_join(ls, "\n", fn i -> "  L#{i} = #{D.to_quoted_string(elem(leaves, i))}" end)
  end

  defp bareopt(e, leaves) do
    o = ev(e, leaves, :opt)
    H.Print.note(o, "oracle 6 set expression")
    b = ev(e, leaves, :bare)

    if D.equal?(o, b) do
      :ok
    else
      x = D.opt_union(D.opt_difference(o, b), D.opt_difference(b, o))
      {:fail, "algebra__bareopt__" <> kind_of(x),
       "opt_* and bare_* disagree on #{describe(e, leaves)}\nopt:  #{D.to_quoted_string(o)}\nbare: #{D.to_quoted_string(b)}"}
    end
  end

  @kinds [{"map", :open_map}, {"tuple", :tuple}, {"fun", :fun}, {"atom", :atom}]
  defp kind_of(x) do
    x = D.upper_bound(x)
    Enum.find_value(@kinds, "other", fn {k, f} -> if not D.empty?(D.opt_intersection(x, apply(D, f, []))), do: k end) |> then(fn k ->
      if k == "other" and not D.empty?(D.opt_intersection(x, D.opt_union(D.list(D.term()), D.non_empty_list(D.term(), D.term())))), do: "list", else: k
    end)
  end

  defp check_static(e, leaves, ms, values) do
    with :ok <- bareopt(e, leaves) do
      t = ev(e, leaves, :opt)

      values
      |> Enum.with_index()
      |> Enum.take_random(@per_expr)
      |> Enum.reduce_while({:ok, 0}, fn {v, j}, {:ok, n} ->
        want = truth(e, ms, j)
        got = D.subtype?(H.Mem.of(v), t)

        cond do
          want != got ->
            {:halt, {:fail, "algebra__member__" <> H.Mem.kind(v),
             "value #{inspect(v)} (type #{D.to_quoted_string(H.Mem.of(v))}) should #{if want, do: "", else: "not "}be in the #{describe(e, leaves)}\n" <>
               "membership of the value in each leaf: " <> Enum.map_join(Enum.uniq(leaves_used(e)), ", ", fn i -> "L#{i}=#{truth({:leaf, i}, ms, j)}" end) <>
               "\ncomputed type: #{D.to_quoted_string(t)}"}}

          want ->
            case proj(v, t) do
              :ok -> {:cont, {:ok, n + 1}}
              {:fail, sig, msg} -> {:halt, {:fail, sig, msg <> "\nfor the #{describe(e, leaves)}\ncomputed type: #{D.to_quoted_string(t)}"}}
            end

          true ->
            {:cont, {:ok, n + 1}}
        end
      end)
    end
  end

  defp in?(v, t), do: D.subtype?(H.Mem.of(v), D.upper_bound(t))

  defp proj(v, t) when is_tuple(v) do
    et = D.opt_intersection(t, D.tuple())
    n = tuple_size(v)

    fetch =
      for i <- 0..(n - 1)//1,
          {_, ti} <- [D.tuple_fetch(et, i)],
          not in?(elem(v, i), ti),
          do: {"tuple_fetch", "elem(#{inspect(v)}, #{i}) = #{inspect(elem(v, i))} not in tuple_fetch(E, #{i}) = #{D.to_quoted_string(ti)}"}

    del =
      for i <- 0..(n - 1)//1,
          d = D.tuple_delete_at(et, i),
          is_map(d) or d == :term,
          not in?(Tuple.delete_at(v, i), d),
          do: {"tuple_delete_at", "Tuple.delete_at(#{inspect(v)}, #{i}) not in tuple_delete_at(E, #{i}) = #{D.to_quoted_string(d)}"}

    ins =
      for i <- 0..n,
          d = D.tuple_insert_at(et, i, D.atom([:z])),
          is_map(d) or d == :term,
          not in?(Tuple.insert_at(v, i, :z), d),
          do: {"tuple_insert_at", "Tuple.insert_at(#{inspect(v)}, #{i}, :z) not in tuple_insert_at(E, #{i}, :z) = #{D.to_quoted_string(d)}"}

    report(fetch ++ del ++ ins, v)
  end

  defp proj(v, t) when is_map(v) do
    em = D.opt_intersection(t, D.open_map())
    ks = Map.keys(v)

    fetch =
      for k <- ks,
          {_, tk} <- [D.map_fetch_key(em, k)],
          not in?(Map.fetch!(v, k), tk),
          do: {"map_fetch_key", "#{inspect(v)}[#{inspect(k)}] not in map_fetch_key(E, #{inspect(k)}) = #{D.to_quoted_string(tk)}"}

    put =
      for k <- Enum.uniq(ks ++ [:a, :b, :c]),
          {:ok, d} <- [D.map_put_key(em, k, D.atom([:z]))],
          not in?(Map.put(v, k, :z), d),
          do: {"map_put_key", "Map.put(#{inspect(v)}, #{inspect(k)}, :z) not in map_put_key(E, #{inspect(k)}, :z) = #{D.to_quoted_string(d)}"}

    del =
      for k <- Enum.uniq(ks ++ [:a, :c]),
          {_, d, _} <- [D.map_update(em, D.atom([k]), :delete, :keep, true)],
          not in?(Map.delete(v, k), d),
          do: {"map_delete", "Map.delete(#{inspect(v)}, #{inspect(k)}) not in map_update(E, #{inspect(k)}, :delete, :keep) = #{D.to_quoted_string(d)}"}

    upd =
      for k <- ks,
          {_, d, _} <- [D.map_update(em, D.atom([k]), D.atom([:z]), :reject, true)],
          not in?(Map.put(v, k, :z), d),
          do: {"map_update", "%{#{inspect(v)} | #{inspect(k)} => :z} not in map_update(E, #{inspect(k)}, :z, :reject) = #{D.to_quoted_string(d)}"}

    report(fetch ++ put ++ del ++ upd, v)
  end

  defp proj(_, _), do: :ok

  defp report([], _), do: :ok
  defp report([{op, msg} | _], v), do: {:fail, "algebra__proj__" <> op, "value #{inspect(v)} is in E, but " <> msg}
end

defmodule H.Print do
  # Oracle 10: printed types denote the printed type. Descr.to_quoted_string/1 ("Converts a descr to
  # its quoted string representation", descr.ex) is what every type diagnostic shows. The string is
  # parsed back (Code.string_to_quoted) and evaluated by an interpreter of the type syntax
  # (gradual-set-theoretic-types.md: `integer() or atom()`, `not`, `and`, `dynamic(t)`, `%{..., a: t}`,
  # `if_set(t)`, `not_set()`, `%{integer() => t}`, `{a, ...}`, `non_empty_list(t, tail)`, `(a -> b)`,
  # `%S{}` structs whose omitted fields are term() (:collapse_structs)). The result must be equal? to
  # the original. The fun part is compared on its own and in one direction only (fun_to_quoted drops the
  # negated arrows of each clause by design, see fun_form/1); types with a fun nested inside a tuple, map
  # or list are skipped.
  # Signatures: print__<parse|eval|lost|extra>__<kind>, lost = values of the type the printed type
  # misses, extra = values the printed type has and the type does not.
  alias Module.Types.Descr, as: D

  @max_types 60

  # types are collected while the other oracles run (process dictionary, reset per case)
  def note(t, label) do
    l = Process.get(:print_types, [])
    if length(l) < 4 * @max_types, do: Process.put(:print_types, [{t, label} | l])
  end

  def check_all do
    Process.delete(:print_not_arity)
    # term order, not collection order: the other oracles walk runtime maps, whose atom key order follows
    # atom indexes (different between VMs), so a sorted list keeps repro.sh deterministic
    types = (Process.delete(:print_types) || []) |> Enum.uniq_by(&elem(&1, 0)) |> Enum.sort() |> Enum.take(@max_types)
    types = types ++ derived(Enum.map(types, &elem(&1, 0)))

    Enum.reduce_while(types, {0, 0}, fn {t, label}, {n, skip} ->
      case check(t, label) do
        :ok -> {:cont, {n + 1, skip}}
        :skip -> {:cont, {n, skip + 1}}
        {:fail, sig, msg} -> {:halt, {:fail, sig, smallest(t, sig) <> msg}}
      end
    end)
  end

  # Types the printer sees inside other types: the case's types put in containers (tuple element, map
  # field, optional field, domain value, list element and tail, dynamic) and combined pairwise.
  # Seeded by the types themselves (deterministic in repro).
  @n_derived 40
  defp derived([]), do: []

  defp derived(ts) do
    old = :rand.export_seed()
    :rand.seed(:exsss, {:erlang.phash2(ts), 3, 5})
    d1 = derived1(ts)
    # a second round puts derived types inside containers and combinations again
    res = d1 ++ Enum.take(derived1(Enum.map(d1, &elem(&1, 0)) ++ ts), div(@n_derived, 2))
    if old != :undefined, do: :rand.seed(old)
    res
  end

  defp derived1(ts) do
    ts = List.to_tuple(Enum.reject(ts, &(&1 == D.term())))
    pick = fn -> elem(ts, :rand.uniform(tuple_size(ts)) - 1) end

    for _ <- 1..@n_derived do
      a = pick.()
      b = pick.()

      t =
        case :rand.uniform(12) do
          1 -> D.tuple([a, D.atom([:ok])])
          2 -> D.open_tuple([a])
          3 -> D.closed_map(a: {a, false}, b: {b, true})
          4 -> D.open_map(a: {a, :rand.uniform(2) == 1})
          5 -> D.closed_map([{[:integer], a}, {:a, {b, false}}])
          6 -> D.non_empty_list(a, b)
          7 -> D.list(a)
          8 -> D.opt_difference(a, b)
          9 -> D.opt_intersection(a, D.opt_negation(b))
          10 -> D.opt_union(D.dynamic(a), b)
          11 -> D.opt_difference(D.opt_union(a, b), D.opt_intersection(a, b))
          12 -> D.tuple([D.opt_difference(a, b), D.opt_difference(b, a)])
        end

      {t, "derived type"}
    end
  rescue
    _ -> []
  end

  # the single descr component (atom, map, tuple, list, bitmap, ...) that still fails, as a smaller repro
  defp smallest(t, sig) when is_map(t) and map_size(t) > 1 do
    t
    |> Map.to_list()
    |> Enum.find_value("", fn {k, v} ->
      c = %{k => v}
      case check(c, "component") do
        {:fail, ^sig, msg} -> "smallest failing component #{inspect(k)}:\n#{msg}\n----- the whole type:\n"
        _ -> nil
      end
    end)
  rescue
    _ -> ""
  end

  defp smallest(_, _), do: ""

  def check(t, label) do
    with true <- checkable?(t),
         str = D.to_quoted_string(t, skip_dynamic_for_indivisible: false),
         {:ok, ast} <- parse(str, t, label),
         {:ok, back} <- eval_top(ast, str, t, label) do
      {t1, b1} = strip_fun(t, back)
      {tf, bf} = only_fun(t, back)
      # the fun part is printed without its negated arrows (fun_to_quoted): it may only be wider
      cond do
        fun_form(t) == :union and not D.subtype?(tf, bf) ->
          mismatch("lost", D.opt_difference(tf, bf), str, back, t, label)

        fun_form(t) == :negation and not D.subtype?(bf, tf) ->
          mismatch("extra", D.opt_difference(bf, tf), str, back, t, label)

        D.equal?(t1, b1) ->
          :ok

        true ->
          if D.subtype?(t1, b1),
            do: mismatch("extra", D.opt_difference(b1, t1), str, back, t, label),
            else: mismatch("lost", D.opt_difference(t1, b1), str, back, t, label)
      end
    else
      false -> :skip
      fail -> fail
    end
  end

  defp mismatch(dir, x, str, back, t, label) do
    {:fail, "print__#{dir}__#{kind(x)}",
     "#{label}: printed type does not denote the type\nprinted: #{str}\nreprinted from the parse: #{D.to_quoted_string(back)}\n" <>
       "#{dir}: #{D.to_quoted_string(x)}\ninternal: #{inspect(t, limit: :infinity)}"}
  end

  # a fun component is only allowed at the top level of the descr
  defp checkable?(t) do
    rest = if is_map(t), do: Map.drop(t, [:fun]), else: t
    rest = if is_map(rest) and is_map(rest[:dynamic]), do: Map.update!(rest, :dynamic, &Map.delete(&1, :fun)), else: rest
    s = inspect(rest, limit: :infinity)
    not String.contains?(s, "fun:") and not String.contains?(s, "#Reference")
  end

  defp strip_fun(t, back) do
    nf = D.opt_negation(D.fun())
    {D.opt_intersection(t, nf), D.opt_intersection(back, nf)}
  end

  # fun_to_quoted prints the positive arrows of each DNF clause only: a `{:union, _}` fun part may print
  # wider, a `{:negation, _}` one (fun() and not ...) narrower; both forms at once are not compared
  defp fun_form(t) when is_map(t) do
    dyn = Map.get(t, :dynamic, %{})
    # with funs in both parts the dynamic one is printed as a difference with the static one, whose
    # positive-only clauses may be wider or narrower
    case for(%{fun: {f, _}} <- [t, if(is_map(dyn), do: dyn, else: %{})], do: f) do
      [f] -> f
      [] -> :union
      _ -> :mixed
    end
  end

  defp fun_form(_), do: :union

  defp only_fun(t, back), do: {D.opt_intersection(t, D.fun()), D.opt_intersection(back, D.fun())}

  # `atom() and not(:a, :b)`: atom_to_quoted({:negation, set}) puts every negated atom in one `not` node.
  # Reported for 1 type in @not_arity_sample (phash2 of the term: the printed atom order follows atom
  # indexes, which differ between VMs) (it is in most cases with a case on atoms); the
  # others are evaluated from the quoted form with the node read as `not (:a or :b)`, so the rest of the
  # type is still checked.
  @not_arity_sample 12
  defp parse(str, t, label) do
    case Code.string_to_quoted(str) do
      {:ok, ast} ->
        {:ok, ast}

      {:error, e} ->
        quoted = D.to_quoted(t, skip_dynamic_for_indivisible: false)
        area = bad_node(quoted)

        if area == "not_arity" and rem(:erlang.phash2(t), @not_arity_sample) != 0 do
          Process.put(:print_not_arity, (Process.get(:print_not_arity) || 0) + 1)
          {:ok, repair(quoted)}
        else
          {:fail, "print__parse__#{area}", "#{label}: printed type does not parse: #{inspect(e)}\nprinted: #{str}\ninternal: #{inspect(t, limit: :infinity)}"}
        end
    end
  end

  defp repair(quoted) do
    Macro.prewalk(quoted, fn
      {:not, m, [_, _ | _] = args} -> {:not, m, [Enum.reduce(args, &{:or, [], [&2, &1]})]}
      n -> n
    end)
  end

  # the construct that makes the printed type unparsable: an operator node with the wrong arity
  @arity %{not: [1], and: [2], or: [2], dynamic: [0, 1], if_set: [1], not_set: [0], ->: [2]}
  defp bad_node(ast) do
    {_, found} =
      Macro.prewalk(ast, nil, fn
        {op, _, args} = n, nil when is_atom(op) and is_list(args) ->
          if Map.has_key?(@arity, op) and length(args) not in @arity[op], do: {n, "#{op}_arity"}, else: {n, nil}
        n, acc -> {n, acc}
      end)
    found || "other"
  end

  defp eval_top(ast, str, t, label) do
    {:ok, ev(ast)}
  rescue
    e ->
      {:fail, "print__eval__#{kind(t)}",
       "#{label}: printed type uses syntax outside the documented type syntax: #{Exception.message(e)}\nprinted: #{str}\ninternal: #{inspect(t, limit: :infinity)}"}
  end

  @kinds [{"map", :open_map}, {"tuple", :tuple}, {"list", :list_top}, {"atom", :atom}, {"fun", :fun}]
  def kind(x) do
    x = D.upper_bound(x)
    Enum.find_value(@kinds, "other", fn {k, f} ->
      ty = if f == :list_top, do: D.non_empty_list(D.term(), D.term()), else: apply(D, f, [])
      if not D.empty?(D.opt_intersection(x, ty)), do: k
    end)
  end

  @nullary %{term: :term, none: :none, atom: :atom, boolean: :boolean, integer: :integer, float: :float,
             binary: :binary, bitstring: :bitstring, pid: :pid, port: :port, reference: :reference,
             empty_list: :empty_list, empty_map: :empty_map, map: :open_map, tuple: :tuple, fun: :fun,
             dynamic: :dynamic}

  def ev({:__block__, _, [x]}), do: ev(x)
  def ev({:or, _, [a, b]}), do: D.opt_union(ev(a), ev(b))
  def ev({:and, _, [a, {:not, _, [b]}]}), do: D.opt_difference(ev(a), ev(b))
  def ev({:and, _, [a, b]}), do: D.opt_intersection(ev(a), ev(b))
  def ev({:not, _, [a]}), do: D.opt_negation(ev(a))
  def ev({:dynamic, _, [a]}), do: D.dynamic(ev(a))
  def ev({:list, _, [a]}), do: D.list(ev(a))
  def ev({:list, _, [a, b]}), do: D.opt_union(D.empty_list(), D.non_empty_list(ev(a), ev(b)))
  def ev({:non_empty_list, _, [a]}), do: D.non_empty_list(ev(a))
  def ev({:non_empty_list, _, [a, b]}), do: D.non_empty_list(ev(a), ev(b))
  def ev({:non_struct_map, _, []}), do: D.open_map(__struct__: {D.opt_negation(D.atom()), true})
  def ev({:fun, _, [n]}) when is_integer(n), do: D.fun(n)
  def ev({name, _, args}) when is_atom(name) and args in [[], nil] and is_map_key(@nullary, name), do: apply(D, @nullary[name], [])
  def ev(a) when is_atom(a), do: D.atom([a])
  def ev({:__aliases__, _, parts}), do: D.atom([Module.concat(parts)])
  def ev({a, b}), do: ev_tuple([a, b])
  def ev({:{}, _, es}), do: ev_tuple(es)
  def ev([{:->, _, [args, ret]}]), do: D.fun(Enum.map(args, &ev/1), ev(ret))
  def ev({:%{}, _, entries}), do: ev_map(entries)
  def ev({:%, _, [s, {:%{}, _, entries}]}), do: ev_struct(s, entries)
  def ev(other), do: raise("cannot evaluate #{Macro.to_string(other)}")

  defp ev_tuple(es) do
    case List.last(es) do
      {:..., _, _} -> D.open_tuple(Enum.map(Enum.drop(es, -1), &ev/1))
      _ -> D.tuple(Enum.map(es, &ev/1))
    end
  end

  defp ev_map(entries) do
    {open, entries} =
      case entries do
        [{:..., _, _} | rest] -> {true, rest}
        _ -> {false, entries}
      end

    entries = Enum.flat_map(entries, fn
      kw when is_list(kw) -> kw
      e -> [e]
    end)

    fields =
      Enum.flat_map(entries, fn {k, v} ->
        k = unblock(k)
        if is_atom(k) or match?({:__aliases__, _, _}, k) do
          key = if is_atom(k), do: k, else: Module.concat(elem(k, 2))
          [{key, field(v)}]
        else
          if open, do: raise("domain key in an open map")
          vt = case unblock(v) do
            {:not_set, _, []} -> D.none()
            v -> ev(v)
          end
          for d <- domains(k), do: {[d], vt}
        end
      end)

    if open, do: D.open_map(fields), else: D.closed_map(fields)
  end

  defp unblock({:__block__, _, [x]}), do: unblock(x)
  defp unblock(x), do: x

  defp domains({:bitstring, _, []}), do: [:binary, :bitstring_no_binary]
  defp domains({:and, _, [{:bitstring, _, []}, {:not, _, [{:binary, _, []}]}]}), do: [:bitstring_no_binary]
  defp domains({name, _, []}) when name in [:integer, :float, :binary, :pid, :port, :reference, :fun, :tuple, :map, :list, :atom],
    do: [name]
  defp domains(k), do: raise("unknown domain key #{Macro.to_string(k)}")

  defp field(v) do
    case unblock(v) do
      {:if_set, _, [x]} -> {ev(x), true}
      {:not_set, _, []} -> {D.none(), true}
      x -> {ev(x), false}
    end
  end

  defp ev_struct({:__aliases__, _, parts}, entries) do
    s = Module.concat(parts)
    given = Map.new(Enum.flat_map(entries, fn
      kw when is_list(kw) -> kw
      e -> [e]
    end), fn {k, v} -> {unblock(k), field(v)} end)

    fields =
      for %{field: f} <- s.__info__(:struct), do: {f, Map.get(given, f, {D.term(), false})}

    D.closed_map([{:__struct__, {D.atom([s]), false}} | fields])
  end
end

defmodule H.Worker do
  import Module.Types.Descr, only: [to_quoted_string: 1, opt_union: 2, none: 0]

  @compile_ms 30_000

  def check(%{"src" => src, "calls" => calls} = case_, compile_ms \\ @compile_ms) do
    purge()
    task = Task.async(fn -> compile(src) end)

    result =
      case Task.yield(task, compile_ms) || Task.shutdown(task, :brutal_kill) do
        {:ok, res} -> res
        nil -> {:fail, "hang__compile__typecheck", "compiling took more than #{compile_ms} ms"}
        {:exit, reason} -> {:skip, "compile_exit", inspect(reason)}
      end

    Process.put(:funapps_term, 0)
    Process.delete(:print_types)

    out =
      case result do
        {:compiled, bin, nwarn, claims, fails} ->
          Process.delete(:ok_hits)
          Process.delete(:all_hits)
          # only a rescue/catch (try, or a def-level rescue) can swallow a callee's exception
          Process.put(:trace_aborts, String.contains?(src, ["rescue", "catch"]))
          # oracle 5's extra arguments: values of the fn literals' patterns and the calls' arguments
          {extra, _} = Code.eval_string(Map.get(case_, "funargs", "[]"))
          calls_args = for %{"args" => a} <- calls, do: elem(Code.eval_string(a), 0)
          Process.put(:funargs1, Enum.uniq(extra ++ for([a] <- calls_args, do: a)))
          Process.put(:funargs2, Enum.uniq(for [_, _] = a <- calls_args, do: a))
          out = run_calls(bin, calls) |> Map.put("warnings", nwarn) |> Map.put("claims", length(claims))
          hits0 = {Process.get(:ok_hits), Process.get(:all_hits)}
          out = out |> Map.put("fails", length(fails))
          out = if out["v"] == "pass", do: check_claims(out, claims, Map.get(case_, "probes", [])), else: out
          out = if out["v"] == "pass", do: check_fails(out, fails, Map.get(case_, "lines", []), Map.get(case_, "posts", [])), else: out
          out = if out["v"] == "pass", do: check_algebra(out, src, extra), else: out
          out = if out["v"] == "pass", do: check_heads(out, src, calls_args), else: out
          out = if out["v"] == "pass", do: check_expr(out, case_), else: out
          out = if out["v"] == "pass", do: check_print(out), else: out
          if out["v"] == "pass", do: check_gradual(out, case_, hits0, compile_ms), else: out
        {:fail, sig, msg} -> %{"v" => "fail", "sig" => sig, "msg" => msg}
        {:skip, why, msg} -> %{"v" => "skip", "skip" => why, "msg" => String.slice(msg, 0, 400)}
      end

    purge()
    # oracle 5 applications whose expected result was term() (vacuous)
    Map.put(out, "funapps_term", Process.delete(:funapps_term) || 0)
  end

  # Oracle 8 (H.Head): the case's clause heads lifted into CaseHead and applied to many values
  defp check_heads(out, src, calls_args) do
    case H.Head.check(src, calls_args) do
      %{"v" => "fail"} = fail -> fail
      counts -> Map.merge(out, counts)
    end
  end

  # Oracle 9 (H.Expr): single expressions typed with chosen variable types, judged by their evaluation.
  # Seeded by the case's eseed only (the shrinker keeps it), so the module does not matter.
  defp check_expr(out, case_) do
    seed = Map.get(case_, "eseed") || :erlang.phash2(case_["src"])

    case H.Expr.check(seed) do
      %{"v" => "fail"} = fail ->
        msg = "(oracle 9, H.Expr: drawn from eseed #{seed}; the module and calls in case.ex are irrelevant)\n" <> fail["msg"]
        %{fail | "msg" => msg} |> Map.put("noshrink", true)

      counts -> Map.merge(out, counts)
    end
  end

  # Oracle 10 (H.Print): the types oracles 1, 6 and 9 saw, printed, parsed back and evaluated
  defp check_print(out) do
    case H.Print.check_all() do
      {:fail, sig, msg} -> %{"v" => "fail", "sig" => sig, "msg" => String.slice(msg, 0, 6000)} |> Map.put("noshrink", true)
      {n, skip} ->
        out |> Map.put("print_checked", n) |> Map.put("print_skipped", skip) |> Map.put("print_not_arity_masked", Process.delete(:print_not_arity) || 0)
    end
  end

  # Oracle 6 (H.Alg): set algebra and projections over the case's inferred types and seen values
  defp check_algebra(out, src, extra) do
    types = Process.delete(:alg_types) || []
    values = (Process.delete(:alg_values) || []) ++ extra

    case H.Alg.check(src, types, values) do
      {ne, nm} -> out |> Map.put("alg_exprs", ne) |> Map.put("alg_members", nm)
      {:fail, sig, msg} -> %{"v" => "fail", "sig" => sig, "msg" => String.slice(msg, 0, 4000)}
    end
  end

  # Oracle 7 (gradual__<mode>__<area>): oracles 3 and 4 on a variant of the module that runs exactly like it
  # but whose checker sees less precise types, so the checker's gradual (dynamic()) code paths judge the same
  # runtime evidence. Variants: "wrap", gen.rs wraps value subexpressions in CaseDyn.id/1 (an identity typed
  # dynamic(); same lines, same probes, same values); "noinfer", the same source compiled with
  # infer_signatures: false, which "disables module-local signature inference used when type checking remote
  # calls to the compiled module. Type checking will be executed regardless" (code.ex), so the self-remote
  # CaseMod.f calls are dynamic() and Module.Types.Traverse runs. Contract as for oracles 3/4
  # (gradual-set-theoretic-types.md:236/:244), and :214/:216: with a gradual type "only a subset of the type
  # needs to be valid", warnings come "once it is certain the code will fail".
  # The wrap variant's calls are also run against its own inferred signatures (oracles 1 and 5,
  # area `ret_<kind>`, `domain_<kind>`, `funapp_<origin>`).
  # Not compared: which warnings the original has. A variant may legitimately warn where the original does
  # not: the checker skips code it proves dead (`Map.pop!(m, k) || rhs`), code after a violation, and some
  # literal-only expressions (`is_nil({"k"})`, `case {0.5, :b} do %{} -> ...`); only runtime evidence decides.
  defp check_gradual(out, case_, {ok_hits, all_hits}, compile_ms) do
    variant =
      case case_ do
        %{"gvar" => "wrap", "gsrc" => gsrc} -> {"wrap", gsrc, false}
        %{"gvar" => "noinfer", "src" => src} -> {"noinfer", src, true}
        _ -> nil
      end

    case variant do
      nil ->
        out

      {mode, vsrc, noinfer} ->
        purge()
        prev = Code.get_compiler_option(:infer_signatures)

        task =
          Task.async(fn ->
            if noinfer, do: Code.put_compiler_option(:infer_signatures, false)

            try do
              compile(vsrc)
            after
              Code.put_compiler_option(:infer_signatures, prev)
            end
          end)

        result =
          case Task.yield(task, compile_ms) || Task.shutdown(task, :brutal_kill) do
            {:ok, res} -> res
            nil -> {:fail, "hang__compile__typecheck", "compiling the #{mode} variant took more than #{compile_ms} ms"}
            {:exit, reason} -> {:skip, "compile_exit", inspect(reason)}
          end

        Code.put_compiler_option(:infer_signatures, prev)
        out = Map.put(out, "gvar_" <> mode, 1)

        case result do
          {:compiled, bin, _nwarn, claims, fails} ->
            # the original run's probe hits (check_claims/check_fails consume them)
            Process.put(:ok_hits, ok_hits)
            Process.put(:all_hits, all_hits)
            vout = check_claims(%{"v" => "pass"}, claims, Map.get(case_, "probes", []))

            vout =
              if vout["v"] == "pass",
                do: check_fails(vout, fails, Map.get(case_, "lines", []), Map.get(case_, "posts", [])),
                else: vout

            out = out |> Map.put("gvar_claims", length(claims)) |> Map.put("gvar_fails", length(fails))

            # oracles 1 and 5 on the wrap variant: same values, its own inferred signatures (the noinfer
            # variant has none)
            vout =
              if vout["v"] == "pass" and mode == "wrap" do
                r = run_calls(bin, case_["calls"])
                for k <- [:alg_types, :alg_values, :ok_hits, :all_hits], do: Process.delete(k)
                if r["v"] == "fail", do: r, else: Map.put(vout, "gvar_ok", r["ok"])
              else
                vout
              end

            out = Map.put(out, "gvar_ok", vout["gvar_ok"] || 0)

            case vout do
              %{"v" => "fail", "sig" => sig, "msg" => msg} ->
                area =
                  case sig do
                    "warn__clause__" <> k -> "clause_" <> k
                    "warn__expr__" <> a -> a
                    "sound__" <> rest -> String.replace(rest, "__", "_", global: false)
                    other -> other
                  end

                %{"v" => "fail", "sig" => "gradual__#{mode}__#{area}",
                  "msg" => "(#{mode} variant, see gsrc in case.json) " <> msg}

              _ ->
                out
            end

          {:fail, sig, msg} ->
            %{"v" => "fail", "sig" => sig, "msg" => "(#{mode} variant, see gsrc in case.json) " <> msg}

          {:skip, why, msg} ->
            out |> Map.put("gvar_skip", 1) |> Map.put("gvar_skip_msg", why <> ": " <> String.slice(msg, 0, 300))
        end
    end
  end

  # Oracle 3: a warning says a clause never runs (redundant, cannot match, will never match,
  # guard will never succeed), but its probe ran during a call that returned normally (a clause
  # that always raises may be reported as never matching): gradual-set-theoretic-types.md:236 says the
  # checker reports code where "all combinations of a type _will_ fail". Probes sit on the first
  # line of each def clause, case/else arm and fn clause body; the head is alone on its line.
  defp check_claims(out, claims, probes) do
    hit = Process.delete(:ok_hits) || MapSet.new()
    lines = for [id, line, kind] <- probes, MapSet.member?(hit, id), into: %{}, do: {line, kind}

    # keep :ok_hits for check_fails
    Process.put(:ok_hits, hit)

    {struct_gone, claims} = Enum.split_with(claims, fn {_, msg} -> removed_struct_field?(msg) end)
    out = Map.put(out, "claims_removed_struct_field", length(struct_gone))

    case Enum.find(claims, fn {line, _} -> Map.has_key?(lines, line) end) do
      nil ->
        out

      {line, msg} ->
        %{"v" => "fail", "sig" => "warn__clause__" <> lines[line],
          "msg" => "line #{line} ran, but the type checker warned:\n" <> String.slice(msg, 0, 2000)}
    end
  end

  # A struct pattern %CaseS{} is typed as the struct with every declared field present
  # (Of.struct_type/3; gradual-set-theoretic-types.md:294: "Elixir programs frequently pattern match on
  # structs, which reveals information about the struct fields"). A value with a field removed
  # (Map.delete/:maps.remove/Map.drop on a struct) is no longer a well-formed struct (Inspect prints it
  # as a plain map) though it still matches %CaseS{} at runtime. So "will never match" on a struct
  # pattern whose expected type says one of that struct's fields is `not_set()` is by design: counted
  # (`n_claims_removed_struct_field`), not reported.
  def removed_struct_field?(msg) do
    with [_, rest] <- String.split(msg, "will never match:\n\n", parts: 2),
         [pat | _] <- String.split(rest, "\n\n", parts: 2),
         [_, mod] <- Regex.run(~r/%(CaseS|CaseT)\{/, pat),
         [_, type] <- String.split(msg, "it is expected to match on type:\n\n", parts: 2) do
      Module.concat([mod]).__struct__()
      |> Map.keys()
      |> Enum.any?(&(&1 != :__struct__ and type =~ ~r/(^|[^\w])#{&1}: not_set\(\)/))
    else
      _ -> false
    end
  end

  # Oracle 4: a warning says an expression always fails ("incompatible types given to", "expected a
  # map within map update syntax", "incompatible types assigned to", ...: the checker reports code
  # where "all combinations of a type _will_ fail", gradual-set-theoretic-types.md:236), but every
  # expression on the warning's line evaluated without raising. `lines` lists [line, probe id] for lines
  # whose code runs unconditionally once that probe's body runs (no nested clause, branch, guard or
  # short-circuit operand on the line); the probe hit in a call that returned proves the line ran.
  # `posts` lists [line, id] for such lines inside a block statement followed by probe `id`: a hit
  # of that probe in any call (also one that raised later) proves the line ran.
  defp check_fails(out, fails, lines, posts) do
    hit = Process.delete(:ok_hits) || MapSet.new()
    all = Process.delete(:all_hits) || MapSet.new()
    ran = for [line, id] <- lines, MapSet.member?(hit, id), into: MapSet.new(), do: line
    ran = for [line, id] <- posts, MapSet.member?(all, id), into: ran, do: line
    owned = MapSet.new(lines, fn [line, _] -> line end)
    out = Map.put(out, "fails_owned", Enum.count(fails, fn {l, _, _} -> MapSet.member?(owned, l) end))
    out = Map.put(out, "lines_ran", MapSet.size(ran))

    if d = System.get_env("H_DUMP") do
      for {line, area, msg} <- fails, MapSet.member?(ran, line),
          do: File.write!(d, "=====#{area}\n#{msg}\n", [:append])
    end

    # A type with a static part (`:error or {:ok, x}`, `dynamic() or :error`) must be a subtype of
    # the domain, so "incompatible" there says only that some value of that part fails (by design,
    # gradual-set-theoretic-types.md: "If you have a type atom() or integer(), the underlying code
    # needs to work with both"): counted, not reported. Seen: Map.fetch(%{a: x}, :a) is typed
    # `{:ok, x} or :error` even though the key is required, so elem(Map.fetch(...), 1) warns.
    {sure, static} = fails |> Enum.filter(fn {l, _, _} -> MapSet.member?(ran, l) end) |> Enum.split_with(fn {_, _, m} -> certain?(m) end)
    out = Map.put(out, "fails_static_ran", length(static))

    case sure do
      [] ->
        out

      [{line, area, msg} | _] ->
        %{"v" => "fail", "sig" => "warn__expr__" <> area,
          "msg" => "line #{line} evaluated without raising, but the type checker warned:\n" <> String.slice(msg, 0, 2000)}
    end
  end

  # The warning claims that a clause never runs. "the following conditional expression will never
  # succeed" (pattern.ex badpattern, `op in [:and, :or]`) is not such a claim: it says the left operand
  # of `and`/`or` is never true (e.g. `x or %{}` where x is false), on the line of whatever clause
  # contains the operator.
  def clause_claim?(msg) do
    msg =~ ~r/is redundant|cannot match because the previous clauses|will never match|will never succeed/ and
      not (msg =~ ~r/^the following conditional expression will (never|always) succeed/m)
  end

  # The warning claims that every value fails: each type it shows for the culprit is purely
  # gradual (`dynamic(...)`) or has no static union at the top (`integer()`, `{:ok, term()}`).
  # Patterns that never match and incompatible assignments are disjointness claims: certain.
  def certain?(msg) do
    block =
      Enum.find_value(["given types:", "but got type:", "the given type does not have the given index:"], fn h ->
        case String.split(msg, h <> "\n\n", parts: 2) do
          [_, rest] -> rest |> String.split("\n\n", parts: 2) |> hd()
          _ -> nil
        end
      end)

    case block do
      nil ->
        # Same rule for :maps.update/Map.replace! ("the map: T does not have all required keys:
        # :k"): seen on :maps.update(:b, v, Map.from_keys([:a, :b], 0)).
        not static_missing_key?(msg)

      block ->
        # A static map type in which the required key is optional (`%{a: if_set(t), b: if_set(t)}`)
        # is a static union of maps with and without the key, so the same rule applies. Seen:
        # Map.from_keys([:a, :b], v) is typed with optional keys by design (map_test.exs
        # "Map.from_keys/2": closed_map(key1: {integer(), true}, ...)), so %{... | a: x} warns.
        not static_optional_key?(msg, block) and certain_block?(block)
    end
  end

  defp static_optional_key?(msg, block) do
    case Regex.run(~r/expected a map with key :(\S+) /, msg) do
      [_, key] ->
        t = block |> String.replace(~r/\s+/, " ") |> String.trim()
        not pure_dynamic?(t) and Regex.match?(~r/[{ ,]#{Regex.escape(key)}: if_set\(/, t)

      _ ->
        false
    end
  end

  # every key listed as missing is optional (`k: if_set(`) in a map type that is not pure dynamic
  defp static_missing_key?(msg) do
    case Regex.run(~r/the map:\n\n(.*?)\n\ndoes not have all required keys:\n\n(.*?)\n\n/s, msg) do
      [_, map, keys] ->
        t = map |> String.replace(~r/\s+/, " ") |> String.trim()
        keys = keys |> String.replace(~r/\s+/, " ") |> String.trim() |> top_split(", ")

        not pure_dynamic?(t) and
          Enum.all?(keys, fn k ->
            case Regex.run(~r/\A:(\S+)\z/, String.trim(k)) do
              [_, key] -> Regex.match?(~r/[{ ,]#{Regex.escape(key)}: if_set\(/, t)
              _ -> false
            end
          end)

      _ ->
        false
    end
  end

  defp certain_block?(block) do
    # args_docs_to_quoted_string (apply.ex) wraps an argument list that does not fit on one line
    # in "(\n ... \n)"; unwrapped, the whole list read as one term and every static union in it
    # (Map.fetch's `:error or {:ok, x}`) passed as certain
    block = String.replace(block, ~r/\A\s*\(\n(.*)\n\s*\)\s*\z/s, "\\1")

    block
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
    |> top_split(", ")
    |> Enum.all?(fn t ->
      t = t |> String.trim() |> String.trim("-")
      pure_dynamic?(t) or length(top_split(t, " or ")) == 1
    end)
  end

  defp pure_dynamic?("dynamic(" <> _ = t), do: top_split(t, " or ") == [t] and String.ends_with?(t, ")")
  defp pure_dynamic?(_), do: false

  # split at `sep` outside brackets
  def top_split(str, sep) do
    {parts, cur, _} =
      str
      |> String.graphemes()
      |> Enum.reduce({[], "", 0}, fn ch, {parts, cur, depth} ->
        depth = cond do
          ch in ["(", "[", "{"] -> depth + 1
          ch in [")", "]", "}"] -> depth - 1
          true -> depth
        end

        cur = cur <> ch

        if depth == 0 and String.ends_with?(cur, sep),
          do: {[String.replace_suffix(cur, sep, "") | parts], "", depth},
          else: {parts, cur, depth}
      end)

    Enum.reverse([cur | parts])
  end

  # Warnings that claim the expression always fails, by first line (format_diagnostic in
  # apply.ex, expr.ex, pattern.ex, of.ex). Not: comparisons, conditionals that always yield the
  # same value, redundant clauses (oracle 3).
  @fail_warnings [
    {~r/^incompatible types given to ([A-Z][\w.]*|:\w+)\.[^\s]+\/\d+/, :remote},
    {~r/^incompatible types given (to|as default arguments to) [a-z_]\w*[?!]?\/\d+/, "local"},
    {~r/^incompatible types given on /, "fnapply"},
    {~r/^expected a \d+-arity function on /, "fnapply"},
    {~r/^expected a tuple with at least /, "tupleindex"},
    {~r/^expected a non-negative integer as index /, "tupleindex"},
    {~r/^expected a map (within|with key)/, "mapupdate"},
    {~r/^incompatible types assigned to /, "assign"},
    {~r/^the following pattern will never match/, "match"},
    {~r/^(unknown key \.|expected a map or struct when accessing)/, "dot"},
    {~r/^incompatible types in binary/, "binary"},
    {~r/^expected a module/, "module"}
  ]

  def fail_area(msg) do
    first = msg |> String.trim_leading() |> String.split("\n", parts: 2) |> hd()

    # a key missing from the map: "this function will always return <default>/do nothing" claims
    # no failure; only "will always raise" (Map.fetch!) does
    if msg =~ "does not have all required keys" and not (msg =~ "therefore this function will always raise"),
      do: nil,
      else: fail_area(first, msg)
  end

  defp fail_area(first, _msg) do
    Enum.find_value(@fail_warnings, fn {re, area} ->
      case Regex.run(re, first) do
        nil -> nil
        [_, mod | _] when area == :remote -> "remote_" <> String.replace(mod, ~r/[^A-Za-z0-9_]/, "_")
        _ -> area
      end
    end)
  end

  defp hits, do: for({{:h, id}, _} <- Process.get(), into: MapSet.new(), do: id)
  defp clear_hits, do: for({{:h, _} = k, _} <- Process.get(), do: Process.delete(k))

  defp purge do
    :code.purge(CaseMod)
    :code.delete(CaseMod)
    :code.purge(CaseMod)
  end

  # The runtime oracles compare what the program does with what the checker inferred, so the BEAM
  # code must do what the source says. OTP 28.5's SSA optimiser miscompiles some generated cases
  # (alias analysis + destructive update of a setelement/3 on a returned literal tuple makes a
  # sibling function return {} instead of {[:a]}; reproduces with plain erlc, gone with
  # +no_ssa_opt). That is an OTP bug, not the type checker's, so the case module is compiled
  # without SSA optimisations. Injected on the defmodule line so source line numbers stay put.
  @no_opt "defmodule CaseMod do @compile :no_ssa_opt\n"

  defp compile(src) do
    src = String.replace(src, "defmodule CaseMod do\n", @no_opt, global: false)

    try do
      {res, diags} =
        Code.with_diagnostics([log: false], fn ->
          try do
            {:ok, Code.compile_string(src, "case.ex")}
          rescue
            e -> {:error, e, __STACKTRACE__}
          end
        end)

      nwarn = Enum.count(diags, &(&1.severity == :warning))

      # type checker claims that a clause never runs (pattern.ex format_diagnostic). The Erlang
      # compiler's "cannot match because a previous clause at line N always matches" is excluded.
      claims =
        for %{severity: :warning, message: msg, position: pos} <- diags,
            clause_claim?(msg),
            line = (case pos do
                      {l, _} -> l
                      l -> l
                    end),
            is_integer(line),
            do: {line, msg}

      fails =
        for %{severity: :warning, message: msg, position: pos} <- diags,
            area = fail_area(msg),
            line = (case pos do
                      {l, _} -> l
                      l -> l
                    end),
            is_integer(line),
            do: {line, area, msg}

      case res do
        {:ok, mods} ->
          case List.keyfind(mods, CaseMod, 0) do
            {CaseMod, bin} -> {:compiled, bin, nwarn, claims, fails}
            nil -> {:skip, "no_module", inspect(Enum.map(mods, &elem(&1, 0)))}
          end

        {:error, e, st} ->
          classify(e, st, diags)
      end
    rescue
      e -> classify(e, __STACKTRACE__, [])
    catch
      kind, reason -> {:skip, "compile_throw", inspect({kind, reason})}
    end
  end

  def classify(e, st, diags) do
    frame =
      Enum.find(st, fn
        {m, _, _, _} -> String.starts_with?(Atom.to_string(m), "Elixir.Module.Types")
        _ -> false
      end)

    case {e, frame} do
      {%CompileError{}, _} ->
        errs = for d <- diags, d.severity == :error, do: d.message
        {:skip, "compile_error", Enum.join(errs, "\n") <> Exception.message(e)}

      {%kind{}, {m, f, _, _}} when kind not in [SyntaxError, TokenMissingError, MismatchedDelimiterError] ->
        area = String.replace_prefix(Atom.to_string(m), "Elixir.Module.Types.", "") <> "." <> Atom.to_string(f)
        area = String.replace(area, ~r/[^A-Za-z0-9_.]/, "_")
        {:fail, "crash__exception__" <> area,
         Exception.format(:error, e, st) |> String.slice(0, 3000)}

      _ ->
        {:skip, "compile_other", Exception.format(:error, e, st) |> String.slice(0, 1500)}
    end
  end

  def sigs(bin) do
    version = :elixir_erl.checker_version()

    case :beam_lib.chunks(bin, [~c"ExCk"]) do
      {:ok, {_, [{~c"ExCk", chunk}]}} ->
        case :erlang.binary_to_term(chunk) do
          {^version, %{exports: exports}} -> Map.new(exports)
          other -> raise "unexpected checker chunk #{inspect(elem(other, 0))}"
        end
    end
  end

  defp run_calls(bin, calls) do
    {:module, CaseMod} = :code.load_binary(CaseMod, ~c"case.ex", bin)
    sigs = sigs(bin)

    Process.put(:alg_types, Enum.uniq(for {_, %{sig: {:infer, _, cs}}} <- sigs, {as, r} <- cs, t <- [r | as], do: t))
    for t <- Process.get(:alg_types), do: H.Print.note(t, "inferred signature type")
    Process.put(:alg_values, [])

    calls
    |> Enum.with_index()
    |> Enum.reduce_while(%{"v" => "pass", "ok" => 0, "raised" => 0, "nosig" => 0}, fn {call, i}, acc ->
      %{"f" => f, "args" => args_src} = call
      {args, _} = Code.eval_string(args_src)
      f = String.to_atom(f)

      clear_hits()

      {res, aborted} =
        traced_aborts(fn ->
          try do
            {:ok, apply(CaseMod, f, args)}
          catch
            _, _ -> :raised
          end
        end)

      # probes hit by a call that returned: those clause bodies completed, unless a rescue/catch arm
      # ran (the driver marks each with {:h, :caught}): a probed body in a function called from a try
      # body may have raised and been rescued, so its line did not evaluate (hd([]) in f0, f1 calls f0
      # inside try ... catch). Such calls are counted, their hits only kept for posts.
      caught? = MapSet.member?(hits(), :caught)
      # Also unless some CaseMod function exited with an exception (exception_trace): this covers
      # cases generated before the driver emitted {:h, :caught} (their repro.sh sources lack it).
      acc = if aborted > 0 and match?({:ok, _}, res), do: bump(acc, "ok_aborted"), else: acc
      if match?({:ok, _}, res) and not caught? and aborted == 0, do: Process.put(:ok_hits, MapSet.union(Process.get(:ok_hits, MapSet.new()), hits()))
      Process.put(:all_hits, MapSet.union(Process.get(:all_hits, MapSet.new()), hits()))
      acc = if match?({:ok, _}, res) and caught?, do: bump(acc, "ok_caught"), else: acc

      case res do
        :raised ->
          {:cont, Map.update!(acc, "raised", &(&1 + 1))}

        {:ok, v} ->
          Process.put(:alg_values, [v | args ++ Process.get(:alg_values, [])])

          case Map.get(sigs, {f, length(args)}) do
            %{sig: {:infer, _domain, clauses}} ->
              case check_call(f, args, v, clauses) do
                {:ok, ret} ->
                  acc = Map.update!(acc, "ok", &(&1 + 1))
                  acc = bump(acc, if(H.Mem.precise?(v), do: "check_precise", else: "check_imprecise"))
                  all = Enum.reduce(clauses, none(), fn {_, r}, u -> opt_union(r, u) end)
                  acc = if Module.Types.Descr.upper_bound(all) == Module.Types.Descr.term(), do: bump(acc, "ret_term"), else: acc

                  # oracle 5 runs generated code again: its probe hits do not count
                  case check_funs("#{f}(#{Enum.map_join(args, ", ", &inspect/1)})", v, ret, 0) do
                    {:fail, sig, msg} -> {:halt, %{"v" => "fail", "sig" => sig, "msg" => msg, "call" => i}}
                    n5 -> {:cont, Map.update(acc, "funapps", n5, &(&1 + n5))}
                  end

                {:fail, sig, msg} ->
                  {:halt, %{"v" => "fail", "sig" => sig, "msg" => msg, "call" => i}}
              end

            _ ->
              {:cont, Map.update!(acc, "nosig", &(&1 + 1))}
          end
      end
    end)
  end

  defp bump(acc, k), do: Map.update(acc, k, 1, &(&1 + 1))

  # Runs fun with exception_trace on every CaseMod function (fn bodies included: they are local
  # functions of CaseMod); returns {fun's result, number of CaseMod functions that exited with an
  # exception}. exception_trace keeps a frame for tail calls too, so a raise in a remote callee is
  # seen at the CaseMod function that made the call.
  defp traced_aborts(fun) do
    if Process.get(:trace_aborts, true), do: traced_aborts1(fun), else: {fun.(), 0}
  end

  defp traced_aborts1(fun) do
    me = self()
    tracer = spawn(fn -> count_aborts(0) end)
    :erlang.trace_pattern({CaseMod, :_, :_}, [{:_, [], [{:exception_trace}]}], [:local])
    :erlang.trace(me, true, [:call, {:tracer, tracer}])

    res =
      try do
        fun.()
      after
        :erlang.trace(me, false, [:call])
        :erlang.trace_pattern({CaseMod, :_, :_}, false, [:local])
      end

    ref = :erlang.trace_delivered(me)
    receive do: ({:trace_delivered, ^me, ^ref} -> :ok)
    send(tracer, {:get, me})
    n = receive do: ({:aborts, ^tracer, n} -> n)
    {res, n}
  end

  defp count_aborts(n) do
    receive do
      {:trace, _, :exception_from, _, _} -> count_aborts(n + 1)
      {:trace, _, _, _} -> count_aborts(n)
      {:trace, _, _, _, _} -> count_aborts(n)
      {:get, pid} -> send(pid, {:aborts, self(), n})
    end
  end

  def check_call(f, args, v, clauses) do
    applicable =
      for {targs, ret} <- clauses,
          Enum.zip(args, targs) |> Enum.all?(fn {a, t} -> H.Mem.member?(a, t) end),
          do: ret

    call = "#{f}(#{Enum.map_join(args, ", ", &inspect/1)}) returned #{inspect(v)}"
    sigtext = Enum.map_join(clauses, "\n", fn {as, r} ->
      "  (" <> Enum.map_join(as, ", ", &to_quoted_string/1) <> ") -> " <> to_quoted_string(r)
    end)

    cond do
      # pipeline self-test only (shrinking, findings, repro.sh): never set in real runs
      System.get_env("H_FAKE_BUG") && is_map(v) && Map.has_key?(v, :b) ->
        {:fail, "fake__ret__map", "#{call}"}

      applicable == [] ->
        bad = Enum.find(args, hd(args), fn a -> not Enum.any?(clauses, fn {ts, _} -> Enum.all?(ts, &H.Mem.member?(a, &1)) end) end)
        {:fail, "sound__domain__" <> H.Mem.kind(bad),
         "#{call}, but no clause of the inferred signature accepts the arguments:\n#{sigtext}"}

      not H.Mem.member?(v, Enum.reduce(applicable, none(), &opt_union/2)) ->
        {:fail, "sound__ret__" <> H.Mem.kind(v),
         "#{call}, which is not in the inferred return type:\n#{sigtext}"}

      true ->
        {:ok, Enum.reduce(applicable, none(), &opt_union/2)}
    end
  end

  # Oracle 5: a function value inside a returned value (fn literal, local, remote or stdlib
  # capture) is applied to a fixed universe of arguments; when it returns, the result must be in
  # fun_apply(F, argument types), F the arrows of the value's inferred type (upper bound). A function
  # of type (a -> b) applied to a value of a returns a value of b: soundness,
  # gradual-set-theoretic-types.md:10. Arguments and F are static, so fun_apply's gradual cases
  # (#16094) do not enter. Returns the number of checked applications or {:fail, sig, msg}.
  @universe [:a, :ok, :error, nil, true, 0, 1, -1, 1.0, "k", [], [1], {}, {:ok, 1}, {1, 2}, %{}, %{a: 1},
             %{a: :b, b: 0}, %{1 => 2}, [:a | :b]]
  @universe2 [:a, nil, true, 0, 1.0, "k", [], [1], {:ok, 1}, %{}, %{a: 1}, %{1 => 2}]

  def check_funs(path, v, t, depth) when is_function(v) do
    {:arity, arity} = :erlang.fun_info(v, :arity)
    alias Module.Types.Descr, as: D
    f = D.opt_intersection(D.upper_bound(t), D.fun(arity))

    if arity > 2 or D.empty?(f) do
      0
    else
      argss =
        case arity do
          0 -> [[]]
          1 -> for a <- Enum.uniq(@universe ++ Process.get(:funargs1, [])), do: [a]
          2 ->
            u = Enum.uniq(@universe2 ++ Enum.take(Process.get(:funargs1, []), 8))
            Enum.uniq(Process.get(:funargs2, []) ++ for(a <- u, b <- u, do: [a, b]))
        end

      Enum.reduce_while(argss, 0, fn args, n ->
        with {:ok, r} <- (try do
                            {:ok, apply(v, args)}
                          catch
                            _, _ -> :raised
                          end),
             {:ok, rt} <- D.fun_apply(f, Enum.map(args, &H.Mem.of/1)) do
          if d = System.get_env("H_DUMP_FUN"),
            do: File.write!(d, "#{D.to_quoted_string(f)} | #{inspect(args)} -> #{inspect(r)} in #{D.to_quoted_string(rt)}\n", [:append])

          if H.Mem.member?(r, rt) do
            if D.upper_bound(rt) == D.term(), do: Process.put(:funapps_term, Process.get(:funapps_term, 0) + 1)
            {:cont, n + 1}
          else
            msg =
              "#{path} returned a function of inferred type\n  #{D.to_quoted_string(f)}\n" <>
                "applied to #{Enum.map_join(args, ", ", &inspect/1)} it returned #{inspect(r)}, " <>
                "outside fun_apply(type, args) = #{D.to_quoted_string(rt)}"

            {:halt, {:fail, "sound__funapp__" <> fun_origin(v), msg}}
          end
        else
          _ -> {:cont, n}
        end
      end)
    end
  end

  def check_funs(path, v, t, depth) when is_tuple(v) and depth < 3 do
    v
    |> Tuple.to_list()
    |> Enum.with_index()
    |> Enum.reduce_while(0, fn {x, i}, n ->
      case Module.Types.Descr.tuple_fetch(Module.Types.Descr.upper_bound(t), i) do
        {_, ti} ->
          case check_funs("elem(#{path}, #{i})", x, ti, depth + 1) do
            {:fail, _, _} = fail -> {:halt, fail}
            m -> {:cont, n + m}
          end

        _ ->
          {:cont, n}
      end
    end)
  end

  def check_funs(path, v, t, depth) when is_map(v) and depth < 3 do
    Enum.reduce_while(Map.to_list(v), 0, fn
      {k, x}, n when is_atom(k) ->
        case Module.Types.Descr.map_fetch_key(Module.Types.Descr.upper_bound(t), k) do
          {_, tk} ->
            case check_funs("#{path}.#{k}", x, tk, depth + 1) do
              {:fail, _, _} = fail -> {:halt, fail}
              m -> {:cont, n + m}
            end

          _ ->
            {:cont, n}
        end

      _, n ->
        {:cont, n}
    end)
  end

  def check_funs(_path, _v, _t, _depth), do: 0

  # fn literal / local capture vs remote capture (&Mod.fun/n)
  defp fun_origin(v) do
    case :erlang.fun_info(v, :type) do
      {:type, :external} -> "remote"
      _ -> if :erlang.fun_info(v, :module) == {:module, CaseMod}, do: "local", else: "other"
    end
  end
end

defmodule H.Head do
  # Oracle 8 (head__<domain|ret|clause>__<area>): clause heads in isolation. Every group of clause
  # heads in the case (the clauses of a def, of a case/receive/with-else/try-else, of a fn, and each
  # `pat <- expr` clause of a with) is lifted into a function of a separate module CaseHead:
  #
  #     def hK(pats..., free...) when guard, do: {i, {bound vars of clause i}}   (one line per clause)
  #     def hK(a1, ..., an), do: {:none, {a1, ..., an}}                         (catch-all)
  #
  # `free` are the variables the guards use but the patterns do not bind (outer variables of a case
  # arm), passed as plain parameters. Every call of hK returns (the catch-all accepts everything; a
  # guard that raises just fails), so each application is evidence. The functions are applied to
  # values instantiated from the clause patterns (seeded by the group's source), near-miss mutants of
  # them, a fixed universe and the case's call arguments. Then, like oracle 1, against the inferred
  # signature of hK (soundness, gradual-set-theoretic-types.md:10, "the inferred and assigned by the
  # type system align with the behaviour of the program"):
  #   - some clause's argument types accept the arguments (head__domain__<area>);
  #   - the result, which holds the variables the pattern bound, is in the return type of the clauses
  #     that accept the arguments (head__ret__<area>): the types of pattern variables after refinement
  #     by the pattern and guard, and, through the catch-all, the types left after the previous
  #     clauses (what the checker subtracts as precise);
  #   - no clause the checker warns never matches / is redundant returned (head__clause__<area>, contract
  #     :236/:244 as for oracle 3; bodies here never raise).
  # Also, per group, @skels functions `sK_J(l1, .., free..) do case SUBJ do <the heads as arms>; h_rest ->
  # {:none, h_rest} end end`, SUBJ the structure of an instantiated value with some leaves literal and the
  # others parameters (headsub__<ret|clause>__<area>): the arms see a precise structural expected type.
  # area = what the clause that ran looks like (pattern construct, `guard` when a bare variable or
  # literal pattern has a guard); for the catch-all `rest_` + the construct of the clauses before it.
  alias Module.Types.Descr, as: D

  @max_groups 12
  @per_clause 5
  @max_args 40
  @skels 2
  @skel_calls 6
  @pool [:a, :b, :ok, :error, nil, true, false, 0, 1, -1, 2, 2.0, 0.5, 1.0, "k", "", [], [1], [:a | :b],
         {}, {:ok, 1}, {:error, :a}, {1, 2}, %{}, %{a: 1}, %{b: nil}, %{a: :b, b: 0}, %{1 => 2}, %{"k" => 1.0},
         <<1, 2, 3>>, <<1::1>>, [%{a: 1}], {%{}, []}]

  def check(src, call_args) do
    with {:ok, ast} <- Code.string_to_quoted(src),
         groups when groups != [] <- groups(ast) do
      entries =
        groups
        |> Enum.take(@max_groups)
        |> Enum.with_index()
        |> Enum.flat_map(fn {g, k} -> entries(g, k, call_args) end)

      {hsrc, entries} = render(entries)
      run(hsrc, entries, length(groups))
    else
      _ -> %{"head_groups" => 0}
    end
  end

  ## Collecting clause groups: [{origin, [{pats, guard}]}]

  defp groups({:defmodule, _, [_, [do: body]]}) do
    defs =
      for {kind, _, [head | _]} <- block(body), kind in [:def, :defp] do
        {name, args, guard} =
          case head do
            {:when, _, [{name, _, args}, guard]} -> {name, args, guard}
            {name, _, args} -> {name, args, nil}
          end

        {{name, length(List.wrap(args))}, {List.wrap(args), guard}}
      end

    def_groups =
      defs
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Enum.sort()
      |> Enum.map(fn {_, cs} -> {"def", cs} end)

    {_, inner} = Macro.prewalk(body, [], &collect/2)
    Enum.filter(def_groups ++ Enum.reverse(inner), &liftable?/1)
  end

  defp groups(_), do: []

  defp block({:__block__, _, xs}), do: xs
  defp block(x), do: [x]

  defp collect({:case, _, [_, kw]} = n, acc) when is_list(kw), do: {n, arrows("case", kw[:do], acc)}
  defp collect({:receive, _, [kw]} = n, acc) when is_list(kw), do: {n, arrows("receive", kw[:do], acc)}
  defp collect({:try, _, [kw]} = n, acc) when is_list(kw), do: {n, arrows("else", kw[:else], acc)}
  defp collect({:fn, _, cs} = n, acc), do: {n, arrows("fn", cs, acc)}

  defp collect({:with, _, args} = n, acc) when is_list(args) do
    {clauses, kw} =
      case List.last(args) do
        kw when is_list(kw) -> {Enum.drop(args, -1), kw}
        _ -> {args, []}
      end

    acc =
      Enum.reduce(clauses, acc, fn
        {:<-, _, [pat, _]}, acc -> [{"with", [split_when([pat])]} | acc]
        _, acc -> acc
      end)

    {n, arrows("else", kw[:else], acc)}
  end

  defp collect(n, acc), do: {n, acc}

  defp arrows(origin, cs, acc) when is_list(cs) and cs != [] do
    if Enum.all?(cs, &match?({:->, _, [_, _]}, &1)),
      do: [{origin, for({:->, _, [pats, _]} <- cs, do: split_when(pats))} | acc],
      else: acc
  end

  defp arrows(_, _, acc), do: acc

  defp split_when([{:when, _, args}]) do
    {pats, [guard]} = Enum.split(args, -1)
    {pats, guard}
  end

  defp split_when(pats), do: {pats, nil}

  # Same arity everywhere; no pins, no defaults, no size() over a variable the pattern does not bind
  defp liftable?({_, [{pats, _} | _] = cs}) do
    n = length(pats)

    n in 1..3 and
      Enum.all?(cs, fn {ps, g} ->
        length(ps) == n and not has?(ps, [:^, :\\]) and not has?(g, [:^]) and
          MapSet.subset?(size_vars(ps), pat_vars(ps))
      end)
  end

  defp has?(ast, ops) do
    {_, found} = Macro.prewalk(ast, false, fn
      {op, _, _} = x, acc when is_atom(op) -> {x, acc or op in ops}
      x, acc -> {x, acc}
    end)

    found
  end

  defp var?({name, _, ctx}) when is_atom(name) and is_atom(ctx), do: true
  defp var?(_), do: false

  defp named?(name), do: not String.starts_with?(Atom.to_string(name), "_")

  # variables a pattern binds (not the specifiers of bitstring segments)
  def pat_vars(ast) do
    {_, vs} =
      Macro.prewalk(ast, MapSet.new(), fn
        {:"::", m, [l, _]}, acc -> {{:"::", m, [l, nil]}, acc}
        {name, _, ctx} = v, acc when is_atom(name) and is_atom(ctx) ->
          {v, if(named?(name), do: MapSet.put(acc, name), else: acc)}
        x, acc -> {x, acc}
      end)

    vs
  end

  defp size_vars(ast) do
    {_, vs} =
      Macro.prewalk(ast, MapSet.new(), fn
        {:"::", _, [_, r]} = x, acc ->
          {_, inner} =
            Macro.prewalk(r, acc, fn
              {:size, _, [arg]} = s, a -> {s, MapSet.union(a, all_vars(arg))}
              s, a -> {s, a}
            end)

          {x, inner}

        x, acc ->
          {x, acc}
      end)

    vs
  end

  defp all_vars(ast) do
    {_, vs} =
      Macro.prewalk(ast, MapSet.new(), fn
        {name, _, ctx} = v, acc when is_atom(name) and is_atom(ctx) -> {v, MapSet.put(acc, name)}
        x, acc -> {x, acc}
      end)

    vs
  end

  ## Entries: one lifted def per group, plus @skels functions matching a typed subject

  defp entries({origin, cs}, k, call_args) do
    key = Enum.map(cs, fn {ps, g} -> Macro.to_string(ps) <> " when " <> Macro.to_string(g) end)
    :rand.seed(:exsss, {:erlang.phash2(key), 3, 5})
    bound = Enum.map(cs, fn {ps, _} -> pat_vars(ps) end)

    free =
      cs
      |> Enum.zip(bound)
      |> Enum.reduce(MapSet.new(), fn {{_, g}, b}, acc -> MapSet.union(acc, MapSet.difference(all_vars(g || []), b)) end)
      |> Enum.filter(&named?/1)
      |> Enum.sort()

    npat = length(elem(hd(cs), 0))

    inst =
      for {ps, _} <- cs, _ <- 1..@per_clause do
        {vals, _} = Enum.map_reduce(ps, %{}, &inst/2)
        vals
      end

    mutants = for vals <- inst, do: Enum.map(vals, &mutate/1)
    fromcalls = for a <- call_args, length(a) == npat, do: a
    pool = for _ <- 1..6, do: for(_ <- 1..npat, do: pick())
    frees = fn -> for _ <- free, do: free_value(inst) end

    argss =
      (inst ++ fromcalls ++ mutants ++ pool)
      |> Enum.uniq()
      |> Enum.take(@max_args)
      |> Enum.map(&(&1 ++ frees.()))

    base = %{origin: origin, cs: cs, bound: bound, free: free, npat: npat}
    lift = Map.merge(base, %{kind: :lift, name: "h#{k}", argss: argss})

    skels =
      for j <- 1..@skels//1 do
        vals = Enum.random(inst ++ mutants)
        {subs, leaves} = Enum.map_reduce(vals, [], &skel(&1, 3, &2))
        leaves = Enum.reverse(leaves)
        subj = if npat == 1, do: hd(subs), else: "{" <> Enum.join(subs, ", ") <> "}"
        calls = [leaves | for(_ <- 1..@skel_calls, do: Enum.map(leaves, &leaf_variant/1))]
        calls = calls |> Enum.uniq() |> Enum.map(&(&1 ++ frees.()))
        # some leaves get a type guard from their value (dynamic(integer()) etc. instead of dynamic())
        lguards =
          for {v, i} <- Enum.with_index(leaves, 1), :rand.uniform(10) <= 3, g = kind_guard(v), do: "#{g}(l#{i})"

        Map.merge(base, %{kind: :skel, name: "s#{k}_#{j}", subj: subj, nleaves: length(leaves), argss: calls, lguards: lguards})
      end

    [lift | skels]
  end

  # the value's structure as an expression: some leaves literal, the others parameters l1, l2, ...
  # (dynamic()), so the case subject has a precise structural type, as in `case [v0, 2] do`
  defp skel(v, d, acc) do
    r = :rand.uniform(10)

    cond do
      d > 0 and r <= 7 and is_tuple(v) ->
        {subs, acc} = Enum.map_reduce(Tuple.to_list(v), acc, &skel(&1, d - 1, &2))
        {"{" <> Enum.join(subs, ", ") <> "}", acc}

      d > 0 and r <= 7 and is_list(v) and v != [] and not List.improper?(v) ->
        {subs, acc} = Enum.map_reduce(v, acc, &skel(&1, d - 1, &2))
        {"[" <> Enum.join(subs, ", ") <> "]", acc}

      d > 0 and r <= 7 and is_struct(v) ->
        {subs, acc} =
          Enum.map_reduce(Map.to_list(Map.from_struct(v)), acc, fn {f, x}, acc ->
            {sx, acc} = skel(x, d - 1, acc)
            {"#{f}: #{sx}", acc}
          end)

        {"%" <> inspect(v.__struct__) <> "{" <> Enum.join(subs, ", ") <> "}", acc}

      d > 0 and r <= 7 and is_map(v) ->
        {subs, acc} =
          Enum.map_reduce(Map.to_list(v), acc, fn {key, x}, acc ->
            {sx, acc} = skel(x, d - 1, acc)
            {inspect(key) <> " => " <> sx, acc}
          end)

        {"%{" <> Enum.join(subs, ", ") <> "}", acc}

      r <= 4 ->
        {inspect(v, limit: :infinity, printable_limit: :infinity), acc}

      true ->
        {"l#{length(acc) + 1}", [v | acc]}
    end
  end

  defp kind_guard(v) when is_atom(v), do: "is_atom"
  defp kind_guard(v) when is_integer(v), do: "is_integer"
  defp kind_guard(v) when is_float(v), do: "is_float"
  defp kind_guard(v) when is_binary(v), do: "is_binary"
  defp kind_guard(v) when is_list(v), do: "is_list"
  defp kind_guard(v) when is_tuple(v), do: "is_tuple"
  defp kind_guard(v) when is_map(v), do: "is_map"
  defp kind_guard(_), do: nil

  defp lguard_src([]), do: ""
  defp lguard_src(gs), do: " when " <> Enum.join(gs, " and ")

  defp leaf_variant(v) do
    case :rand.uniform(10) do
      n when n <= 2 -> pick()
      n when n <= 4 -> mutate(v)
      _ -> v
    end
  end

  ## Rendering CaseHead (every clause head on its own line)

  defp render(entries) do
    {lines, entries, _} =
      Enum.reduce(entries, {["defmodule CaseHead do"], [], 2}, fn e, {lines, acc, line} ->
        {elines, first} = render_entry(e, line)
        # Macro.to_string wraps long patterns/guards; one source line per clause keeps warning lines exact
        elines = Enum.map(elines, &String.replace(&1, ~r/\n\s*/, " "))
        e = Map.merge(e, %{first: first, nlines: length(elines), line: line, arity: arity(e)})
        {lines ++ elines, [e | acc], line + length(elines)}
      end)

    {Enum.join(lines ++ ["end", ""], "\n"), Enum.reverse(entries)}
  end

  defp arity(%{kind: :lift} = e), do: e.npat + length(e.free)
  defp arity(%{kind: :skel} = e), do: e.nleaves + length(e.free)

  defp vars_tuple(b), do: "{" <> (b |> Enum.sort() |> Enum.map_join(", ", &Atom.to_string/1)) <> "}"
  defp guard_src(nil), do: ""
  defp guard_src(g), do: " when " <> Macro.to_string(g)

  defp render_entry(%{kind: :lift} = e, line) do
    clause_lines =
      e.cs
      |> Enum.zip(e.bound)
      |> Enum.with_index()
      |> Enum.map(fn {{{ps, g}, b}, i} ->
        # a free variable the clause binds itself is the clause's own (`_` in its parameter slot)
        fargs = Enum.map(e.free, &if(MapSet.member?(b, &1), do: Macro.var(:_, nil), else: Macro.var(&1, nil)))
        args = Enum.map_join(ps ++ fargs, ", ", &Macro.to_string/1)
        "  def #{e.name}(#{args})#{guard_src(g)}, do: {#{i}, #{vars_tuple(b)}}"
      end)

    params = Enum.map_join(1..arity(e)//1, ", ", &"a#{&1}")
    {clause_lines ++ ["  def #{e.name}(#{params}), do: {:none, {#{params}}}"], line}
  end

  defp render_entry(%{kind: :skel} = e, line) do
    params = Enum.map(1..e.nleaves//1, &"l#{&1}") ++ Enum.map(e.free, &Atom.to_string/1)

    arms =
      e.cs
      |> Enum.zip(e.bound)
      |> Enum.with_index()
      |> Enum.map(fn {{{ps, g}, b}, i} ->
        pat = if e.npat == 1, do: Macro.to_string(hd(ps)), else: "{" <> Enum.map_join(ps, ", ", &Macro.to_string/1) <> "}"
        "      #{pat}#{guard_src(g)} -> {#{i}, #{vars_tuple(b)}}"
      end)

    lines =
      ["  def #{e.name}(#{Enum.join(params, ", ")})#{lguard_src(e.lguards)} do", "    case #{e.subj} do"] ++
        arms ++ ["      h_rest -> {:none, h_rest}", "    end", "  end"]

    {lines, line + 2}
  end

  ## Running

  defp run(hsrc, entries, ngroups) do
    purge()

    task = Task.async(fn -> compile(hsrc) end)

    result =
      case Task.yield(task, 30_000) || Task.shutdown(task, :brutal_kill) do
        {:ok, res} -> res
        nil -> {:fail, "hang__compile__typecheck", "compiling the clause-head module took more than 30 s\n" <> hsrc}
        {:exit, reason} -> {:skip, inspect(reason)}
      end

    out =
      case result do
        {:fail, sig, msg} ->
          %{"v" => "fail", "sig" => sig, "msg" => "while compiling the lifted clause heads (oracle 8)\n" <> msg <> "\n" <> hsrc}

        {:skip, _} ->
          %{"head_skip" => 1}

        {:compiled, bin, claims} ->
          {:module, CaseHead} = :code.load_binary(CaseHead, ~c"case_head.ex", bin)
          sigs = H.Worker.sigs(bin)
          src_lines = String.split(hsrc, "\n")
          init = %{"head_groups" => ngroups, "head_calls" => 0, "headsub_calls" => 0}

          Enum.reduce_while(entries, init, fn e, acc ->
            counter = if e.kind == :lift, do: "head_calls", else: "headsub_calls"

            case check_entry(e, sigs, claims, src_lines) do
              {:ok, n} -> {:cont, Map.update!(acc, counter, &(&1 + n))}
              {:fail, sig, msg} -> {:halt, %{"v" => "fail", "sig" => sig, "msg" => msg}}
            end
          end)
      end

    purge()
    out
  end

  defp purge do
    :code.purge(CaseHead)
    :code.delete(CaseHead)
    :code.purge(CaseHead)
  end

  defp compile(src) do
    {res, diags} =
      Code.with_diagnostics([log: false], fn ->
        try do
          {:ok, Code.compile_string(src, "case_head.ex")}
        rescue
          e -> {:error, e, __STACKTRACE__}
        end
      end)

    claims =
      for %{severity: :warning, message: msg, position: pos} <- diags,
          H.Worker.clause_claim?(msg),
          line = (case pos do
                    {l, _} -> l
                    l -> l
                  end),
          is_integer(line),
          do: {line, msg}

    case res do
      {:ok, mods} ->
        case List.keyfind(mods, CaseHead, 0) do
          {CaseHead, bin} -> {:compiled, bin, claims}
          nil -> {:skip, :no_module}
        end

      {:error, e, st} ->
        case H.Worker.classify(e, st, diags) do
          {:fail, _, _} = f -> f
          _ -> {:skip, :compile}
        end
    end
  end

  defp check_entry(e, sigs, claims, src_lines) do
    f = String.to_atom(e.name)
    oracle = if e.kind == :lift, do: "head", else: "headsub"
    what = if e.kind == :lift, do: "as def clauses", else: "as case arms on a typed subject"

    ctx =
      "CaseHead (oracle 8: the clause heads of a #{e.origin} #{what}):\n" <>
        Enum.join(Enum.slice(src_lines, e.line - 1, e.nlines), "\n")

    aligned = String.starts_with?(Enum.at(src_lines, e.line - 1, ""), "  def #{e.name}(")

    case aligned && Map.get(sigs, {f, e.arity}) do
      %{sig: {:infer, _, sclauses}} ->
        Enum.reduce_while(e.argss, {:ok, 0}, fn args, {:ok, n} ->
          # only a skeleton's leaf guards can reject arguments (FunctionClauseError: not evidence)
          v = try do
            apply(CaseHead, f, args)
          rescue
            FunctionClauseError -> :rejected
          end
          if v == :rejected, do: {:cont, {:ok, n}}, else: check_result(e, f, args, v, n, oracle, claims, sclauses, ctx)
        end)

      _ ->
        {:ok, 0}
    end
  end

  defp check_result(e, f, args, v, n, oracle, claims, sclauses, ctx) do
    i = elem(v, 0)
    area = area(e.cs, i)
    cline = if i == :none, do: e.first + length(e.cs), else: e.first + i
    claim = Enum.find(claims, fn {line, _} -> line == cline end)
    call = "#{e.name}(#{Enum.map_join(args, ", ", &inspect/1)}) returned #{inspect(v)}"

    if claim do
      {:halt,
       {:fail, oracle <> "__clause__" <> area,
        "#{call} (clause #{inspect(i)} ran), but the checker warns it never runs:\n#{elem(claim, 1)}\n#{ctx}"}}
    else
      case H.Worker.check_call(f, args, v, sclauses) do
        {:ok, _} ->
          {:cont, {:ok, n + 1}}

        {:fail, "sound__" <> rest, msg} ->
          [kind | _] = String.split(rest, "__")
          {:halt, {:fail, "#{oracle}__#{kind}__#{area}", msg <> "\n" <> ctx}}

        {:fail, sig, msg} ->
          {:halt, {:fail, sig, msg <> "\n" <> ctx}}
      end
    end
  end

  # pattern construct of clause i (the catch-all: of the clauses before it)
  defp area(cs, :none), do: "rest_" <> (cs |> Enum.map(&construct/1) |> Enum.min_by(&rank/1))
  defp area(cs, i), do: construct(Enum.at(cs, i))

  @ranks ~w(bitstring struct map cons list alias tuple guard literal var)
  defp rank(c), do: Enum.find_index(@ranks, &(&1 == c))

  defp construct({ps, g}) do
    case ps |> Enum.flat_map(&cs_of/1) |> Enum.sort_by(&rank/1) do
      [c | _] -> c
      [] -> if(g, do: "guard", else: if(Enum.all?(ps, &var?/1), do: "var", else: "literal"))
    end
  end

  defp cs_of({:<<>>, _, _}), do: ["bitstring"]
  defp cs_of({:%, _, [_, {:%{}, _, kvs}]}), do: ["struct" | Enum.flat_map(kvs, &cs_of(elem(&1, 1)))]
  defp cs_of({:%{}, _, kvs}), do: ["map" | Enum.flat_map(kvs, &cs_of(elem(&1, 1)))]
  defp cs_of({:=, _, [l, r]}), do: ["alias" | cs_of(l) ++ cs_of(r)]
  defp cs_of({:{}, _, xs}), do: ["tuple" | Enum.flat_map(xs, &cs_of/1)]
  defp cs_of({a, b}), do: ["tuple" | cs_of(a) ++ cs_of(b)]
  defp cs_of([]), do: []

  defp cs_of(xs) when is_list(xs) do
    case List.last(xs) do
      {:|, _, [h, t]} -> ["cons" | Enum.flat_map(Enum.drop(xs, -1) ++ [h, t], &cs_of/1)]
      _ -> ["list" | Enum.flat_map(xs, &cs_of/1)]
    end
  end

  defp cs_of(_), do: []

  ## Values

  defp pick, do: Enum.random(@pool)

  # free (outer) variables: mostly booleans and values like the patterns' (guards compare them)
  defp free_value(inst) do
    case :rand.uniform(4) do
      1 -> Enum.random([true, false])
      2 when inst != [] and hd(inst) != [] -> inst |> Enum.random() |> Enum.random()
      _ -> pick()
    end
  end

  # a value that matches the pattern if it is satisfiable (bindings keep repeated variables equal)
  defp inst({:_, _, ctx}, b) when is_atom(ctx), do: {pick(), b}

  defp inst({name, _, ctx}, b) when is_atom(name) and is_atom(ctx) do
    case b do
      %{^name => v} -> {v, b}
      _ -> v = pick(); {v, Map.put(b, name, v)}
    end
  end

  defp inst({:=, _, [l, r]}, b) do
    cond do
      var?(l) -> {v, b} = inst(r, b); bind(l, v, b)
      true -> {v, b} = inst(l, b); if(var?(r), do: bind(r, v, b), else: {v, b})
    end
  end

  defp inst({:-, _, [n]}, b) when is_number(n), do: {-n, b}
  defp inst({a, c}, b), do: (with {[x, y], b} <- Enum.map_reduce([a, c], b, &inst/2), do: {{x, y}, b})
  defp inst({:{}, _, xs}, b), do: (with {vs, b} <- Enum.map_reduce(xs, b, &inst/2), do: {List.to_tuple(vs), b})
  defp inst({:%{}, _, kvs}, b), do: inst_map(kvs, %{}, b)

  defp inst({:%, _, [{:__aliases__, _, [s]}, {:%{}, _, kvs}]}, b) when s in [:CaseS, :CaseT] do
    base = if s == :CaseS, do: %CaseS{}, else: %CaseT{}
    inst_map(kvs, base, b)
  end

  defp inst({:%, _, [_, {:%{}, _, kvs}]}, b), do: inst_map(kvs, Enum.random([%CaseS{}, %CaseT{}]), b)

  defp inst(list, b) when is_list(list) do
    case List.last(list) do
      {:|, _, [h, t]} ->
        {hs, b} = Enum.map_reduce(Enum.drop(list, -1) ++ [h], b, &inst/2)
        {tl, b} = inst(t, b)
        {hs ++ tl, b}

      _ ->
        Enum.map_reduce(list, b, &inst/2)
    end
  rescue
    _ -> {pick(), b}
  end

  defp inst({:<>, _, [l, r]}, b) when is_binary(l) do
    {v, b} = inst(r, b)
    {l <> if(is_binary(v), do: v, else: ""), b}
  end

  defp inst({:<<>>, _, segs}, b) do
    {parts, b} = Enum.map_reduce(segs, b, &seg/2)
    {Enum.reduce(parts, <<>>, fn p, acc -> <<acc::bitstring, p::bitstring>> end), b}
  end

  defp inst(lit, b) when is_atom(lit) or is_number(lit) or is_binary(lit), do: {lit, b}
  defp inst(_, b), do: {pick(), b}

  defp bind({:_, _, _}, v, b), do: {v, b}
  defp bind({name, _, _}, v, b), do: {v, Map.put(b, name, v)}

  defp inst_map(kvs, base, b) do
    {m, b} =
      Enum.reduce(kvs, {base, b}, fn {k, p}, {m, b} ->
        {kv, _} = inst(k, %{})
        {v, b} = inst(p, b)
        {Map.put(m, kv, v), b}
      end)

    # sometimes an extra key (map patterns are open)
    if is_map_key(m, :__struct__) or :rand.uniform(3) > 1, do: {m, b}, else: {Map.put(m, Enum.random([:c, 1, "k"]), pick()), b}
  end

  # one bitstring segment: literal or variable with a type specifier
  defp seg({:"::", _, [l, spec]}, b) do
    names = spec_names(spec)

    v =
      cond do
        "float" in names -> <<:rand.uniform() * 4::float>>
        "utf8" in names -> <<Enum.random([?a, 233, 8364])::utf8>>
        "binary" in names or "bytes" in names ->
          case size_of(spec) do
            n when is_integer(n) -> :binary.copy("a", n)
            _ -> Enum.random(["", "a", "ab", <<1, 2, 3>>])
          end
        "bits" in names or "bitstring" in names -> Enum.random(["", <<1::1>>, "a"])
        true ->
          bits = case size_of(spec) do
            n when is_integer(n) -> n
            _ -> 8
          end
          lit = if is_integer(l), do: l, else: :rand.uniform(255) - 1
          <<lit::size(bits)>>
      end

    _ = l
    {v, b}
  end

  defp seg(x, b) when is_integer(x), do: {<<x>>, b}
  defp seg(x, b) when is_binary(x), do: {x, b}
  defp seg(_, b), do: {<<:rand.uniform(255)>>, b}

  defp spec_names(spec) do
    {_, ns} = Macro.prewalk(spec, [], fn
      {name, _, ctx} = x, acc when is_atom(name) and (is_atom(ctx) or ctx == []) -> {x, [Atom.to_string(name) | acc]}
      x, acc -> {x, acc}
    end)

    ns
  end

  defp size_of(spec) do
    {_, s} = Macro.prewalk(spec, nil, fn
      {:size, _, [n]} = x, _ when is_integer(n) -> {x, n}
      x, acc -> {x, acc}
    end)

    s
  end

  # near miss: one random place replaced
  defp mutate(v) do
    case :rand.uniform(5) do
      1 -> pick()
      _ -> mutate_in(v)
    end
  end

  defp mutate_in(v) when is_tuple(v) and tuple_size(v) > 0 do
    i = :rand.uniform(tuple_size(v)) - 1
    put_elem(v, i, mutate(elem(v, i)))
  end

  defp mutate_in(v) when is_map(v) and map_size(v) > 0 do
    k = Enum.random(Map.keys(v))

    case :rand.uniform(3) do
      1 -> if(is_map_key(v, :__struct__), do: v, else: Map.delete(v, k))
      _ -> if(k == :__struct__, do: v, else: Map.put(v, k, mutate(Map.fetch!(v, k))))
    end
  end

  defp mutate_in([h | t]), do: if(:rand.uniform(2) == 1, do: [mutate(h) | t], else: [h | mutate_tail(t)])
  defp mutate_in(v), do: pick() |> then(&if(&1 == v, do: :b, else: &1))

  defp mutate_tail([]), do: Enum.random([[pick()], []])
  defp mutate_tail(t) when is_list(t), do: mutate_in(t)
  defp mutate_tail(_), do: []
end

defmodule H.Expr do
  # Oracle 9 (expr__<ret|warn|clause>__<op>): single expressions typed in isolation, with variables of
  # chosen types. Each trial picks an operation template (stdlib/erlang calls with hand-written or
  # inferred signatures, tuple/map/list syntax, case/fn/if/cond/with over the variables, a random export
  # of a pure stdlib module), fills its holes with literals or variables x1..xn, gives each variable a
  # runtime value v and a type T that contains v (H.Mem.of(v), v's type widened at random places, a
  # union with another type, any of these wrapped in dynamic(), or dynamic()), types the expression as
  # the body of `fn x1, .., xn -> body end` with Pattern.of_head(expected = Ts) + Expr.of_expr (what
  # type_helper.exs does, in :dynamic or :static mode), then evaluates it with the values bound.
  # Programs rarely hand the checker such types (parameters are dynamic(), literals exact), so the
  # static and gradual application paths of apply.ex and the stdlib's inferred signatures are judged
  # on argument types the module oracles never produce. When the evaluation returns r:
  #   - ret:    no error was reported, so r must be in the upper bound of the expression's type
  #             (soundness, gradual-set-theoretic-types.md:10);
  #   - warn:   a warning that claims certain failure (H.Worker.fail_area/certain?) where every
  #             variable's type is purely dynamic(...) or the exact type of its value and the claim
  #             is about all values (:236 "all combinations of a type _will_ fail", :244);
  #   - clause: the clause that ran (each case/fn template clause returns {i, ..}) was reported as
  #             redundant / never matching (:236/:244).
  # The trials depend only on the case's eseed (kept by the shrinker), so repro.sh is stable. Struct
  # update syntax and `for` (documented false positives) are only checked for ret.
  alias Module.Types.{Pattern, Expr, Descr}
  import Descr, only: [dynamic: 0, dynamic: 1, term: 0, opt_union: 2, to_quoted_string: 1, upper_bound: 1]

  @trials 12
  @tc_ms 10_000
  @atoms [:a, :b, :ok, :error, nil, true, false]
  @ints [0, 1, -1, 2, 3]
  @keys [:a, :b, :c, 1, "k", 2.0]
  @fun1 ["fn z -> z end", "fn z -> {z} end", "&is_atom/1", "fn {a, _} -> a end", "fn z -> [z] end",
         "&is_integer/1", "fn z when is_integer(z) -> z + 1; z -> z end", "fn _ -> :ok end"]
  @fun2 ["fn a, b -> {a, b} end", "&max/2", "fn _, b -> b end", "fn a, _ -> a end"]
  @fun0 ["fn -> :d end", "fn -> 0 end", "fn -> %{} end"]
  @std [List, Map, Keyword, Tuple, String, Integer, Float, Atom, Enum, MapSet, Range, Access, Bitwise,
        Base, URI, Version, Function]
  @std_skip [random: 1, shuffle: 1, take_random: 2, random: 2]

  # {name, holes, format}; $1.. are the holes
  @ops [
    {"elem", [:tuple, :idx], "elem($1, $2)"},
    {"put_elem", [:tuple, :idx, :any], "put_elem($1, $2, $3)"},
    {"tuple_size", [:tuple], "tuple_size($1)"},
    {"erlang.element", [:idx1, :tuple], ":erlang.element($1, $2)"},
    {"erlang.setelement", [:idx1, :tuple, :any], ":erlang.setelement($1, $2, $3)"},
    {"erlang.insert_element", [:idx1, :tuple, :any], ":erlang.insert_element($1, $2, $3)"},
    {"erlang.delete_element", [:idx1, :tuple], ":erlang.delete_element($1, $2)"},
    {"erlang.append_element", [:tuple, :any], ":erlang.append_element($1, $2)"},
    {"erlang.make_tuple", [:size, :any], ":erlang.make_tuple($1, $2)"},
    {"erlang.tuple_to_list", [:tuple], ":erlang.tuple_to_list($1)"},
    {"Tuple.insert_at", [:tuple, :idx, :any], "Tuple.insert_at($1, $2, $3)"},
    {"Tuple.delete_at", [:tuple, :idx], "Tuple.delete_at($1, $2)"},
    {"Tuple.duplicate", [:any, :size], "Tuple.duplicate($1, $2)"},
    {"Tuple.to_list", [:tuple], "Tuple.to_list($1)"},
    {"tuple_lit", [:any, :any], "{$1, $2}"},
    {"hd", [:list], "hd($1)"},
    {"tl", [:list], "tl($1)"},
    {"length", [:list], "length($1)"},
    {"concat", [:list, :any], "$1 ++ $2"},
    {"subtract", [:list, :list], "$1 -- $2"},
    {"cons_lit", [:any, :list], "[$1 | $2]"},
    {"List.first", [:list], "List.first($1)"},
    {"List.last", [:list], "List.last($1)"},
    {"List.wrap", [:any], "List.wrap($1)"},
    {"List.flatten", [:list], "List.flatten($1)"},
    {"List.delete", [:list, :any], "List.delete($1, $2)"},
    {"List.to_tuple", [:list], "List.to_tuple($1)"},
    {"List.keyfind", [:list, :key], "List.keyfind($1, $2, 0)"},
    {"List.zip", [:list, :list], "List.zip([$1, $2])"},
    {"lists.reverse", [:list], ":lists.reverse($1)"},
    {"lists.member", [:any, :list], ":lists.member($1, $2)"},
    {"Map.get", [:map, :key], "Map.get($1, $2)"},
    {"Map.get3", [:map, :key, :any], "Map.get($1, $2, $3)"},
    {"Map.fetch", [:map, :key], "Map.fetch($1, $2)"},
    {"Map.fetch!", [:map, :key], "Map.fetch!($1, $2)"},
    {"Map.put", [:map, :key, :any], "Map.put($1, $2, $3)"},
    {"Map.put_new", [:map, :key, :any], "Map.put_new($1, $2, $3)"},
    {"Map.replace", [:map, :key, :any], "Map.replace($1, $2, $3)"},
    {"Map.replace!", [:map, :key, :any], "Map.replace!($1, $2, $3)"},
    {"Map.update", [:map, :key, :any, :fun1], "Map.update($1, $2, $3, $4)"},
    {"Map.update!", [:map, :key, :fun1], "Map.update!($1, $2, $3)"},
    {"Map.delete", [:map, :key], "Map.delete($1, $2)"},
    {"Map.drop", [:map, :key], "Map.drop($1, [$2])"},
    {"Map.take", [:map, :key], "Map.take($1, [$2])"},
    {"Map.merge", [:map, :map], "Map.merge($1, $2)"},
    {"Map.pop", [:map, :key], "Map.pop($1, $2)"},
    {"Map.pop3", [:map, :key, :any], "Map.pop($1, $2, $3)"},
    {"Map.pop!", [:map, :key], "Map.pop!($1, $2)"},
    {"Map.pop_lazy", [:map, :key, :fun0], "Map.pop_lazy($1, $2, $3)"},
    {"Map.get_lazy", [:map, :key, :fun0], "Map.get_lazy($1, $2, $3)"},
    {"Map.put_new_lazy", [:map, :key, :fun0], "Map.put_new_lazy($1, $2, $3)"},
    {"Map.replace_lazy", [:map, :key, :fun1], "Map.replace_lazy($1, $2, $3)"},
    {"Map.keys", [:map], "Map.keys($1)"},
    {"Map.values", [:map], "Map.values($1)"},
    {"Map.to_list", [:map], "Map.to_list($1)"},
    {"Map.has_key?", [:map, :key], "Map.has_key?($1, $2)"},
    {"Map.from_keys", [:key, :key, :any], "Map.from_keys([$1, $2], $3)"},
    {"Map.from_struct", [:map], "Map.from_struct($1)"},
    {"Map.split", [:map, :key], "Map.split($1, [$2])"},
    {"Map.new", [:list], "Map.new($1)"},
    {"Map.filter", [:map], "Map.filter($1, fn {_, v} -> is_atom(v) end)"},
    {"maps.get", [:key, :map], ":maps.get($1, $2)"},
    {"maps.get3", [:key, :map, :any], ":maps.get($1, $2, $3)"},
    {"maps.find", [:key, :map], ":maps.find($1, $2)"},
    {"maps.is_key", [:key, :map], ":maps.is_key($1, $2)"},
    {"maps.take", [:key, :map], ":maps.take($1, $2)"},
    {"maps.put", [:key, :any, :map], ":maps.put($1, $2, $3)"},
    {"maps.update", [:key, :any, :map], ":maps.update($1, $2, $3)"},
    {"maps.remove", [:key, :map], ":maps.remove($1, $2)"},
    {"maps.keys", [:map], ":maps.keys($1)"},
    {"maps.values", [:map], ":maps.values($1)"},
    {"maps.to_list", [:map], ":maps.to_list($1)"},
    {"maps.from_list", [:list], ":maps.from_list($1)"},
    {"maps.merge", [:map, :map], ":maps.merge($1, $2)"},
    {"maps.with", [:key, :map], ":maps.with([$1], $2)"},
    {"maps.without", [:key, :map], ":maps.without([$1], $2)"},
    {"map_get", [:key, :map], ":erlang.map_get($1, $2)"},
    {"is_map_key", [:key, :map], "is_map_key($2, $1)"},
    {"map_size", [:map], "map_size($1)"},
    {"map_upd_atom", [:map, :any], "%{$1 | a: $2}"},
    {"map_upd_key", [:map, :key, :any], "%{$1 | $2 => $3}"},
    {"map_lit", [:key, :any, :any], "%{$1 => $2, b: $3}"},
    {"map_dot_a", [:map], "$1.a"},
    {"map_dot_b", [:map], "$1.b"},
    {"struct_upd", [:map, :any], "%CaseS{$1 | a: $2}"},
    {"struct_lit", [:any], "%CaseT{a: $1}"},
    {"eq", [:any, :any], "$1 == $2"},
    {"lt", [:any, :any], "$1 < $2"},
    {"and", [:bool, :any], "$1 and $2"},
    {"or", [:bool, :any], "$1 or $2"},
    {"not", [:bool], "not $1"},
    {"andand", [:any, :any], "$1 && $2"},
    {"oror", [:any, :any], "$1 || $2"},
    {"plus", [:num, :num], "$1 + $2"},
    {"div", [:int, :int], "div($1, $2)"},
    {"abs", [:num], "abs($1)"},
    {"min", [:any, :any], "min($1, $2)"},
    {"max", [:any, :any], "max($1, $2)"},
    {"erlang.min", [:any, :any], ":erlang.min($1, $2)"},
    {"binconcat", [:bin, :bin], "$1 <> $2"},
    {"byte_size", [:bin], "byte_size($1)"},
    {"to_string", [:any], "to_string($1)"},
    {"interp", [:any], "\"<\#{$1}>\""},
    {"is_function2", [:any, :size], "is_function($1, $2)"},
    {"fun_apply", [:fun1, :any], "$1.($2)"},
    {"fun_apply2", [:fun2, :any, :any], "$1.($2, $3)"},
    {"Enum.map", [:list, :fun1], "Enum.map($1, $2)"},
    {"Enum.filter", [:list, :fun1], "Enum.filter($1, $2)"},
    {"Enum.reduce", [:list, :any, :fun2], "Enum.reduce($1, $2, $3)"},
    {"Enum.count", [:any], "Enum.count($1)"},
    {"Atom.to_string", [:atom], "Atom.to_string($1)"},
    {"Integer.to_string", [:int], "Integer.to_string($1)"},
    {"Keyword.get", [:list, :key], "Keyword.get($1, $2)"},
    {"for", [:list], "for z <- $1, do: {z}"},
    {"for_map", [:map], "for {k, v} <- $1, into: %{}, do: {v, k}"},
    {"case_shape", [:any],
     "case $1 do\n{a, b} -> {1, a, b}\n%{a: a} -> {2, a}\n[h | t] -> {3, h, t}\n[] -> {4}\nz when is_atom(z) -> {5, z}\nz -> {6, z}\nend"},
    {"case_guard", [:any],
     "case $1 do\n%{a: z} when is_integer(z) -> {1, z}\n%{} = m when map_size(m) == 0 -> {2, m}\n{z, _} when is_atom(z) -> {3, z}\n{_, z} -> {4, z}\nz when is_list(z) and z != [] -> {5, hd(z)}\nz when is_map(z) -> {6, z}\nz -> {7, z}\nend"},
    {"case_lit", [:any],
     "case $1 do\n:a -> {1}\n{:ok, z} -> {2, z}\n%{b: :b} = m -> {3, m}\n[_, z] -> {4, z}\n0 -> {5}\nz when is_binary(z) -> {6, z}\nz -> {7, z}\nend"},
    {"case_two", [:any, :any],
     "case {$1, $2} do\n{a, a} -> {1, a}\n{%{a: a}, b} when is_atom(b) -> {2, a, b}\n{{a}, [b | _]} -> {3, a, b}\n{a, b} when is_tuple(a) or is_map(b) -> {4, a, b}\nz -> {5, z}\nend"},
    {"case_struct", [:any],
     "case $1 do\n%CaseS{a: z} -> {1, z}\n%CaseT{} = t -> {2, t}\n%_{} = s -> {3, s}\n%{__struct__: z} -> {4, z}\nz -> {5, z}\nend"},
    {"fn_clauses", [:any],
     "(fn\n{a, _} -> {1, a}\n%{b: b} -> {2, b}\n[a | _] when is_atom(a) -> {3, a}\nz -> {4, z}\nend).($1)"},
    {"if_refine", [:any],
     "if is_tuple($1) and tuple_size($1) > 0 do\n{1, elem($1, 0)}\nelse\n{2, $1}\nend"},
    {"cond_refine", [:any],
     "cond do\nis_map($1) and is_map_key($1, :a) -> {1, $1.a}\nis_list($1) and $1 != [] -> {2, hd($1)}\nis_integer($1) -> {3, $1 + 1}\ntrue -> {4, $1}\nend"},
    {"with_refine", [:any, :key],
     "with {:ok, z} <- $1,\n%{} = m <- z,\n{:ok, w} <- Map.fetch(m, $2) do\n{1, w}\nelse\ne -> {2, e}\nend"},
    {"match_refine", [:any],
     "case $1 do\nz = {_, _} -> {1, z, elem(z, 1)}\nz = %{a: _} -> {2, Map.put(z, :b, 1)}\nz -> {3, z}\nend"},
  ]

  @oneline for {_, _, f} = op <- @ops, not String.contains?(f, "\n"), do: op

  def check(seed) do
    :rand.seed(:exsss, {seed, 13, 17})
    Process.delete(:expr_forbody_skip)
    # one export cache per VM (it caches the stdlib's ExCk signatures)
    cache =
      Process.get(:expr_cache) ||
        (fn ->
           {:ok, c} = Module.ParallelChecker.start_link()
           Process.put(:expr_cache, c)
           c
         end).()

    Enum.reduce_while(1..@trials, %{"expr_trials" => 0, "expr_ok" => 0, "expr_raised" => 0, "expr_skip" => 0, "expr_err" => 0, "expr_term" => 0, "expr_forbody_skip" => 0}, fn _, acc ->
        case trial(cache) do
          {:fail, sig, msg} -> {:halt, %{"v" => "fail", "sig" => sig, "msg" => String.slice(msg, 0, 4000)}}
          k ->
            fb = if Process.delete(:expr_forbody_skip), do: 1, else: 0
            {:cont, acc |> Map.update!("expr_trials", &(&1 + 1)) |> Map.update!(k, &(&1 + 1)) |> Map.update!("expr_forbody_skip", &(&1 + fb))}
        end
      end)
  end

  defp trial(cache) do
    # 1 in 5: a random export of a pure stdlib module (inferred signatures, inlined/rewritten calls)
    {name, holes, fmt} = if :rand.uniform(5) == 1, do: std_op(), else: Enum.random(@ops)
    {src, vars, lits_certain, _} = build(holes, fmt, 1, 2)
    mode = if :rand.uniform(10) <= 7, do: :dynamic, else: :static
    run(name, src, vars, lits_certain, mode, cache)
  end

  # fills the holes of fmt: a literal, a variable x<i>, or (1 in 5, up to depth 2) a one-line template
  # with its own holes; returns {source, [{i, value, type, certain?}], all literals certain?, next i}
  defp build(holes, fmt, i, depth) do
    {fills, i} =
      Enum.map_reduce(holes, i, fn kind, i ->
        if depth > 0 and :rand.uniform(5) == 1 do
          {_, h2, f2} = Enum.random(@oneline)
          {s2, vs2, _, i} = build(h2, f2, i, depth - 1)
          # a subexpression's type is the checker's own, usually static and wider than its value (an
          # error on a static type means some value fails, not all): never certain
          {{:sub, s2, vs2, false}, i}
        else
          case fill(kind) do
            {:lit, _, _} = l -> {l, i}
            {:val, v} -> {var(i, v), i + 1}
          end
        end
      end)

    src =
      fills
      |> Enum.with_index(1)
      |> Enum.reverse()
      |> Enum.reduce(fmt, fn
        {{:lit, s, _}, j}, acc -> String.replace(acc, "$#{j}", "(" <> s <> ")")
        {{:sub, s, _, _}, j}, acc -> String.replace(acc, "$#{j}", "(" <> s <> ")")
        {{:var, i, _, _, _}, j}, acc -> String.replace(acc, "$#{j}", "x#{i}")
      end)

    vars =
      Enum.flat_map(fills, fn
        {:var, i, v, t, c} -> [{i, v, t, c}]
        {:sub, _, vs, _} -> vs
        _ -> []
      end)

    certain = Enum.all?(fills, fn
      {:lit, _, c} -> c
      {:sub, _, _, c} -> c
      _ -> true
    end)

    {src, vars, certain, i}
  end

  defp run(name, src, vars, lits_certain, mode, cache) do
    task = Task.async(fn -> typecheck(src, vars, mode, cache) end)

    res =
      case Task.yield(task, @tc_ms) || Task.shutdown(task, :brutal_kill) do
        {:ok, r} -> r
        nil -> {:fail, "hang__expr__typecheck", "typing took more than #{@tc_ms} ms\n" <> describe(src, vars, mode)}
        {:exit, reason} -> {:skip, inspect(reason)}
      end

    case res do
      {:fail, _, _} = f -> f
      {:skip, _} -> "expr_skip"
      {:typed, type, warnings, failed} ->
        H.Print.note(type, "oracle 9 expression type")
        for {_, _, t, _} <- vars, do: H.Print.note(t, "oracle 9 variable type")
        judge(name, src, vars, lits_certain, mode, type, warnings, failed)
    end
  end

  def typecheck(src, vars, mode, cache) do
    {res, _diags} =
      Code.with_diagnostics([log: false], fn ->
        try do
          env = Code.env_for_eval([])
          body = Code.string_to_quoted!(src)
          params = for {i, _, _, _} <- vars, do: Macro.var(:"x#{i}", nil)
          fun = {:fn, [], [{:->, [], [params, body]}]}
          {ast, _, _} = :elixir_expand.expand(fun, :elixir_env.env_to_ex(env), env)
          {:fn, _, [{:->, _, [pats, ebody]}]} = ast
          {:expanded, pats, ebody}
        rescue
          e -> {:skip, Exception.message(e)}
        end
      end)

    case res do
      {:skip, _} = s ->
        s

      {:expanded, pats, ebody} ->
        try do
          handler = fn _, fa, _, _ -> raise "no local #{inspect(fa)}" end
          stack = Module.Types.stack(mode, "expr.ex", ExprMod, {:test, 0}, [], cache, handler)
          types = for {_, _, t, _} <- vars, do: t
          {_, _, _, _, _, ctx} =
            Pattern.of_head(pats, [], types, Pattern.init_previous(), {:fn, pats}, [], stack, Module.Types.context())

          {type, ctx} = Expr.of_expr(ebody, term(), ebody, stack, ctx)

          warnings =
            for {m, w, {_, meta, _}} <- ctx.warnings do
              {Keyword.get(meta, :line), m.format_diagnostic(w).message}
            end

          {:typed, type, Enum.reverse(warnings), ctx.failed}
        rescue
          e ->
            case H.Worker.classify(e, __STACKTRACE__, []) do
              {:fail, sig, msg} -> {:fail, sig, describe(src, vars, mode) <> "\n" <> msg}
              {:skip, _, msg} -> {:skip, msg}
            end
        end
    end
  end

  defp judge(name, src, vars, lits_certain, mode, type, warnings, failed) do
    {res, _} =
      Code.with_diagnostics([log: false], fn ->
        try do
          {v, _} = Code.eval_string(src, for({i, x, _, _} <- vars, do: {:"x#{i}", x}))
          {:ok, v}
        catch
          _, _ -> :raised
        end
      end)

    area = String.replace(name, ~r/[^A-Za-z0-9_]/, "_")
    exempt = name in ["for", "for_map", "struct_upd"]

    case res do
      :raised ->
        "expr_raised"

      {:ok, r} ->
        certain_vars = lits_certain and Enum.all?(vars, fn {_, _, _, c} -> c end)
        # a struct pattern is typed as the well-formed struct (gradual-set-theoretic-types.md:294); a map
        # with a struct's fields removed or extra keys added (Map.delete/Map.put_new on a struct) prints
        # as %{__struct__: CaseS, ..}: such claims on struct patterns are by design (as in oracle 3)
        claims =
          for {line, msg} <- warnings,
              H.Worker.clause_claim?(msg),
              not (msg =~ ~r/^    %(CaseS|CaseT|_)\{/m and msg =~ "__struct__:"),
              do: {line, msg}
        # "incompatible types given on function call within Map.update/4" (a fun argument applied to
        # the values it may receive) is conditional: no call happens when the key is absent or the list empty
        fails =
          for {_, msg} <- warnings, H.Worker.fail_area(msg), H.Worker.certain?(msg),
              not (msg =~ "on function call within"), do: msg
        # stdlib functions whose list clause is a for-comprehension (Enum.filter_map/3: `for element <-
        # enumerable, filter.(element), do: mapper.(element)`) get a signature that assumes the body runs:
        # "For comprehensions in Elixir assume they are executed at least once"
        # (gradual-set-theoretic-types.md:246), so a non-fun mapper with [] warns by design (counted skip)
        {forbody, fails} = Enum.split_with(fails, &(&1 =~ ~r/given to Enum\.filter_map\/3:/))
        if forbody != [], do: Process.put(:expr_forbody_skip, true)
        # the clause that ran is the {i, ..} result's line (the case/fn line is 1, clause i on line i + 1)
        ran_line = with t when is_tuple(t) and tuple_size(t) > 0 <- r, i when is_integer(i) <- elem(t, 0), do: i + 1, else: (_ -> nil)
        ran_line = if String.starts_with?(name, ["case_", "fn_", "match_", "cond_"]), do: ran_line, else: nil
        # multi-line templates have branches that did not run: only their clause claims are judged
        oneline = not String.contains?(src, "\n")

        cond do
          ran_line && not exempt && Enum.any?(claims, fn {l, _} -> l == ran_line end) ->
            {_, msg} = Enum.find(claims, fn {l, _} -> l == ran_line end)
            {:fail, "expr__clause__" <> area,
             "the clause on line #{ran_line} ran (result #{inspect(r)}), but the type checker warned:\n" <>
               msg <> "\n" <> describe(src, vars, mode)}

          oneline and fails != [] and certain_vars and not exempt ->
            {:fail, "expr__warn__" <> area,
             "evaluated to #{inspect(r)} without raising, but the type checker warned:\n" <>
               hd(fails) <> "\n" <> describe(src, vars, mode)}

          failed ->
            "expr_err"

          not H.Mem.member?(r, type) and not known_tl?(src, vars) ->
            {:fail, "expr__ret__" <> area,
             "evaluated to #{inspect(r)}, outside the upper bound of its type #{to_quoted_string(type)}\n" <>
               describe(src, vars, mode) <> warn_text(warnings)}

          upper_bound(type) == term() ->
            "expr_term"

          true ->
            "expr_ok"
        end
    end
  end

  # open upstream #15490: list_tl underapproximates on difference types
  defp known_tl?(src, vars) do
    src =~ ~r/(^|[^\w.])tl\(|List\.|Enum\./ and
      Enum.any?(vars, fn {_, _, t, _} -> to_quoted_string(t) =~ " and not " end)
  end

  defp warn_text([]), do: ""
  defp warn_text(ws), do: "\nwarnings:\n" <> Enum.map_join(ws, "\n", &elem(&1, 1))

  def describe(src, vars, mode) do
    vs =
      for {i, v, t, _} <- vars do
        "  x#{i} = #{inspect(v, limit: :infinity)} :: #{to_quoted_string(t)}"
      end

    "expression (typed in #{mode} mode as the body of fn x1, .. -> .. end, Pattern.of_head with the types below as expected):\n" <>
      src <> "\n" <> Enum.join(vs, "\n")
  end

  # a hole: a literal (its source; certain when its type is exact at Descr granularity: not lists,
  # whose types have no length, nor maps with non-atom keys: known, %{1 => 2} is typed
  # %{integer() => integer()} which includes %{}) or a value
  defp fill(kind) when kind in [:fun0, :fun1, :fun2] do
    srcs = %{fun0: @fun0, fun1: @fun1, fun2: @fun2}[kind]
    s = Enum.random(srcs)
    if :rand.uniform(3) == 1, do: {:val, elem(Code.eval_string(s), 0)}, else: {:lit, s, true}
  end

  defp fill(kind) do
    v = if :rand.uniform(8) == 1, do: value(:any, 2), else: value(kind, 2)
    lit_p = if kind in [:key, :idx, :idx1, :size], do: 7, else: 3

    if :rand.uniform(10) <= lit_p and not is_function(v),
      do: {:lit, inspect(v, limit: :infinity), H.Mem.precise?(v)},
      else: {:val, v}
  end

  # improper lists: stdlib specs say proper lists (`:lists.member(1, [1 | 7])` returns true, the spec
  # says `List :: [T]`), and `[k | map]` is a map iterator for :maps functions: never certain
  defp improper?(v) when is_map(v), do: Enum.any?(Map.to_list(v), fn {k, x} -> improper?(k) or improper?(x) end)
  defp improper?(v) when is_tuple(v), do: v |> Tuple.to_list() |> Enum.any?(&improper?/1)
  defp improper?([h | t]) when is_list(t), do: improper?(h) or improper?(t)
  defp improper?([_ | _]), do: true
  defp improper?(_), do: false

  # a variable: its value and a type containing it; certain when the type is purely gradual or the
  # exact type of the value
  defp var(i, v) do
    exact = H.Mem.of(v)

    base =
      if H.Mem.precise?(v) and :rand.uniform(2) == 1 and not is_struct(v),
        do: H.Alg.widen(v, 3),
        else: exact

    base = if :rand.uniform(4) == 1, do: opt_union(base, Enum.random(extra())), else: base

    # minus a type v is certainly not in (BDD differences/negations in the operand types)
    base =
      if :rand.uniform(5) == 1 do
        y = Enum.random(minus())
        if H.Mem.member?(v, y), do: base, else: Descr.opt_difference(base, y)
      else
        base
      end

    {t, certain} =
      case :rand.uniform(10) do
        k when k <= 5 -> {base, base == exact and H.Mem.precise?(v)}
        k when k <= 9 -> {dynamic(base), true}
        _ -> {dynamic(), true}
      end

    # widen/union keep v by construction; drop the variable's type to exact if not (a harness slip)
    certain = certain and not improper?(v)
    if H.Mem.member?(v, t), do: {:var, i, v, t, certain}, else: {:var, i, v, exact, H.Mem.precise?(v)}
  end

  defp extra do
    import Descr
    [atom(), integer(), float(), binary(), tuple(), open_map(), list(term()), empty_list(), atom([:a, :ok]),
     H.Mem.of(value(:small, 1)), H.Mem.of(value(:small, 1)), closed_map(a: {integer(), false}), open_tuple([atom([:ok])])]
  end

  defp minus do
    import Descr
    [atom([:a]), atom([:ok, nil]), tuple([term()]), open_tuple([atom([:ok])]), closed_map(a: {integer(), false}),
     open_map(b: {term(), false}), empty_map(), empty_list(), non_empty_list(integer()), integer(),
     tuple([term(), term()]), open_map(a: {atom(), false}), H.Mem.of(value(:any, 1)), H.Mem.of(value(:map, 1)),
     H.Mem.of(value(:tuple, 1)), H.Alg.widen(value(:small, 1), 2)]
  end

  def value(:any, d) do
    k = Enum.random([:small, :small, :small, :tuple, :map, :map, :list, :list, :bin, :num, :struct, :range, :fun1])
    value(k, d)
  end

  def value(:small, _d),
    do: Enum.random(@atoms ++ @ints ++ [1.0, 2.5, "k", "", [], {}, {:ok, 1}, %{}, %{a: 1}, [1], [:a, :b]])

  def value(:tuple, d) do
    n = Enum.random([0, 1, 1, 2, 2, 2, 3, 3])
    List.to_tuple(for _ <- 1..n//1, do: sub(d))
  end

  def value(:map, d) do
    case :rand.uniform(12) do
      1 -> %{}
      2 -> %{a: sub(d)}
      3 -> %{a: sub(d), b: sub(d)}
      4 -> %{b: sub(d)}
      5 -> %{c: sub(d), a: sub(d)}
      6 -> %{1 => sub(d)}
      7 -> %{"k" => sub(d), a: sub(d)}
      8 -> %{{:ok} => sub(d), 2.0 => sub(d)}
      9 -> value(:struct, d)
      10 -> %{a: %{b: sub(d)}}
      11 -> %{b: :b, c: sub(d)}
      _ -> %{a: sub(d), b: sub(d), c: sub(d)}
    end
  end

  def value(:list, d) do
    case :rand.uniform(10) do
      1 -> []
      2 -> [sub(d)]
      3 -> [sub(d), sub(d)]
      4 -> [a: sub(d)]
      5 -> [a: sub(d), b: sub(d)]
      6 -> [sub(d) | sub(d)]
      7 -> [1, 2, 3]
      8 -> [{:a, 1}, {1, :b}]
      9 -> [:a]
      _ -> [[sub(d)], []]
    end
  end

  def value(:struct, d), do: Enum.random([%CaseS{}, %CaseS{a: sub(d)}, %CaseT{}, %CaseT{a: sub(d)}])
  def value(:range, _), do: Enum.random([1..3, 0..0//1, 3..1//-1])
  def value(:key, _), do: Enum.random(@keys)
  def value(:idx, _), do: Enum.random([0, 0, 1, 1, 2, 3, -1])
  def value(:idx1, _), do: Enum.random([1, 1, 2, 2, 3, 4, 0])
  def value(:size, _), do: Enum.random([0, 1, 2, 3])
  def value(:int, _), do: Enum.random(@ints)
  def value(:num, _), do: Enum.random(@ints ++ [1.0, 2.5])
  def value(:atom, _), do: Enum.random(@atoms)
  def value(:bool, _), do: Enum.random([true, false])
  def value(:bin, _), do: Enum.random(["k", "", "abc", "a b"])
  def value(:fun1, _), do: elem(Code.eval_string(Enum.random(@fun1)), 0)
  def value(:fun2, _), do: elem(Code.eval_string(Enum.random(@fun2)), 0)
  def value(:fun0, _), do: elem(Code.eval_string(Enum.random(@fun0)), 0)

  defp sub(d) when d <= 0, do: value(:small, 0)
  defp sub(d), do: if(:rand.uniform(3) == 1, do: value(Enum.random([:tuple, :map, :list]), d - 1), else: value(:small, d))

  # a random export with an inferred signature of a pure stdlib module; holes :any
  defp std_op do
    mod = Enum.random(@std)

    funs =
      case Process.get({:expr_std, mod}) do
        nil ->
          fs =
            for {{f, a}, %{sig: {:infer, _, _}}} <- std_exports(mod), a <= 3,
                not String.starts_with?(Atom.to_string(f), "_"), {f, a} not in @std_skip,
                do: {f, a}

          Process.put({:expr_std, mod}, Enum.sort(fs))
          Enum.sort(fs)

        fs ->
          fs
      end

    case funs do
      [] ->
        {"Map.keys", [:map], "Map.keys($1)"}

      _ ->
        {f, a} = Enum.random(funs)
        holes = for _ <- 1..a//1, do: Enum.random([:any, :any, :list, :map, :bin, :int, :fun1, :atom])
        args = Enum.map_join(1..a//1, ", ", &"$#{&1}")
        {"std_" <> inspect(mod), holes, "#{inspect(mod)}.#{f}(#{args})"}
    end
  end

  defp std_exports(mod) do
    with path when is_list(path) <- :code.which(mod),
         {:ok, {_, [{~c"ExCk", chunk}]}} <- :beam_lib.chunks(path, [~c"ExCk"]),
         {_, %{exports: exports}} <- :erlang.binary_to_term(chunk) do
      exports
    else
      _ -> []
    end
  end
end

defmodule H.Main do
  def main(["--repro", file, sig]) do
    case_ = file |> File.read!() |> JSON.decode!()
    out = H.Worker.check(case_, 60_000)
    IO.puts(JSON.encode!(out))

    verdict =
      cond do
        out["v"] == "fail" and out["sig"] == sig -> "yes"
        out["v"] == "skip" -> "undecided"
        true -> "no"
      end

    # the verdict is the last line; repro.sh reads it (a VM that dies prints none)
    IO.puts("REPRO " <> verdict)
    System.halt(0)
  end

  def main([]) do
    cover = System.get_env("ERCOVER_DIR")
    if cover, do: cover_start()
    loop(cover, 0)
  end

  defp cover_start do
    {:ok, _} = :cover.start()
    ebin = :code.lib_dir(:elixir) |> Path.join("ebin")

    for beam <- Path.wildcard(Path.join(ebin, "Elixir.Module.Types*.beam")) do
      :cover.compile_beam(String.to_charlist(beam))
    end
  end

  defp cover_export(nil), do: :ok

  defp cover_export(dir) do
    File.mkdir_p!(dir)
    # one file per VM, rewritten with the cumulative data at every export
    name = Process.get(:cover_file) || "worker-#{System.pid()}-#{System.os_time(:millisecond)}.coverdata"
    Process.put(:cover_file, name)
    :cover.export(String.to_charlist(Path.join(dir, name)))
  end

  defp loop(cover, n) do
    case IO.read(:stdio, :line) do
      line when is_binary(line) ->
        out =
          try do
            H.Worker.check(JSON.decode!(line))
          rescue
            e -> %{"v" => "skip", "skip" => "worker_error", "msg" => Exception.format(:error, e, __STACKTRACE__) |> String.slice(0, 1500)}
          end

        IO.puts(JSON.encode!(out))
        if rem(n + 1, 200) == 0, do: cover_export(cover)
        loop(cover, n + 1)

      _eof ->
        cover_export(cover)
        System.halt(0)
    end
  end
end

unless System.get_env("H_NO_MAIN"), do: H.Main.main(System.argv())
