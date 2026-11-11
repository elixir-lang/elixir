# elixir type checker harness

Target: the gradual set-theoretic type checker (`lib/elixir/lib/module/types{.ex,/*.ex}`), Elixir main.

This harness was written and evolved by ephyra-agent (an autonomous loop of Claude agents around the
[ephyra](https://github.com/datadog-labs/ephyra) property-based testing library) during one run on
Elixir main, 2026-11-10/11. It found elixir-lang/elixir#16101, #16102 and #16105-#16111. It is a run
artifact, not a maintained tool: judge it by what it finds and covers, not by its code.

The same harness, with the 9 bug reports, is in
datadog-labs/ephyra#34 (`case-studies/elixir`).

## Quick start

Needs Erlang/OTP 27+ (`erl` on `PATH`, or `OTP_BIN=<otp>/bin`), Rust (stable), rsync and an Elixir
checkout.

```sh
bash ephyra-harness/build.sh . /tmp/eb      # from the repo root: copies the tree to /tmp/eb/elixir, make compile, builds the driver
bash ephyra-harness/run.sh /tmp/eb 1 60 /tmp/out           # seed 1, 60 s; stats in /tmp/out/stats.json
ls /tmp/out/findings                        # one folder per failure signature
/tmp/out/findings/<sig>/1/repro.sh /tmp/eb  # exit 1: still reproduces
```

Each finding folder has `case.ex` (the module and the calls, readable), `case.json` (what the worker checks)
and `finding.json` (the oracle's message). Coverage of `Module.Types*`: `bash build.sh <src> <build> coverage`,
then run with `ERCOVER_DIR=<dir>` set; each worker VM exports Erlang `cover` data there.

## Layout

- `build.sh <src> <build> [coverage]`: rsyncs the tree (no --delete) to `<build>/elixir`, `make compile` there (with `OTP_BIN`
  or the `erl` on `PATH`), builds the Rust driver (`driver/`, ephyra's `Source` as RNG) into `<build>/cargo`, copies
  `elixir-fuzz` and `worker.exs` to `<build>/harness`. `coverage` also writes `coverage.json`
  (`ercover`, ebin `<build>/elixir/lib/elixir/ebin`, prefix `Elixir.Module.Types`).
- `run.sh <build> <seed> <secs> <out>`: one `elixir-fuzz` process with 3 long-lived Elixir VMs
  (`FUZZ_WORKERS` overrides) running `worker.exs`, one JSON case per line.
  With `ERCOVER_DIR` set each VM cover-compiles `Elixir.Module.Types*` and exports
  `worker-<pid>-<ms>.coverdata` every 200 cases and at exit.
- `driver/src/gen.rs`: generator of modules (1-3 functions, 1-3 clauses each, patterns with tuples/lists/
  maps/binds, guards with `and/or/not`, `is_*`, `map_size`, `is_map_key`, `:erlang.map_get`, `elem`, `hd`,
  `length`, `in`; bodies with case/if/cond/with/else, blocks with matches, multi-clause `fn` applied
  immediately, `Map.*` (put/replace/put_new/update/delete/merge/take/drop/get/fetch/fetch!/pop/keys/values),
  map literal and update syntax, `elem/put_elem/Tuple.*`, `hd/tl/length/++/List.*/Enum.*`, operators,
  local calls to earlier functions, private helpers). Small pools: atoms `:a :b :ok :error nil true false`,
  ints `0 1 -1 2`, keys `:a :b :c 1 "k" 2.0`, 12 variable names. Call arguments are mostly instantiated
  from the clause patterns. No recursion, so every call terminates.
- `driver/src/ast.rs`: one sort-tagged node type, rendering, and a generic shrinker (drop kids,
  replace a node by a same-sort descendant, `nil`/`_`); candidates must fail with the same signature.
- Findings: `findings/<sig>/<n>/{case.ex, case.json, worker.exs, finding.json, repro.sh}`; `repro.sh <build>`
  re-runs `worker.exs --repro` (exit 1 reproduces, 0 does not, 2 undecided: invalid case or VM failure).

## Oracles

1. **Soundness of inferred signatures** (`sound__domain__<kind>`, `sound__ret__<kind>`, kind = value kind).
   Contract: gradual-set-theoretic-types.md:10, the type system is sound: "the inferred and assigned by the
   type system align with the behaviour of the program". The module is compiled, the ExCk chunk's
   `{:infer, _, [{args, ret}]}` signatures read, each generated call run; when it returns:
   - some clause's argument types (upper bound) must contain the arguments (else `domain`);
   - the value must be in the union of `upper_bound(ret)` of the clauses that accept the arguments (else `ret`).
   Membership (`H.Mem` in worker.exs) is exact for atoms, numbers, binaries, tuples and atom-keyed maps
   (Descr does not tell integers/floats/binaries apart, so neither do we). Lists (list types have no
   length), maps with non-atom keys (domain keys are optional) and funs (`fun(n)` is the top of arity n)
   only fail when the value's type is **disjoint** from the expected type (weaker, never a false positive).
2. **Crash / hang** (`crash__exception__<Module.fun>`, `hang__compile__typecheck`): any exception with a
   `Module.Types.*` frame while compiling a valid module (the first such frame names the area), or
   compiling taking more than 30 s (60 s in repro).

3. **Clause-never-runs warnings vs runtime** (`warn__clause__<def|case|fn|else>`). Contract:
   gradual-set-theoretic-types.md:236 (the checker finds code where "all combinations of a type _will_ fail")
   and :244 (it "generally avoids emitting false positive type violations"; documented exceptions are `for`
   and struct update syntax, neither generated). The rendered module has a probe
   `Process.put({:h, id}, 1)` as the first statement of every def clause, case/else arm and fn clause; the
   clause head (pattern + guard) is alone on its line. A warning whose text says a clause never runs
   (`is redundant`, `cannot match because the previous clauses`, `will never match`, `will never succeed`;
   the Erlang compiler's own "previous clause at line N always matches" is excluded) fails the case when
   its line's probe ran during a call that **returned normally**. Probes hit only by calls that raised do
   not count: a clause that always raises may legitimately be reported as never matching (seen in the
   first runs: `def f(v0 = v1)` with a guard that always errors).
   Excluded by design: "will never match" on a `%CaseS{}`/`%CaseT{}` pattern whose expected type has one of
   that struct's fields `not_set()` (a struct with a field removed by Map.delete/:maps.remove; struct patterns
   are typed with every declared field present, gradual-set-theoretic-types.md:294). Counter
   `n_claims_removed_struct_field`.

4. **Always-fails warnings vs runtime** (`warn__expr__<area>`, area = `remote_<Module>`, `local`, `fnapply`,
   `tupleindex`, `mapupdate`, `assign`, `match`, `dot`, `binary`, `module`). Contract: the same two sentences
   as oracle 3 (:236 "all combinations of a type _will_ fail"; :244 false positives are "warnings emitted by
   the type checker when there are no runtime errors", only `for` and struct update are documented).
   A warning whose first line claims failure (`incompatible types given to`, `expected a map within map update
   syntax`, `incompatible types assigned to`, `the following pattern will never match`, ...; not comparisons,
   conditionals, `||`, nor the "does not have all required keys ... will always return/do nothing" notes)
   fails the case when every expression on its line is proven to have evaluated without raising:
   - the renderer tracks, per line, the probe whose body evaluates the line unconditionally (no clause head,
     guard, cond condition, `with` clause, or `and/or/&&/||` right operand on the line; if/cond/with bodies
     now have probes too) → case JSON `lines: [[line, probe]]`; such a probe hit in a call that **returned**
     proves the line ran;
   - every non-final block statement is followed by a statement probe (`posts: [[line, probe]]`); its hit in
     **any** call proves the statement's lines ran.
   Only claims about purely gradual or union-free types count: a type with a static union part
   (`:error or {:ok, x}`, `dynamic() or :error`) must be a subtype of the domain by design ("the underlying code
   needs to work with both"), so those are counted (`n_fails_static_ran`), not reported. Seen there:
   `Map.fetch(%{a: x}, :a)` is typed `{:ok, x} or :error` though the key is required.
   `H_DUMP=<file>` makes the worker append every such warning on a line that ran (debug aid).
   Generator support (round 1 steering): `if`/`cond` branches and case arms whose subject is a variable now
   see the variable refined by the condition / arm pattern (hints), so typed operations on it appear there.

5. **Function values vs their inferred type** (`sound__funapp__<local|remote|other>`: fn literal or local capture,
   remote capture `&Mod.f/n` incl. `&CaseMod.f/n`). Contract: gradual-set-theoretic-types.md:10 (soundness). Every
   function value of arity <= 2 inside a returned value (top level, tuple elements, atom-keyed map fields; component
   types via `tuple_fetch`/`map_fetch_key` on the upper bound) is applied to a fixed universe of values plus the
   case's `funargs` (instantiations of the fn literals' clause patterns) and the calls' arguments. When an application
   returns `r`, `r` must be in `fun_apply(F, Mem.of(args))`, F = upper bound of the value's type ∩ `fun(arity)`
   (static on both sides, so fun_apply's open gradual issue #16094 does not enter; badarg/badfun results are skipped).
   Probe hits from these applications are discarded. Counters: `n_funapps`, `n_funapps_term` (expected term()).

Generator support (round 2 steering): function values (`fn` literals in value position, `&f/n`, `&CaseMod.f/n`,
stdlib captures from `CAPTURES`, ~15% of def bodies return one), captures applied `(&f/n).(..)` and passed to
`Enum.map/filter/flat_map`, self-remote calls `CaseMod.fN(..)` (typed from the export cache, `Apply.export`),
`for` (generators over lists/maps, filters, `uniq`, `into: %{}`, `reduce`), `try` (rescue/catch/else/after),
`receive ... after 0`. Not probed (no owned lines): the `try` body (a rescued raise looks like completion) and
`for` (its warnings are documented false positives). In `for`, only the first generator's source sees outer
variables: the checker assumes the body runs at least once and refines outer variables from it (documented,
gradual-set-theoretic-types.md:246), which otherwise shows up as `sound__domain/ret` on empty comprehensions.

6. **Set algebra on the case's types** (`algebra__bareopt__<kind>`, `algebra__member__<kind of value>`,
   `algebra__proj__<op>`; H.Alg in worker.exs). Runs after the other oracles pass. Leaves: the inferred signature
   types (upper bounds; gradual ones too for bare/opt), `Mem.of` of the values the calls saw, those value types
   widened at random places (optional/dropped/extra fields, open maps and tuples, unions), and a few fixed types.
   Random expressions (union/intersection/difference/negation, depth 3) are evaluated with `opt_*` and `bare_*`:
   - both must be `equal?` (both document computing the same set operation);
   - for a precise value v (exact singleton type), `v in E` must equal the boolean combination of `v in leaf`;
   - for a member v: `elem(v,i)` in `tuple_fetch(E,i)`, `Tuple.delete_at/insert_at` in `tuple_delete_at/insert_at`,
     `v[k]` in `map_fetch_key(E,k)`, `Map.put` in `map_put_key`, `Map.delete` in `map_update(.., :delete, :keep)`,
     `%{v | k => x}` in `map_update(.., :reject)`, all on E restricted to tuples/maps.
   Choices are seeded by the case source (deterministic in repro). Cost ~5%. Round 3: 1.3M expressions under
   stress settings found nothing; it stays as a cheap guard on the recently rewritten BDD code.

Generator support (round 3 steering, from apply.ex/of.ex/pattern.ex uncovered lines): `Map.get_lazy/pop_lazy/pop!/
replace!/replace_lazy/update!/put_new_lazy`, `:maps.get/3, find, is_key, take, keys, values, to_list, put, update,
remove, from_keys`, `min/max` calls, `Tuple.duplicate`, `:erlang.setelement`, `tuple_size/map_size` guards with
every comparison, `is_function/1,2`, `is_bitstring`, `byte_size/bit_size`; structs `CaseS{a: nil, b: 0}` and
`CaseT{a: :x}` (defined by worker.exs, so repro inputs can name them): struct patterns incl. `%_{}`, literals,
`Map.from_struct`, struct values as call arguments (no struct update syntax: documented false positive);
bitstrings: patterns (`integer`, `size(8)`, `float`, `utf8`, `binary-size(1)`, binary/bits tails) and construction.
First runs: `:maps.from_keys([1, :a], v)` is typed `%{integer() => ...}` (the atom key is lost), seen as
sound__ret__map/tuple and sound__funapp__local.

7. **Oracles 3/4 on a less precise variant** (`gradual__<wrap|noinfer>__<area>`, area = `clause_<kind>` or an
   oracle 4 area). The variant runs exactly like the case module, so the original run's probe evidence applies;
   only what the checker knows changes. ~50% of cases `wrap`: `Case.gvar` (drawn last in `G::case`, so old shapes
   are unchanged) picks 15/40/100% of the eligible expressions and `N::render_wrapped` writes them as
   `CaseDyn.id(e)` (identity defined in worker.exs, outside the inferred applications, so typed `dynamic()`;
   no newline added, lines and probes stay put). Not wrapped: patterns, guards, variables, structure, `for`, and
   anything in a case subject, if/cond condition, `with`/`=` right side (that would drop a refinement or a
   literal-subject exemption). ~20% `noinfer`: the same source compiled with `infer_signatures: false`
   ("Type checking will be executed regardless", code.ex), so self-remote `CaseMod.f` calls are `dynamic()`
   and `Module.Types.Traverse` (never entered before) runs. Contract: oracles 3/4, plus
   gradual-set-theoretic-types.md:214/:216 (with `dynamic()` "only a subset of the type needs to be valid",
   warnings "once it is certain the code will fail"). A crash or hang while compiling the variant uses the
   oracle 2 signatures (message prefixed with the variant). The wrap variant's calls are also checked against its
   own inferred signatures (oracles 1 and 5: areas `ret_<kind>`, `domain_<kind>`, `funapp_<origin>`).
   Counters `n_gvar_wrap/noinfer`, `n_gvar_claims/fails`, `n_gvar_ok` (variant calls checked).
   Cost: one more compile on ~70% of cases (about a third fewer cases/s).
   Rejected in round 4: "the variant has a type warning the original lacks" (gradual guarantee without runtime
   evidence) fired on deliberate exemptions: dead code (`Map.pop!(m, k) || rhs` checks no rhs), code after a
   violation, literal-only expressions (`is_nil({"k"})`, `case {0.5, :b} do %{} ->`).

8. **Clause heads in isolation** (`head__<domain|ret|clause>__<area>`, `headsub__<ret|clause>__<area>`; H.Head in
   worker.exs). Runs after oracles 1-6 pass, before 7. Every group of clause heads in the case (a def's clauses,
   case/receive/try-else/with-else arms, fn clauses, each `pat <- e` of a with; arity 1-3, no pins/defaults) is
   lifted into a separate module `CaseHead`, one clause per line, bodies returning `{i, {bound vars}}`:
   - `hK`: the heads as def clauses (outer variables a guard uses become extra parameters) plus a catch-all
     `def hK(a1, ..) -> {:none, {a1, ..}}`, applied to up to 40 argument lists: values instantiated from each
     clause pattern (repeated variables equal, struct/map/bitstring/cons patterns built), near-miss mutants,
     the call arguments of the case and a fixed pool;
   - `sK_J` (2 per group): `case SUBJ do <the heads as arms>; h_rest -> {:none, h_rest} end`, SUBJ the
     structure of an instantiated value with some leaves literal and the others parameters (`[l1, 2]`,
     `{%{a: l1}, :ok}`), so the arms see a precise structural expected type (where patches 002/003/008 bugs lived),
     applied to the exact leaves and 6 variants; ~30% of the leaf parameters get a type guard from their value
     (`when is_integer(l1)`, so dynamic(integer()) instead of dynamic(); calls the guard rejects are not evidence).
   Every call returns (the catch-all matches everything), so each one is evidence for the checks of oracle 1
   (domain, and the result, i.e. the pattern variables' types after the pattern and guard, and through the
   catch-all the type left after the previous clauses, must be in the inferred return type; soundness,
   gradual-set-theoretic-types.md:10) and of oracle 3 (no clause the checker calls redundant / never matching
   may run; :236/:244, bodies here never raise). area = construct of the clause that ran (bitstring, struct, map,
   cons, list, alias, tuple, guard, literal, var), `rest_<construct>` for the catch-all.
   Values are seeded by the group's own clauses (stable under shrinking of other code). Counters
   `n_head_groups`, `n_head_calls`, `n_headsub_calls`. On the unpatched trunk it finds patch 002
   (`def h(v3 = v4) when is_list(v4) and is_map_key(v3, :c)` deducted from the catch-all) and patch 003
   (`case [[], [:a | :b] | 1.0] do [_ | [y | z]]` "will never match") within a few hundred cases.
   Cost: one more compile per case (~+70% worker time).

9. **Single expressions with chosen variable types** (`expr__<ret|warn|clause>__<op>`; H.Expr in worker.exs).
   Runs after oracles 1-6 and 8 pass, before 7; independent of the module: its 12 trials are drawn from the
   case's `eseed` only (gen.rs writes it; the shrinker keeps it), so findings shrink to a trivial module and the
   message holds the whole trial. Each trial picks a template (~130: Kernel/erlang tuple ops incl.
   insert/delete_element and make_tuple, list ops, every Map.* and :maps.* variant the checker special-cases,
   map/struct literal and update syntax, comparisons, boolean and short-circuit operators, interpolation,
   fn application, Enum, `for`, multi-line case/fn/cond/with/if templates that refine their subject; 1 in 5 a
   random export with an inferred signature of a pure stdlib module: List, Map, Keyword, Tuple, String, Integer,
   Float, Atom, Enum, MapSet, Range, Access, Bitwise, Base, URI, Version, Function), fills its holes with
   literals, variables or (1 in 5, depth 2) other one-line templates, and gives each variable a runtime value v
   and a type T containing v: `H.Mem.of(v)`, `H.Alg.widen(v)`, a union with a fixed type, a difference with a
   type v is certainly not in, any of these in `dynamic(..)`, or `dynamic()`. The expression is typed as the
   body of `fn x1, .. -> body end` with `Pattern.of_head(pats, [], Ts, ..)` + `Expr.of_expr` (type_helper.exs'
   `__typecheck__`, mode `:dynamic` 70% / `:static` 30%, a real `Module.ParallelChecker` cache so remote calls
   use the stdlib's ExCk signatures), then evaluated with `Code.eval_string` and the values bound. Programs
   seldom hand the checker such operand types (parameters are `dynamic()`), so apply.ex's static and gradual
   application paths and the stdlib's inferred signatures are judged on new inputs. When the evaluation returns r:
   - `ret`: no error was reported, so r must be in `upper_bound(type)` (gradual-set-theoretic-types.md:10);
   - `warn`: a warning that claims certain failure (`fail_area` + `certain?`, as oracle 4) on a one-line template
     whose variables are all purely `dynamic(..)` or exactly typed (`precise?` value, `T == of(v)`), literals
     precise, no subexpressions (their types are the checker's own, static and wider than the value) and no
     improper lists (stdlib specs say proper lists; `[k | map]` is a :maps iterator);
     "incompatible types given on function call within" (a fun argument that may never be applied) is excluded;
   - `clause`: the clause that ran (case/fn/cond template clauses return `{i, ..}` from line i + 1) is reported
     redundant / never matching (:236/:244). Struct-pattern claims on a malformed struct type
     (`%{__struct__: CaseS, ..}`, fields removed or added) are by design, as in oracle 3.
   A crash while typing uses the oracle 2 signature (`crash__exception__<Module.fun>`), a typing time over 10 s
   `hang__expr__typecheck`. `for`/`for_map`/`struct_upd` (documented false positives) are only checked for ret;
   `tl`/List/Enum results on difference types are not (open #15490). First stress run (200k cases): the only
   failure was a compiler crash, `List.to_charlist(x)` with a non-list x crashes `format_diagnostic`
   (apply.ex:2369, `expr_to_string` on a rewritten call); it reproduces in a plain module. Counters
   `n_expr_trials/ok/term/err/raised/skip`. Cost ~10 ms per case.

10. **Printed types denote the type** (`print__<parse|eval|lost|extra>__<kind>`; H.Print in worker.exs). Runs after
   oracles 1-6, 8 and 9 pass, before 7. Contract: descr.ex `to_quoted/2` "Converts a descr to its quoted
   representation" (`to_quoted_string/2`: "its quoted string representation"); the printed syntax is the type syntax of
   gradual-set-theoretic-types.md, and it is what every type diagnostic shows (upstream fixed printer imprecision as a
   bug before: #15837 "Type printer prints wider types on non_empty_list forms"). Inputs: every inferred signature type,
   every oracle 6 set expression (opt result), every oracle 9 expression and variable type (max 60, term-sorted), plus
   60 derived types (those types put in tuple elements, closed/open/optional map fields, domain values, list elements and
   tails, `dynamic(..)`, and combined by difference/intersection/symmetric difference; two rounds; seeded by the types).
   Each is printed with `to_quoted_string(t, skip_dynamic_for_indivisible: false)` (the default prints
   `dynamic(integer())` as `integer()` on purpose), parsed with `Code.string_to_quoted` (`parse`: it does not parse;
   area = the operator node with a wrong arity, e.g. `not_arity`), evaluated by an independent interpreter of the syntax
   (`eval`: unknown syntax; `or/and/not`, `dynamic(t)`, `list/non_empty_list`, tuples with `...`, `%{..., k: t}`,
   `if_set/not_set`, domain keys `%{integer() => t}`, `%S{..}` structs whose omitted fields are `term()`
   (`:collapse_structs`), `(a -> b)`) and compared with `equal?` (`lost`: values of the type missing from the printed
   type; `extra`: the reverse; kind = map/tuple/list/atom/fun/other of the difference). The fun part is compared on its
   own in one direction only: `fun_to_quoted` prints the positive arrows of each DNF clause (a `{:union, _}` fun part may
   print wider, a `{:negation, _}` one narrower; both static and dynamic fun parts: not compared). Types with a fun
   nested in a tuple/map/list are skipped (`n_print_skipped`). The message starts with the smallest failing descr
   component when one fails alone, and always ends with the full internal descr (paste into
   `Module.Types.Descr.to_quoted_string/1`). Not shrunk (`noshrink`): the module is irrelevant.
   First run: `atom() and not(:a, :b)`, atom_to_quoted({:negation, set}) puts all negated atoms in one `not` node
   (unparsable, and reads as a 2-argument call); it shows in real diagnostics
   (`def f(x) when is_atom(x) and x != :a and x != :b, do: x + 1`) and in 1.20.5. It is in ~5% of cases, so only
   1 type in 12 (phash2 of the term) reports it; the others are evaluated from the quoted form with the node read as
   `not (:a or :b)` (`n_print_not_arity_masked`). With that fixed, 200k types (2.4k cases) gave no failure.
   Cost: negligible (~1 ms per case).

## Masked / skipped (counters in stats.json)

- `skip_compile_error`, `skip_compile_other`: the generated module is invalid Elixir (should be ~0).
- `skip_lost`: the VM gave no answer within 70 s or died (logged under `<out>/skips/`).
- `n_fails` (always-fails warnings), `n_fails_owned` (on a tracked line), `n_lines_ran` (lines proven to run).
- `n_nosig`: calls to functions without an `{:infer, ...}` signature (not checked).
- `n_ret_term`: checks whose expected return was `term()` (vacuous; ~45% today, mostly because variables
  bound by bare patterns are `dynamic()`).
- `n_check_precise` / `n_check_imprecise`: exact vs disjointness-only membership checks.
- Results that raised are not checked (`n_raised`). `n_warnings` counts all warnings, `n_claims` the clause-never-runs warnings oracle 3 inspects.

## Throughput

~145 cases/s per run.sh on an idle machine (3 VMs, ~3 checked calls per case); about half that with cover.

## Ideas for later rounds

- Cross-module: B calls A.f; B's warnings use A's inferred signature; compare with `infer_signatures: false`.
- Descr algebra laws (bare_* vs opt_*), operation models (map_fetch_key, tuple_fetch vs runtime).
- Structs, binaries/bitstrings, defaults; fun values as call arguments (higher-order defs).
