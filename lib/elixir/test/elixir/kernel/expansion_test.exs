Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ExpansionTarget do
  defmacro seventeen, do: 17
end

defmodule Kernel.ExpansionTest do
  use ExUnit.Case, async: false

  describe "__block__" do
    test "expands to nil when empty" do
      assert expand(quote do: __block__()) == nil
    end

    test "expands to argument when arity is 1" do
      assert expand(quote do: __block__(1)) == 1
    end

    test "is recursive to argument when arity is 1" do
      assert expand(quote do: __block__(_ = 1, __block__(2))) == quote do: __block__(_ = 1, 2)
    end

    test "accumulates vars" do
      assert expand(quote(do: (a = 1; a))) == quote do: (a = 1; a)
    end
  end

  describe "alias" do
    test "expand args, defines alias and returns itself" do
      alias true, as: True

      input = quote do: (alias :hello, as: World, warn: True)
      {output, env} = expand_env(input, __ENV__)

      assert output == :hello
      assert env.aliases == [{:"Elixir.True", true}, {:"Elixir.World", :hello}]
    end

    test "invalid alias" do
      assert_raise CompileError, ~r"invalid value for keyword :as, expected a simple alias, got nested alias: Sample.Lists", fn ->
        expand(quote do: (alias :lists, as: Sample.Lists))
      end

      assert_raise CompileError, ~r"invalid argument for alias, expected a compile time atom or alias, got: 1 \+ 2", fn ->
        expand(quote do: (alias 1 + 2))
      end

      assert_raise CompileError, ~r"invalid value for keyword :as, expected an alias, got: :\"bar.baz\"", fn ->
        expand(quote do: (alias :lists, as: :"bar.baz"))
      end
    end

    test "invalid expansion" do
      assert_raise CompileError, ~r"invalid alias: \"foo\.Foo\"", fn ->
        expand(quote do: (foo = :foo; foo.Foo))
      end
    end

    test "raises if :as is passed to multi-alias aliases" do
      assert_raise CompileError, ~r":as option is not supported by multi-alias call", fn ->
        expand(quote do: (alias Foo.{Bar, Baz}, as: BarBaz))
      end
    end

    test "invalid options" do
      assert_raise CompileError, ~r"unsupported option :ops given to alias", fn ->
        expand(quote do: (alias Foo, ops: 1))
      end
    end
  end

  describe "__aliases__" do
    test "expands even if no alias" do
      assert expand(quote do: World) == :"Elixir.World"
      assert expand(quote do: Elixir.World) == :"Elixir.World"
    end

    test "expands with alias" do
      alias Hello, as: World
      assert expand_env(quote(do: World), __ENV__) |> elem(0) == :"Elixir.Hello"
    end

    test "expands with alias is recursive" do
      alias Source, as: Hello
      alias Hello, as: World
      assert expand_env(quote(do: World), __ENV__) |> elem(0) == :"Elixir.Source"
    end
  end

  describe "import" do
    test "raises on invalid macro" do
      assert_raise CompileError,
        ~r"cannot import Kernel.invalid/1 because it is undefined or private",
        fn -> expand(quote do: (import Kernel, only: [invalid: 1])) end
    end

    test "raises on invalid options" do
      assert_raise CompileError,
        ~r"invalid :only option for import, expected a keyword list with integer values",
        fn -> expand(quote do: (import Kernel, only: [invalid: nil])) end

      assert_raise CompileError,
        ~r"invalid :except option for import, expected a keyword list with integer values",
        fn -> expand(quote do: (import Kernel, except: [invalid: nil])) end
    end

    test "raises on conflicting options" do
      assert_raise CompileError,
        ~r":only and :except can only be given together to import when :only is either :functions or :macros",
        fn -> expand(quote do: (import Kernel, only: [], except: [])) end
    end

    test "invalid import option" do
      assert_raise CompileError,
        ~r"unsupported option :ops given to import",
        fn -> expand(quote do: (import :lists, [ops: 1])) end
    end

    test "raises for non-compile-time module" do
      assert_raise CompileError, ~r"invalid argument for import, .*, got: {:a, :tuple}", fn ->
        expand(quote do: (import {:a, :tuple}))
      end
    end
  end

  describe "require" do
    test "raises for non-compile-time module" do
      assert_raise CompileError, ~r"invalid argument for require, .*, got: {:a, :tuple}", fn ->
        expand(quote do: (require {:a, :tuple}))
      end
    end

    test "invalid options" do
      assert_raise CompileError, ~r"unsupported option :ops given to require", fn ->
        expand(quote do: (require Foo, ops: 1))
      end
    end
  end

  describe "=" do
    test "sets context to match" do
      assert expand(quote do: __ENV__.context = :match) == quote do: :match = :match
    end

    test "defines vars" do
      {output, env} = expand_env(quote(do: a = 1), __ENV__)
      assert output == quote(do: a = 1)
      assert {:a, __MODULE__} in env.vars
    end

    test "does not define _" do
      {output, env} = expand_env(quote(do: _ = 1), __ENV__)
      assert output == quote(do: _ = 1)
      assert env.vars == []
    end
  end

  describe "environment macros" do
    test "__MODULE__" do
      assert expand(quote do: __MODULE__) == __MODULE__
    end

    test "__DIR__" do
      assert expand(quote do: __DIR__) == __DIR__
    end

    test "__CALLER__" do
      assert expand(quote do: __CALLER__) == quote do: __CALLER__
    end

    test "__ENV__" do
      env = %{__ENV__ | line: 0}
      assert expand_env(quote(do: __ENV__), env) ==
             {{:%{}, [], Map.to_list(env)}, env}
    end

    test "__ENV__.accessor" do
      env = %{__ENV__ | line: 0}
      assert expand_env(quote(do: __ENV__.file), env) == {__ENV__.file, env}
      assert expand_env(quote(do: __ENV__.unknown), env) ==
             {quote(do: unquote({:%{}, [], Map.to_list(env)}).unknown), env}
    end
  end

  describe "vars" do
    test "expand to local call" do
      {output, env} = expand_env(quote(do: a), __ENV__)
      assert output == quote(do: a())
      assert env.vars == []
    end

    test "forces variable to exist" do
      assert expand(quote do: (var!(a) = 1; var!(a)))

      message = ~r"expected variable \"a\" to expand to an existing variable or be part of a match"
      assert_raise CompileError, message, fn -> expand(quote do: var!(a)) end

      message = ~r"expected variable \"a\" \(context Unknown\) to expand to an existing variable or be part of a match"
      assert_raise CompileError, message, fn -> expand(quote do: var!(a, Unknown)) end
    end

    test "raises for _ used outside of a match" do
      assert_raise CompileError, ~r"unbound variable _", fn ->
        expand(quote do: {1, 2, _})
      end
    end
  end

  describe "^" do
    test "expands args" do
      assert expand(quote do: (a = 1; ^a = 1)) == quote do: (a = 1; ^a = 1)
    end

    test "raises outside match" do
      assert_raise CompileError, ~r"cannot use \^a outside of match clauses", fn ->
        expand(quote do: ^a)
      end
    end

    test "raises without var" do
      assert_raise CompileError, ~r"invalid argument for unary operator \^, expected an existing variable, got: \^1", fn ->
        expand(quote do: ^1 = 1)
      end
    end

    test "raises when the var is undefined" do
      assert_raise CompileError, ~r"unbound variable \^foo", fn ->
        expand(quote do: ^foo = :foo)
      end
    end
  end

  describe "locals" do
    test "expands to remote calls" do
      assert {{:., _, [Kernel, :=~]}, _, [{:a, _, []}, {:b, _, []}]} =
            expand(quote do: a =~ b)
    end

    test "in matches" do
      assert_raise CompileError, ~r"cannot invoke local foo/1 inside match, called as: foo\(:bar\)", fn ->
        expand(quote do: foo(:bar) = :bar)
      end
    end

    test "in guards" do
      assert expand(quote(do: fn pid when :erlang.==(pid, self) -> pid end)) |> clean_meta([:import, :context]) ==
             quote(do: fn pid when :erlang.==(pid, :erlang.self()) -> pid end)

      assert_raise CompileError, ~r"cannot invoke local foo/1 inside guard, called as: foo\(arg\)", fn ->
        expand(quote do: fn arg when foo(arg) -> arg end)
      end
    end

    test "custom imports" do
      assert expand(quote do: (import Kernel.ExpansionTarget; seventeen)) ==
             quote do: (:"Elixir.Kernel.ExpansionTarget"; 17)
    end
  end

  describe "tuples" do
    test "expanded as arguments" do
      assert expand(quote(do: {a = 1, a})) == quote do: {a = 1, a()}
      assert expand(quote(do: {b, a = 1, a})) == quote do: {b(), a = 1, a()}
    end
  end

  describe "maps" do
    test "expanded as arguments" do
      assert expand(quote(do: %{a: a = 1, b: a})) == quote do: %{a: a = 1, b: a()}
    end

    test "with variables on keys" do
      assert expand(quote(do: %{x = 1 => 1})) ==
             quote(do: %{x = 1 => 1})

      assert_raise CompileError,
        ~r"illegal use of variable x inside map key match,",
        fn -> expand(quote do: (%{x => 1} = %{})) end
    end

    test "expects key-value pairs" do
      assert_raise CompileError, ~r"expected key-value pairs in a map, got: :foo", fn ->
        expand(quote do: unquote({:%{}, [], [:foo]}))
      end
    end
  end

  defmodule User do
    defstruct name: "", age: 0
  end

  describe "structs" do
    test "expanded as arguments" do
      assert expand(quote(do: %User{})) ==
             quote do: %:"Elixir.Kernel.ExpansionTest.User"{age: 0, name: ""}

      assert expand(quote(do: %User{name: "john doe"})) ==
             quote do: %:"Elixir.Kernel.ExpansionTest.User"{age: 0, name: "john doe"}
    end

    test "expects atoms" do
      expand(quote do: %unknown{a: 1} = x)

      assert_raise CompileError, ~r"expected struct name to be a compile time atom or alias", fn ->
        expand(quote do: %unknown{a: 1})
      end

      assert_raise CompileError, ~r"expected struct name to be a compile time atom or alias", fn ->
        expand(quote do: %unquote(1){a: 1})
      end

      assert_raise CompileError, ~r"expected struct name in a match to be a compile time atom, alias or a variable", fn ->
        expand(quote do: %unquote(1){a: 1} = x)
      end
    end

    test "update syntax" do
      expand(quote do: %{%{a: 0} | a: 1})

      assert_raise CompileError, ~r"cannot use map/struct update syntax in match", fn ->
        expand(quote do: %{%{a: 0} | a: 1} = %{})
      end
    end

    test "unknown ^keys in structs" do
      message = ~r"unknown key \^my_key for struct Kernel\.ExpansionTest\.User"
      assert_raise CompileError, message, fn ->
        expand(quote do
          my_key = :my_key
          %User{^my_key => :my_value} = %{}
        end)
      end
    end
  end

  describe "quote" do
    test "expanded to raw forms" do
      assert expand(quote do: (quote do: hello)) == {:{}, [], [:hello, [], __MODULE__]}
    end

    test "raises if the :context option is nil or not a compile-time module" do
      assert_raise CompileError, ~r"invalid :context for quote, .*, got: :erlang\.self\(\)", fn ->
        expand(quote do: (quote context: self(), do: :ok))
      end

      assert_raise CompileError, ~r"invalid :context for quote, .*, got: nil", fn ->
        expand(quote do: (quote context: nil, do: :ok))
      end
    end

    test "raises for missing do" do
      assert_raise CompileError, ~r"missing :do option in \"quote\"", fn ->
        expand(quote do: (quote context: Foo))
      end
    end

    test "raises for invalid arguments" do
      assert_raise CompileError, ~r"invalid arguments for \"quote\"", fn ->
        expand(quote do: (quote 1 + 1))
      end
    end

    test "raises unless its options are a keyword list" do
      assert_raise CompileError, ~r"invalid options for quote, expected a keyword list", fn ->
        expand(quote do: (quote :foo, do: :foo))
      end
    end
  end

  describe "anonymous calls" do
    test "expands base and args" do
      assert expand(quote do: a.(b)) == quote do: a().(b())
    end

    test "raises on atom base" do
      assert_raise CompileError, ~r"invalid function call :foo.()", fn ->
        expand(quote do: :foo.(a))
      end
    end
  end

  describe "remotes" do
    test "expands to Erlang" do
      assert expand(quote do: Kernel.is_atom(a)) == quote do: :erlang.is_atom(a())
    end

    test "expands macros" do
      assert expand(quote do: Kernel.ExpansionTest.thirteen) == 13
    end

    test "expands receiver and args" do
      assert expand(quote do: a.is_atom(b)) == quote do: a().is_atom(b())
      assert expand(quote do: (a = :foo).is_atom(a)) == quote do: (a = :foo).is_atom(a())
    end

    test "modules must be required for macros" do
      assert expand(quote do: (require Kernel.ExpansionTarget; Kernel.ExpansionTarget.seventeen)) ==
             quote do: (:"Elixir.Kernel.ExpansionTarget"; 17)
    end

    test "raises when not required" do
      msg = ~r"you must require Kernel\.ExpansionTarget before invoking the macro Kernel\.ExpansionTarget\.seventeen/0"
      assert_raise CompileError, msg, fn ->
        expand(quote do: Kernel.ExpansionTarget.seventeen)
      end
    end

    test "in matches" do
      assert_raise CompileError,
        ~r"cannot invoke remote function Hello.something_that_does_not_exist/0 inside match",
        fn -> expand(quote(do: Hello.something_that_does_not_exist() = :foo)) end

      assert_raise CompileError,
        ~r"cannot invoke remote function :erlang.make_ref/0 inside match",
        fn -> expand(quote(do: make_ref() = :foo)) end
    end

    test "in guards" do
      assert_raise CompileError,
        ~r"cannot invoke remote function Hello.something_that_does_not_exist/1 inside guard",
        fn -> expand(quote do: fn arg when Hello.something_that_does_not_exist(arg) -> arg end) end

      assert_raise CompileError,
        ~r"cannot invoke remote function :erlang.make_ref/0 inside guard",
        fn -> expand(quote do: fn arg when make_ref() -> arg end) end
    end
  end

  describe "comprehensions" do
    test "variables do not leak with enums" do
      assert expand(quote do: (for(a <- b, do: c = 1); c)) ==
             quote do: (for(a <- b(), do: c = 1); c())
    end

    test "variables do not leak with binaries" do
      assert expand(quote do: (for(<<a <- b>>, do: c = 1); c)) ==
             quote do: (for(<< <<a>> <- b() >>, do: c = 1); c())
    end

    test "variables inside filters are available in blocks" do
      assert expand(quote do: for(a <- b, c = a, do: c)) ==
             quote do: (for(a <- b(), c = a, do: c))
    end

    test "variables inside options do not leak" do
      assert expand(quote do: (for(a <- c = b, into: [], do: 1); c)) ==
             quote do: (for(a <- c = b(), do: 1, into: []); c())

      assert expand(quote do: (for(a <- b, into: c = [], do: 1); c)) ==
             quote do: (for(a <- b(), do: 1, into: c = []); c())
    end

    test "must start with generators" do
      assert_raise CompileError, ~r"for comprehensions must start with a generator", fn ->
        expand(quote do: (for is_atom(:foo), do: :foo))
      end

      assert_raise CompileError, ~r"for comprehensions must start with a generator", fn ->
        expand(quote do: (for do: :foo))
      end
    end

    test "require do keyword" do
      assert_raise CompileError,
        ~r"missing do keyword in for comprehension",
        fn -> expand(quote do: for x <- 1..2) end
    end
  end

  describe "with" do
    test "variables do not leak" do
      input = quote(do: (with({foo} <- {bar}, do: baz = :ok); baz))
      other = Macro.var(:other, Elixir)
      result = quote do
        case {bar()} do
          {foo} -> baz = :ok
          unquote(other) -> unquote(other)
        end
        baz()
      end

      assert input |> expand() |> clean_meta([:export_vars, :generated, :location]) == result
    end

    test "variables are available in do option" do
      input = quote(do: (with({foo} <- {bar}, do: baz = foo); baz))
      other = Macro.var(:other, Elixir)
      result = quote do
        case {bar()} do
          {foo} -> baz = foo
          unquote(other) -> unquote(other)
        end
        baz()
      end

      assert input |> expand() |> clean_meta([:export_vars, :generated, :location]) == result
    end

    test "variables inside else do not leak" do
      input = quote(do: (with({foo} <- {bar}, do: :ok, else: (baz -> baz)); baz))
      other = Macro.var(:other, Elixir)
      return = Macro.var(:return, Elixir)
      result = quote do
        case(case {bar()} do
          {foo} -> {:ok, :ok}
          unquote(other) -> {:error, unquote(other)}
        end) do
          {:ok, unquote(return)} -> unquote(return)
          {:error, baz} -> baz
          {:error, unquote(other)} -> :erlang.error({:with_clause, unquote(other)})
        end
        baz()
      end

      assert input |> expand() |> clean_meta([:export_vars, :generated, :location]) == result
    end

    test "fails if \"do\" is missing" do
      assert_raise CompileError, ~r"missing :do option in \"with\"", fn ->
        expand(quote do: with(_ <- true, []))
      end
    end

    test "fails on invalid else option" do
      assert_raise CompileError, ~r"expected -> clauses for :else in \"with\"", fn ->
        expand(quote(do: with(_ <- true, do: :ok, else: [:error])))
      end

      assert_raise CompileError, ~r"expected -> clauses for :else in \"with\"", fn ->
        expand(quote(do: with(_ <- true, do: :ok, else: ())))
      end
    end

    test "fails for invalid options" do
      # Only the required "do" is present alongside the unexpected option.
      assert_raise CompileError, ~r"unexpected option :foo in \"with\"", fn ->
        expand(quote do: with(_ <- true, foo: :bar, do: :ok))
      end

      # More options are present alongside the unexpected option.
      assert_raise CompileError, ~r"unexpected option :foo in \"with\"", fn ->
        expand(quote do: with(_ <- true, do: :ok, else: (_ -> :ok), foo: :bar))
      end
    end
  end

  describe "&" do
    test "keeps locals" do
      assert expand(quote do: &unknown/2) ==
             {:&, [], [{:/, [], [{:unknown, [], nil}, 2]}]}
      assert expand(quote do: &unknown(&1, &2)) ==
             {:&, [], [{:/, [], [{:unknown, [], nil}, 2]}]}
    end

    test "expands remotes" do
      assert expand(quote do: &List.flatten/2) ==
             quote(do: &:"Elixir.List".flatten/2) |> clean_meta([:import, :context])

      assert expand(quote do: &Kernel.is_atom/1) ==
             quote(do: &:erlang.is_atom/1) |> clean_meta([:import, :context])
    end

    test "expands macros" do
      assert expand(quote do: (require Kernel.ExpansionTarget; &Kernel.ExpansionTarget.seventeen/0)) ==
             quote do: (:"Elixir.Kernel.ExpansionTarget"; fn -> 17 end)
    end

    test "fails on non-continuous" do
      assert_raise CompileError,
        ~r"capture &0 is not allowed",
        fn -> expand(quote do: &foo(&0)) end
      assert_raise CompileError,
        ~r"capture &2 cannot be defined without &1",
        fn -> expand(quote do: &(&2)) end
      assert_raise CompileError,
        ~r"capture &255 cannot be defined without &1",
        fn -> expand(quote do: &(&255)) end
    end

    test "fails on block" do
      assert_raise CompileError,
        ~r"invalid args for &, block expressions are not allowed, got: \(\n  1\n  2\n\)",
        fn -> expand(quote do: &(1;2)) end
    end

    test "fails on other types" do
      assert_raise CompileError,
        ~r"invalid args for &, expected an expression in the format of &Mod.fun/arity, &local/arity or a capture containing at least one argument as &1, got: :foo",
        fn -> expand(quote do: &:foo) end
    end

    test "fails on invalid arity" do
      assert_raise CompileError,
        ~r"invalid arity for &, expected a number between 0 and 255, got: 256",
        fn -> expand(quote do: &Mod.fun/256) end
    end

    test "fails when no captures" do
      assert_raise CompileError,
        ~r"invalid args for &, expected an expression in the format of &Mod.fun/arity, &local/arity or a capture containing at least one argument as &1, got: foo()",
        fn -> expand(quote do: &foo()) end
    end

    test "fails on nested capture" do
      assert_raise CompileError,
        ~r"nested captures via & are not allowed: &\(nil\)",
        fn -> expand(quote do: &(&())) end
    end

    test "fails on integers" do
      assert_raise CompileError,
        ~r"unhandled &1 outside of a capture",
        fn -> expand(quote do: &1) end
    end
  end

  describe "fn" do
    test "expands each clause" do
      assert expand(quote do: fn x -> x; _ -> x end) ==
             quote do: fn x -> x; _ -> x() end
    end

    test "does not share lexical scope between clauses" do
      assert expand(quote do: fn 1 -> import List; 2 -> flatten([1, 2, 3]) end) ==
             quote do: fn 1 -> :"Elixir.List"; 2 -> flatten([1, 2, 3]) end
    end

    test "expands guards" do
      assert expand(quote do: fn x when x when __ENV__.context -> true end) ==
             quote do: fn x when x when :guard -> true end
    end

    test "does not leak vars" do
      assert expand(quote do: (fn x -> x end; x)) ==
             quote do: (fn x -> x end; x())
    end

    test "raises on mixed arities" do
      assert_raise CompileError, ~r"cannot mix clauses with different arities in anonymous functions", fn ->
        expand(quote do: (fn x -> x; x, y -> x + y end))
      end
    end
  end

  describe "cond" do
    test "expands each clause" do
      assert expand(quote do: (cond do x = 1 -> x; true -> x end)) ==
             quote do: (cond do x = 1 -> x; true -> x() end)
    end

    test "does not share lexical scope between clauses" do
      assert expand(quote do: (cond do 1 -> import List; 2 -> flatten([1, 2, 3]) end)) ==
             quote do: (cond do 1 -> :"Elixir.List"; 2 -> flatten([1, 2, 3]) end)
    end

    test "does not leaks vars on head" do
      assert expand(quote do: (cond do x = 1 -> x; y = 2 -> y end; :erlang.+(x, y))) ==
             quote do: (cond do x = 1 -> x; y = 2 -> y end; :erlang.+(x(), y()))
    end

    test "leaks vars" do
      assert expand(quote do: (cond do 1 -> x = 1; 2 -> y = 2 end; :erlang.+(x, y))) ==
             quote do: (cond do 1 -> x = 1; 2 -> y = 2 end; :erlang.+(x, y))
    end

    test "expects exactly one do" do
      assert_raise CompileError, ~r"missing :do option in \"cond\"", fn ->
        expand(quote do: (cond []))
      end

      assert_raise CompileError, ~r"duplicated :do clauses given for \"cond\"", fn ->
        expand(quote(do: (cond do: (x -> x), do: (y -> y))))
      end
    end

    test "expects clauses" do
      assert_raise CompileError, ~r"expected -> clauses for :do in \"cond\"", fn ->
        expand(quote do: (cond do: :ok))
      end

      assert_raise CompileError, ~r"expected -> clauses for :do in \"cond\"", fn ->
        expand(quote do: (cond do: [:not, :clauses]))
      end
    end

    test "expects one argument in clauses" do
      assert_raise CompileError, ~r"expected one arg for :do clauses \(->\) in \"cond\"", fn ->
        expand(quote do: (cond do _, _ -> :ok end))
      end
    end

    test "raises for invalid arguments" do
      assert_raise CompileError, ~r"invalid arguments for \"cond\"", fn ->
        expand(quote do: (cond :foo))
      end
    end

    test "raises with invalid keywords" do
      assert_raise CompileError, ~r"unexpected option :foo in \"cond\"", fn ->
        expand(quote do: (cond do: (1 -> 1), foo: :bar))
      end
    end

    test "raises for _ in clauses" do
      assert_raise CompileError, ~r"unbound variable _ inside cond\. If you want the last clause", fn ->
        expand(quote(do: (cond do x -> x; _ -> :raise end)))
      end
    end
  end

  describe "case" do
    test "expands each clause" do
      assert expand(quote do: (case w do x -> x; _ -> x end)) ==
             quote do: (case w() do x -> x; _ -> x() end)
    end

    test "does not share lexical scope between clauses" do
      assert expand(quote do: (case w do 1 -> import List; 2 -> flatten([1, 2, 3]) end)) ==
             quote do: (case w() do 1 -> :"Elixir.List"; 2 -> flatten([1, 2, 3]) end)
    end

    test "expands guards" do
      assert expand(quote do: (case w do x when x when __ENV__.context -> true end)) ==
             quote do: (case w() do x when x when :guard -> true end)
    end

    test "does not leaks vars on head" do
      assert expand(quote do: (case w do x -> x; y -> y end; :erlang.+(x, y))) ==
             quote do: (case w() do x -> x; y -> y end; :erlang.+(x(), y()))
    end

    test "leaks vars" do
      assert expand(quote do: (case w do x -> x = x; y -> y = y end; :erlang.+(x, y))) ==
             quote do: (case w() do x -> x = x; y -> y = y end; :erlang.+(x, y))
    end

    test "expects exactly one do" do
      assert_raise CompileError, ~r"missing :do option in \"case\"", fn ->
        expand(quote(do: (case e, [])))
      end

      assert_raise CompileError, ~r"duplicated :do clauses given for \"case\"", fn ->
        expand(quote(do: (case e, do: (x -> x), do: (y -> y))))
      end
    end

    test "expects clauses" do
      assert_raise CompileError, ~r"expected -> clauses for :do in \"case\"", fn ->
        expand(quote do: (case e do x end))
      end

      assert_raise CompileError, ~r"expected -> clauses for :do in \"case\"", fn ->
        expand(quote do: (case e do [:not, :clauses] end))
      end
    end

    test "expects exactly one argument in clauses" do
      assert_raise CompileError, ~r"expected one arg for :do clauses \(->\) in \"case\"", fn ->
        expand(quote do: (case e do _, _ -> :ok end))
      end
    end

    test "fails with invalid arguments" do
      assert_raise CompileError, ~r"invalid arguments for \"case\"", fn ->
        expand(quote do: (case :foo, :bar))
      end
    end

    test "fails for invalid keywords" do
      assert_raise CompileError, ~r"unexpected option :foo in \"case\"", fn ->
        expand(quote do: (case e, do: (x -> x), foo: :bar))
      end
    end
  end

  describe "receive" do
    test "expands each clause" do
      assert expand(quote do: (receive do x -> x; _ -> x end)) ==
             quote do: (receive do x -> x; _ -> x() end)
    end

    test "does not share lexical scope between clauses" do
      assert expand(quote do: (receive do 1 -> import List; 2 -> flatten([1, 2, 3]) end)) ==
             quote do: (receive do 1 -> :"Elixir.List"; 2 -> flatten([1, 2, 3]) end)
    end

    test "expands guards" do
      assert expand(quote do: (receive do x when x when __ENV__.context -> true end)) ==
             quote do: (receive do x when x when :guard -> true end)
    end

    test "does not leaks clause vars" do
      assert expand(quote do: (receive do x -> x; y -> y end; :erlang.+(x, y))) ==
             quote do: (receive do x -> x; y -> y end; :erlang.+(x(), y()))
    end

    test "leaks vars" do
      assert expand(quote do: (receive do x -> x = x; y -> y = y end; :erlang.+(x, y))) ==
             quote do: (receive do x -> x = x; y -> y = y end; :erlang.+(x, y))
    end

    test "leaks vars on after" do
      assert expand(quote do: (receive do x -> x = x after y -> y; w = y end; :erlang.+(x, w))) ==
             quote do: (receive do x -> x = x after y() -> y(); w = y() end; :erlang.+(x, w))
    end

    test "expects exactly one do or after" do
      assert_raise CompileError, ~r"missing :do or :after in \"receive\"", fn ->
        expand(quote do: (receive []))
      end

      assert_raise CompileError, ~r"duplicated :do clauses given for \"receive\"", fn ->
        expand(quote(do: (receive do: (x -> x), do: (y -> y))))
      end

      assert_raise CompileError, ~r"duplicated :after clauses given for \"receive\"", fn ->
        expand(quote(do: (receive do x -> x after y -> y after z -> z end)))
      end
    end

    test "expects clauses" do
      assert_raise CompileError, ~r"expected -> clauses for :do in \"receive\"", fn ->
        expand(quote do: (receive do x end))
      end

      assert_raise CompileError, ~r"expected -> clauses for :do in \"receive\"", fn ->
        expand(quote do: (receive do [:not, :clauses] end))
      end
    end

    test "expects on argument for do/after clauses" do
      assert_raise CompileError, ~r"expected one arg for :do clauses \(->\) in \"receive\"", fn ->
        expand(quote do: (receive do _, _ -> :ok end))
      end

      assert_raise CompileError, ~r"expected one arg for :after clauses \(->\) in \"receive\"", fn ->
        expand(quote do: (receive do x -> x after _, _ -> :ok end))
      end
    end

    test "expects a single clause for \"after\"" do
      assert_raise CompileError, ~r"expected a single -> clause for :after in \"receive\"", fn ->
        expand(quote do: (receive do x -> x after 1 -> y; 2 -> z end))
      end
    end

    test "raises for invalid arguments" do
      assert_raise CompileError, ~r"invalid arguments for \"receive\"", fn ->
        expand(quote do: (receive :foo))
      end
    end

    test "raises with invalid keywords" do
      assert_raise CompileError, ~r"unexpected option :foo in \"receive\"", fn ->
        expand(quote do: (receive do: (x -> x), foo: :bar))
      end
    end
  end

  describe "try" do
    test "expands catch" do
      assert expand(quote do: (try do x catch x, y -> z = :erlang.+(x, y) end; z)) ==
             quote do: (try do x() catch x, y -> z = :erlang.+(x, y) end; z())
    end

    test "expands after" do
      assert expand(quote do: (try do x after z = y end; z)) ==
             quote do: (try do x() after z = y() end; z())
    end

    test "expands else" do
      assert expand(quote do: (try do x else z -> z end; z)) ==
             quote do: (try do x() else z -> z end; z())
    end

    test "expands rescue" do
      assert expand(quote do: (try do x rescue x -> x; Error -> x end; x)) ==
             quote do: (try do x() rescue x -> x; unquote(:in)(_, [:"Elixir.Error"]) -> x() end; x())
    end

    test "expects more than do" do
      assert_raise CompileError, ~r"missing :catch/:rescue/:after/:else in \"try\"", fn ->
        expand(quote do: (try do x = y end; x))
      end
    end

    test "raises if do is missing" do
      assert_raise CompileError, ~r"missing :do option in \"try\"", fn ->
        expand(quote do: (try []))
      end
    end

    test "expects at most one clause" do
      assert_raise CompileError, ~r"duplicated :do clauses given for \"try\"", fn ->
        expand(quote(do: (try do: e, do: f)))
      end

      assert_raise CompileError, ~r"duplicated :rescue clauses given for \"try\"", fn ->
        expand(quote(do: (try do e rescue x -> x rescue y -> y end)))
      end

      assert_raise CompileError, ~r"duplicated :after clauses given for \"try\"", fn ->
        expand(quote(do: (try do e after x = y after x = y end)))
      end

      assert_raise CompileError, ~r"duplicated :else clauses given for \"try\"", fn ->
        expand(quote(do: (try do e else x -> x else y -> y end)))
      end

      assert_raise CompileError, ~r"duplicated :catch clauses given for \"try\"", fn ->
        expand(quote(do: (try do e catch x -> x catch y -> y end)))
      end
    end

    test "raises with invalid arguments" do
      assert_raise CompileError, ~r"invalid arguments for \"try\"", fn ->
        expand(quote do: (try :foo))
      end
    end

    test "raises with invalid keywords" do
      assert_raise CompileError, ~r"unexpected option :foo in \"try\"", fn ->
        expand(quote do: (try do: x, foo: :bar))
      end
    end

    test "expects exactly one argument in rescue clauses" do
      assert_raise CompileError, ~r"expected one arg for :rescue clauses \(->\) in \"try\"", fn ->
        expand(quote do: (try do x rescue _, _ -> :ok end))
      end
    end

    test "expects an alias, a variable, or \"var in [alias]\" as the argument of rescue clauses" do
      assert_raise CompileError, ~r"invalid \"rescue\" clause\. The clause should match", fn ->
        expand(quote do: (try do x rescue function(:call) -> :ok end))
      end
    end

    test "expects one or two args for catch clauses" do
      assert_raise CompileError, ~r"expected one or two args for :catch clauses \(->\) in \"try\"", fn ->
        expand(quote do: (try do x catch _, _, _ -> :ok end))
      end
    end

    test "expects clauses for rescue, else, catch" do
      assert_raise CompileError, ~r"expected -> clauses for :rescue in \"try\"", fn ->
        expand(quote do: (try do e rescue x end))
      end

      assert_raise CompileError, ~r"expected -> clauses for :rescue in \"try\"", fn ->
        expand(quote do: (try do e rescue [:not, :clauses] end))
      end

      assert_raise CompileError, ~r"expected -> clauses for :catch in \"try\"", fn ->
        expand(quote do: (try do e catch x end))
      end

      assert_raise CompileError, ~r"expected -> clauses for :catch in \"try\"", fn ->
        expand(quote do: (try do e catch [:not, :clauses] end))
      end

      assert_raise CompileError, ~r"expected -> clauses for :else in \"try\"", fn ->
        expand(quote do: (try do e else x end))
      end

      assert_raise CompileError, ~r"expected -> clauses for :else in \"try\"", fn ->
        expand(quote do: (try do e else [:not, :clauses] end))
      end
    end
  end

  describe "bitstrings" do
    test "bitstrings: size * unit" do
      import Kernel, except: [-: 2]

      assert expand(quote do: <<x::13>>) ==
             quote do: <<x()::size(13)>>

      assert expand(quote do: <<x::13*6>>) ==
             quote do: <<x()::unit(6)-size(13)>>

      assert expand(quote do: <<x::_*6>>) ==
             quote do: <<x()::unit(6)>>

      assert expand(quote do: <<x::13*6-binary>>) ==
             quote do: <<x()::unit(6)-binary()-size(13) >>

      assert expand(quote do: <<x::binary-13*6>>) ==
             quote do: <<x()::binary()-unit(6)-size(13)>>
    end

    test "expands modifiers" do
      assert expand(quote do: (import Kernel.ExpansionTarget; <<x::seventeen>>)) ==
             quote do: (:"Elixir.Kernel.ExpansionTarget"; <<x()::size(17)>>)

      assert expand(quote do: (import Kernel.ExpansionTarget; <<seventeen::seventeen, x::size(seventeen)>> = 1)) ==
             quote do: (:"Elixir.Kernel.ExpansionTarget";
                        <<seventeen::size(17), x::size(seventeen)>> = 1)
    end

    test "expands modifiers args" do
      assert expand(quote do: (require Kernel.ExpansionTarget; <<x::size(Kernel.ExpansionTarget.seventeen)>>)) ==
             quote do: (:"Elixir.Kernel.ExpansionTarget"; <<x()::size(17)>>)
    end

    test "raises if a size is provided with a literal binary" do
      assert_raise CompileError, ~r"size is not supported for literal string in <<>>", fn ->
        expand(quote do: <<"foo"::binary-size(3)>>)
      end
    end

    test "raises for types that are not valid with a literal binary" do
      assert_raise CompileError, ~r"invalid types for literal string in <<>>", fn ->
        expand(quote do: <<"foo"::integer>>)
      end
    end

    test "raises for invalid literals" do
      assert_raise CompileError, ~r"invalid literal :foo in <<>>", fn ->
        expand(quote do: <<:foo>>)
      end

      assert_raise CompileError, ~r"invalid literal \[\] in <<>>", fn ->
        expand(quote do: <<[]::size(8)>>)
      end
    end
  end

  describe "op ambiguity" do
    test "raises when a call is ambiguous" do
      message = ~r["a -1" looks like a function call but there is a variable named "a"]
      assert_raise CompileError, message, fn ->
        expand(quote do: (a = 1; a -1))
      end
    end
  end

  test "handles invalid expressions" do
    assert_raise CompileError, ~r"invalid quoted expression: {1, 2, 3}", fn ->
      expand(quote do: unquote({1, 2, 3}))
    end

    assert_raise CompileError, ~r"invalid quoted expression: #Function<", fn ->
      expand(quote do: unquote({:sample, fn -> nil end}))
    end

    assert_raise CompileError, ~r"invalid pattern in match", fn ->
      expand(quote do
        case true do
          true && true -> true
        end
      end)
    end

    assert_raise CompileError, ~r"invalid call foo\(1\)\(2\)", fn ->
      expand(quote do: foo(1)(2))
    end

    assert_raise CompileError, ~r"invalid call 1\.foo\(\)", fn ->
      expand(quote do: 1.foo)
    end

    assert_raise CompileError, ~r"unhandled operator ->", fn ->
      expand(quote do: (foo -> bar))
    end
  end

  ## Helpers

  defmacro thirteen do
    13
  end

  defp clean_meta(expr, vars) do
    cleaner = &Keyword.drop(&1, vars)
    Macro.prewalk(expr, &Macro.update_meta(&1, cleaner))
  end

  defp expand(expr) do
    expand_env(expr, __ENV__) |> elem(0)
  end

  defp expand_env(expr, env) do
    ExUnit.CaptureIO.capture_io(:stderr, fn ->
      send self(), {:expand_env, :elixir_expand.expand(expr, env)}
    end)
    receive do
      {:expand_env, result} -> result
    end
  end
end
