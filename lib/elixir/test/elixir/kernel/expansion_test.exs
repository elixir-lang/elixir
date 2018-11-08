Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ExpansionTarget do
  defmacro seventeen, do: 17
  defmacro bar, do: "bar"
end

defmodule Kernel.ExpansionTest do
  use ExUnit.Case, async: false

  describe "__block__" do
    test "expands to nil when empty" do
      assert expand(quote(do: unquote(:__block__)())) == nil
    end

    test "expands to argument when arity is 1" do
      assert expand(quote(do: unquote(:__block__)(1))) == 1
    end

    test "is recursive to argument when arity is 1" do
      expanded =
        quote do
          _ = 1
          2
        end

      assert expand(quote(do: unquote(:__block__)(_ = 1, unquote(:__block__)(2)))) == expanded
    end

    test "accumulates vars" do
      before_expansion =
        quote do
          a = 1
          a
        end

      after_expansion =
        quote do
          a = 1
          a
        end

      assert expand(before_expansion) == after_expansion
    end
  end

  describe "alias" do
    test "expand args, defines alias and returns itself" do
      alias true, as: True

      input = quote(do: alias(:hello, as: World, warn: True))
      {output, env} = expand_env(input, __ENV__)

      assert output == :hello
      assert env.aliases == [{:"Elixir.True", true}, {:"Elixir.World", :hello}]
    end

    test "invalid alias" do
      message =
        ~r"invalid value for option :as, expected a simple alias, got nested alias: Sample.Lists"

      assert_raise CompileError, message, fn ->
        expand(quote(do: alias(:lists, as: Sample.Lists)))
      end

      message = ~r"invalid argument for alias, expected a compile time atom or alias, got: 1 \+ 2"

      assert_raise CompileError, message, fn ->
        expand(quote(do: alias(1 + 2)))
      end

      message = ~r"invalid value for option :as, expected an alias, got: :foobar"

      assert_raise CompileError, message, fn ->
        expand(quote(do: alias(:lists, as: :foobar)))
      end
    end

    test "invalid expansion" do
      assert_raise CompileError, ~r"invalid alias: \"foo\.Foo\"", fn ->
        code =
          quote do
            foo = :foo
            foo.Foo
          end

        expand(code)
      end
    end

    test "raises if :as is passed to multi-alias aliases" do
      assert_raise CompileError, ~r":as option is not supported by multi-alias call", fn ->
        expand(quote(do: alias(Foo.{Bar, Baz}, as: BarBaz)))
      end
    end

    test "invalid options" do
      assert_raise CompileError, ~r"unsupported option :ops given to alias", fn ->
        expand(quote(do: alias(Foo, ops: 1)))
      end
    end
  end

  describe "__aliases__" do
    test "expands even if no alias" do
      assert expand(quote(do: World)) == :"Elixir.World"
      assert expand(quote(do: Elixir.World)) == :"Elixir.World"
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
      message = ~r"cannot import Kernel.invalid/1 because it is undefined or private"

      assert_raise CompileError, message, fn ->
        expand(quote(do: import(Kernel, only: [invalid: 1])))
      end
    end

    test "raises on invalid options" do
      message = ~r"invalid :only option for import, expected a keyword list with integer values"

      assert_raise CompileError, message, fn ->
        expand(quote(do: import(Kernel, only: [invalid: nil])))
      end

      message = ~r"invalid :except option for import, expected a keyword list with integer values"

      assert_raise CompileError, message, fn ->
        expand(quote(do: import(Kernel, except: [invalid: nil])))
      end

      message = ~r/invalid options for import, expected a keyword list, got: "invalid_options"/

      assert_raise CompileError, message, fn ->
        expand(quote(do: import(Kernel, "invalid_options")))
      end
    end

    test "raises on conflicting options" do
      message =
        ~r":only and :except can only be given together to import when :only is either :functions or :macros"

      assert_raise CompileError, message, fn ->
        expand(quote(do: import(Kernel, only: [], except: [])))
      end
    end

    test "invalid import option" do
      assert_raise CompileError, ~r"unsupported option :ops given to import", fn ->
        expand(quote(do: import(:lists, ops: 1)))
      end
    end

    test "raises for non-compile-time module" do
      assert_raise CompileError, ~r"invalid argument for import, .*, got: {:a, :tuple}", fn ->
        expand(quote(do: import({:a, :tuple})))
      end
    end
  end

  describe "require" do
    test "raises for non-compile-time module" do
      assert_raise CompileError, ~r"invalid argument for require, .*, got: {:a, :tuple}", fn ->
        expand(quote(do: require({:a, :tuple})))
      end
    end

    test "invalid options" do
      assert_raise CompileError, ~r"unsupported option :ops given to require", fn ->
        expand(quote(do: require(Foo, ops: 1)))
      end
    end
  end

  describe "=" do
    test "sets context to match" do
      assert expand(quote(do: __ENV__.context = :match)) == quote(do: :match = :match)
    end

    test "defines vars" do
      {output, env} = expand_env(quote(do: a = 1), __ENV__)
      assert output == quote(do: a = 1)
      assert Macro.Env.has_var?(env, {:a, __MODULE__})
    end

    test "does not define _" do
      {output, env} = expand_env(quote(do: _ = 1), __ENV__)
      assert output == quote(do: _ = 1)
      assert Macro.Env.vars(env) == []
    end
  end

  describe "environment macros" do
    test "__MODULE__" do
      assert expand(quote(do: __MODULE__)) == __MODULE__
    end

    test "__DIR__" do
      assert expand(quote(do: __DIR__)) == __DIR__
    end

    test "__ENV__" do
      env = %{__ENV__ | line: 0}
      assert expand_env(quote(do: __ENV__), env) == {Macro.escape(env), env}
    end

    test "__ENV__.accessor" do
      env = %{__ENV__ | line: 0}
      assert expand_env(quote(do: __ENV__.file), env) == {__ENV__.file, env}

      assert expand_env(quote(do: __ENV__.unknown), env) ==
               {quote(do: unquote(Macro.escape(env)).unknown), env}
    end
  end

  describe "vars" do
    test "expand to local call" do
      {output, env} = expand_env(quote(do: a), __ENV__)
      assert output == quote(do: a())
      assert Macro.Env.vars(env) == []
    end

    test "forces variable to exist" do
      code =
        quote do
          var!(a) = 1
          var!(a)
        end

      assert expand(code)

      message = ~r"expected \"a\" to expand to an existing variable or be part of a match"

      assert_raise CompileError, message, fn ->
        expand(quote(do: var!(a)))
      end

      message =
        ~r"expected \"a\" \(context Unknown\) to expand to an existing variable or be part of a match"

      assert_raise CompileError, message, fn ->
        expand(quote(do: var!(a, Unknown)))
      end
    end

    test "raises for _ used outside of a match" do
      assert_raise CompileError, ~r"invalid use of _", fn ->
        expand(quote(do: {1, 2, _}))
      end
    end
  end

  describe "^" do
    test "expands args" do
      before_expansion =
        quote do
          after_expansion = 1
          ^after_expansion = 1
        end

      after_expansion =
        quote do
          after_expansion = 1
          ^after_expansion = 1
        end

      assert expand(before_expansion) == after_expansion
    end

    test "raises outside match" do
      assert_raise CompileError, ~r"cannot use \^a outside of match clauses", fn ->
        expand(quote(do: ^a))
      end
    end

    test "raises without var" do
      message =
        ~r"invalid argument for unary operator \^, expected an existing variable, got: \^1"

      assert_raise CompileError, message, fn ->
        expand(quote(do: ^1 = 1))
      end
    end

    test "raises when the var is undefined" do
      assert_raise CompileError, ~r"undefined variable \^foo", fn ->
        expand(quote(do: ^foo = :foo))
      end
    end
  end

  describe "locals" do
    test "expands to remote calls" do
      assert {{:., _, [Kernel, :=~]}, _, [{:a, _, []}, {:b, _, []}]} = expand(quote(do: a =~ b))
    end

    test "in matches" do
      message = ~r"cannot find or invoke local foo/1 inside match. .+ Called as: foo\(:bar\)"

      assert_raise CompileError, message, fn ->
        expand(quote(do: foo(:bar) = :bar))
      end
    end

    test "in guards" do
      code = quote(do: fn pid when :erlang.==(pid, self) -> pid end)
      expanded_code = quote(do: fn pid when :erlang.==(pid, :erlang.self()) -> pid end)
      assert clean_meta(expand(code), [:import, :context]) == expanded_code

      message = ~r"cannot find or invoke local foo/1"

      assert_raise CompileError, message, fn ->
        expand(quote(do: fn arg when foo(arg) -> arg end))
      end
    end

    test "custom imports" do
      before_expansion =
        quote do
          import Kernel.ExpansionTarget
          seventeen()
        end

      after_expansion =
        quote do
          :"Elixir.Kernel.ExpansionTarget"
          17
        end

      assert expand(before_expansion) == after_expansion
    end
  end

  describe "tuples" do
    test "expanded as arguments" do
      assert expand(quote(do: {after_expansion = 1, a})) == quote(do: {after_expansion = 1, a()})

      assert expand(quote(do: {b, after_expansion = 1, a})) ==
               quote(do: {b(), after_expansion = 1, a()})
    end
  end

  describe "maps" do
    test "expanded as arguments" do
      assert expand(quote(do: %{a: after_expansion = 1, b: a})) ==
               quote(do: %{a: after_expansion = 1, b: a()})
    end

    test "with variables on keys" do
      ast =
        quote do
          %{(x = 1) => 1}
        end

      assert expand(ast) == ast

      ast =
        quote do
          x = 1
          %{%{^x => 1} => 2} = y()
        end

      assert expand(ast) == ast

      assert_raise CompileError,
                   ~r"cannot use pin operator \^x inside a data structure as a map key in a pattern",
                   fn ->
                     expand(
                       quote do
                         x = 1
                         %{{^x} => 1} = %{}
                       end
                     )
                   end

      assert_raise CompileError, ~r"cannot use variable x as map key inside a pattern", fn ->
        expand(quote(do: %{x => 1} = %{}))
      end

      assert_raise CompileError, ~r"undefined variable \^x", fn ->
        expand(quote(do: {x, %{^x => 1}} = %{}))
      end
    end

    test "expects key-value pairs" do
      assert_raise CompileError, ~r"expected key-value pairs in a map, got: :foo", fn ->
        expand(quote(do: unquote({:%{}, [], [:foo]})))
      end
    end
  end

  defmodule User do
    defstruct name: "", age: 0
  end

  describe "structs" do
    test "expanded as arguments" do
      assert expand(quote(do: %User{})) ==
               quote(do: %:"Elixir.Kernel.ExpansionTest.User"{age: 0, name: ""})

      assert expand(quote(do: %User{name: "john doe"})) ==
               quote(do: %:"Elixir.Kernel.ExpansionTest.User"{age: 0, name: "john doe"})
    end

    test "expects atoms" do
      expand(quote(do: %unknown{a: 1} = x))

      message = ~r"expected struct name to be a compile time atom or alias"

      assert_raise CompileError, message, fn ->
        expand(quote(do: %unknown{a: 1}))
      end

      message = ~r"expected struct name to be a compile time atom or alias"

      assert_raise CompileError, message, fn ->
        expand(quote(do: %unquote(1){a: 1}))
      end

      message = ~r"expected struct name in a match to be a compile time atom, alias or a variable"

      assert_raise CompileError, message, fn ->
        expand(quote(do: %unquote(1){a: 1} = x))
      end
    end

    test "update syntax" do
      expand(quote(do: %{%{a: 0} | a: 1}))

      assert_raise CompileError, ~r"cannot use map/struct update syntax in match", fn ->
        expand(quote(do: %{%{a: 0} | a: 1} = %{}))
      end
    end

    test "dynamic syntax expands to itself" do
      assert expand(quote(do: %x{} = 1)) == quote(do: %x{} = 1)
    end

    test "unknown ^keys in structs" do
      message = ~r"unknown key \^my_key for struct Kernel\.ExpansionTest\.User"

      assert_raise CompileError, message, fn ->
        code =
          quote do
            my_key = :my_key
            %User{^my_key => :my_value} = %{}
          end

        expand(code)
      end
    end
  end

  describe "quote" do
    test "expanded to raw forms" do
      assert expand(quote(do: quote(do: hello))) == {:{}, [], [:hello, [], __MODULE__]}
    end

    test "raises if the :context option is nil or not a compile-time module" do
      assert_raise CompileError, ~r"invalid :context for quote, .*, got: :erlang\.self\(\)", fn ->
        expand(quote(do: quote(context: self(), do: :ok)))
      end

      assert_raise CompileError, ~r"invalid :context for quote, .*, got: nil", fn ->
        expand(quote(do: quote(context: nil, do: :ok)))
      end
    end

    test "raises for missing do" do
      assert_raise CompileError, ~r"missing :do option in \"quote\"", fn ->
        expand(quote(do: quote(context: Foo)))
      end
    end

    test "raises for invalid arguments" do
      assert_raise CompileError, ~r"invalid arguments for \"quote\"", fn ->
        expand(quote(do: quote(1 + 1)))
      end
    end

    test "raises unless its options are a keyword list" do
      assert_raise CompileError, ~r"invalid options for quote, expected a keyword list", fn ->
        expand(quote(do: quote(:foo, do: :foo)))
      end
    end
  end

  describe "anonymous calls" do
    test "expands base and args" do
      assert expand(quote(do: a.(b))) == quote(do: a().(b()))
    end

    test "raises on atom base" do
      assert_raise CompileError, ~r"invalid function call :foo.()", fn ->
        expand(quote(do: :foo.(a)))
      end
    end
  end

  describe "remotes" do
    test "expands to Erlang" do
      assert expand(quote(do: Kernel.is_atom(a))) == quote(do: :erlang.is_atom(a()))
    end

    test "expands macros" do
      assert expand(quote(do: Kernel.ExpansionTest.thirteen())) == 13
    end

    test "expands receiver and args" do
      assert expand(quote(do: a.is_atom(b))) == quote(do: a().is_atom(b()))

      assert expand(quote(do: (after_expansion = :foo).is_atom(a))) ==
               quote(do: (after_expansion = :foo).is_atom(a()))
    end

    test "modules must be required for macros" do
      before_expansion =
        quote do
          require Kernel.ExpansionTarget
          Kernel.ExpansionTarget.seventeen()
        end

      after_expansion =
        quote do
          :"Elixir.Kernel.ExpansionTarget"
          17
        end

      assert expand(before_expansion) == after_expansion
    end

    test "raises when not required" do
      msg =
        ~r"you must require Kernel\.ExpansionTarget before invoking the macro Kernel\.ExpansionTarget\.seventeen/0"

      assert_raise CompileError, msg, fn ->
        expand(quote(do: Kernel.ExpansionTarget.seventeen()))
      end
    end

    test "in matches" do
      message = ~r"cannot invoke remote function Hello.fun_that_does_not_exist/0 inside a match"

      assert_raise CompileError, message, fn ->
        expand(quote(do: Hello.fun_that_does_not_exist() = :foo))
      end

      message = ~r"cannot invoke remote function :erlang.make_ref/0 inside a match"
      assert_raise CompileError, message, fn -> expand(quote(do: make_ref() = :foo)) end

      message = ~r"invalid argument for \+\+ operator inside a match"

      assert_raise CompileError, message, fn ->
        expand(quote(do: "a" ++ "b" = "ab"))
      end

      assert_raise CompileError, message, fn ->
        expand(quote(do: [1 | 2] ++ [3] = [1, 2, 3]))
      end

      assert_raise CompileError, message, fn ->
        expand(quote(do: [1] ++ 2 ++ [3] = [1, 2, 3]))
      end

      assert {:=, _, [-1, {{:., [], [:erlang, :-]}, _, [1]}]} = expand(quote(do: -1 = -1))
      assert {:=, _, [1, {{:., [], [:erlang, :+]}, _, [1]}]} = expand(quote(do: +1 = +1))

      assert {:=, _, [[{:|, _, [1, [{:|, _, [2, 3]}]]}], [1, 2, 3]]} =
               expand(quote(do: [1] ++ [2] ++ 3 = [1, 2, 3]))
    end

    test "in guards" do
      message =
        ~r"cannot invoke remote function Hello.something_that_does_not_exist/1 inside guard"

      assert_raise CompileError, message, fn ->
        expand(quote(do: fn arg when Hello.something_that_does_not_exist(arg) -> arg end))
      end

      message = ~r"cannot invoke remote function :erlang.make_ref/0 inside guard"

      assert_raise CompileError, message, fn ->
        expand(quote(do: fn arg when make_ref() -> arg end))
      end
    end
  end

  describe "comprehensions" do
    test "variables do not leak with enums" do
      before_expansion =
        quote do
          for(a <- b, do: c = 1)
          c
        end

      after_expansion =
        quote do
          for(a <- b(), do: c = 1)
          c()
        end

      assert expand(before_expansion) == after_expansion
    end

    test "variables do not leak with binaries" do
      before_expansion =
        quote do
          for(<<a <- b>>, do: c = 1)
          c
        end

      after_expansion =
        quote do
          for(<<(<<a::integer()>> <- b())>>, do: c = 1)
          c()
        end

      assert expand(before_expansion) |> clean_meta([:alignment]) == after_expansion
    end

    test "variables inside filters are available in blocks" do
      assert expand(quote(do: for(a <- b, c = a, do: c))) ==
               quote(do: for(a <- b(), c = a, do: c))
    end

    test "variables inside options do not leak" do
      before_expansion =
        quote do
          for(a <- c = b, into: [], do: 1)
          c
        end

      after_expansion =
        quote do
          for(a <- c = b(), do: 1, into: [])
          c()
        end

      assert expand(before_expansion) == after_expansion

      before_expansion =
        quote do
          for(a <- b, into: c = [], do: 1)
          c
        end

      after_expansion =
        quote do
          for(a <- b(), do: 1, into: c = [])
          c()
        end

      assert expand(before_expansion) == after_expansion
    end

    test "must start with generators" do
      assert_raise CompileError, ~r"for comprehensions must start with a generator", fn ->
        expand(quote(do: for(is_atom(:foo), do: :foo)))
      end

      assert_raise CompileError, ~r"for comprehensions must start with a generator", fn ->
        expand(quote(do: for(do: :foo)))
      end
    end

    test "requires size on binary generators" do
      message = ~r"a binary field without size is only allowed at the end of a binary pattern"

      assert_raise CompileError, message, fn ->
        expand(quote(do: for(<<x::binary <- "123">>, do: x)))
      end
    end

    test "require do option" do
      assert_raise CompileError, ~r"missing :do option in \"for\"", fn ->
        expand(quote(do: for(_ <- 1..2)))
      end
    end

    test "uniq option is boolean" do
      message = ~r":uniq option for comprehensions only accepts a boolean, got: x"

      assert_raise CompileError, message, fn ->
        expand(quote(do: for(x <- 1..2, uniq: x, do: x)))
      end
    end

    test "raise error for unknown options" do
      assert_raise CompileError, ~r"unsupported option :else given to for", fn ->
        expand(quote(do: for(_ <- 1..2, do: 1, else: 1)))
      end

      assert_raise CompileError, ~r"unsupported option :other given to for", fn ->
        expand(quote(do: for(_ <- 1..2, do: 1, other: 1)))
      end
    end
  end

  describe "with" do
    test "variables do not leak" do
      before_expansion =
        quote do
          with({foo} <- {bar}, do: baz = :ok)
          baz
        end

      after_expansion =
        quote do
          with({foo} <- {bar()}, do: baz = :ok)
          baz()
        end

      assert expand(before_expansion) == after_expansion
    end

    test "variables are available in do option" do
      before_expansion =
        quote do
          with({foo} <- {bar}, do: baz = foo)
          baz
        end

      after_expansion =
        quote do
          with({foo} <- {bar()}, do: baz = foo)
          baz()
        end

      assert expand(before_expansion) == after_expansion
    end

    test "variables inside else do not leak" do
      before_expansion =
        quote do
          with({foo} <- {bar}, do: :ok, else: (baz -> baz))
          baz
        end

      after_expansion =
        quote do
          with({foo} <- {bar()}, do: :ok, else: (baz -> baz))
          baz()
        end

      assert expand(before_expansion) == after_expansion
    end

    test "fails if \"do\" is missing" do
      assert_raise CompileError, ~r"missing :do option in \"with\"", fn ->
        expand(quote(do: with(_ <- true, [])))
      end
    end

    test "fails on invalid else option" do
      assert_raise CompileError, ~r"expected -> clauses for :else in \"with\"", fn ->
        expand(quote(do: with(_ <- true, do: :ok, else: [:error])))
      end

      assert_raise CompileError, ~r"expected -> clauses for :else in \"with\"", fn ->
        expand(quote(do: with(_ <- true, do: :ok, else: :error)))
      end
    end

    test "fails for invalid options" do
      # Only the required "do" is present alongside the unexpected option.
      assert_raise CompileError, ~r"unexpected option :foo in \"with\"", fn ->
        expand(quote(do: with(_ <- true, foo: :bar, do: :ok)))
      end

      # More options are present alongside the unexpected option.
      assert_raise CompileError, ~r"unexpected option :foo in \"with\"", fn ->
        expand(quote(do: with(_ <- true, do: :ok, else: (_ -> :ok), foo: :bar)))
      end
    end
  end

  describe "&" do
    test "keeps locals" do
      assert expand(quote(do: &unknown/2)) == {:&, [], [{:/, [], [{:unknown, [], nil}, 2]}]}
      assert expand(quote(do: &unknown(&1, &2))) == {:&, [], [{:/, [], [{:unknown, [], nil}, 2]}]}
    end

    test "expands remotes" do
      assert expand(quote(do: &List.flatten/2)) ==
               quote(do: &:"Elixir.List".flatten/2) |> clean_meta([:import, :context])

      assert expand(quote(do: &Kernel.is_atom/1)) ==
               quote(do: &:erlang.is_atom/1) |> clean_meta([:import, :context])
    end

    test "expands macros" do
      before_expansion =
        quote do
          require Kernel.ExpansionTarget
          &Kernel.ExpansionTarget.seventeen/0
        end

      after_expansion =
        quote do
          :"Elixir.Kernel.ExpansionTarget"
          fn -> 17 end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "fails on non-continuous" do
      assert_raise CompileError, ~r"capture &0 is not allowed", fn ->
        expand(quote(do: &foo(&0)))
      end

      assert_raise CompileError, ~r"capture &2 cannot be defined without &1", fn ->
        expand(quote(do: & &2))
      end

      assert_raise CompileError, ~r"capture &255 cannot be defined without &1", fn ->
        expand(quote(do: & &255))
      end
    end

    test "fails on block" do
      message = ~r"invalid args for &, block expressions are not allowed, got: \(\n  1\n  2\n\)"

      assert_raise CompileError, message, fn ->
        code =
          quote do
            &(
              1
              2
            )
          end

        expand(code)
      end
    end

    test "fails on other types" do
      message =
        ~r"invalid args for &, expected an expression in the format of &Mod.fun/arity, &local/arity or a capture containing at least one argument as &1, got: :foo"

      assert_raise CompileError, message, fn ->
        expand(quote(do: &:foo))
      end
    end

    test "fails on invalid arity" do
      message = ~r"invalid arity for &, expected a number between 0 and 255, got: 256"

      assert_raise CompileError, message, fn ->
        expand(quote(do: &Mod.fun/256))
      end
    end

    test "fails when no captures" do
      message =
        ~r"invalid args for &, expected an expression in the format of &Mod.fun/arity, &local/arity or a capture containing at least one argument as &1, got: foo()"

      assert_raise CompileError, message, fn ->
        expand(quote(do: &foo()))
      end
    end

    test "fails on nested capture" do
      assert_raise CompileError, ~r"nested captures via & are not allowed: &\(&1\)", fn ->
        expand(quote(do: &(& &1)))
      end
    end

    test "fails on integers" do
      assert_raise CompileError, ~r"unhandled &1 outside of a capture", fn ->
        expand(quote(do: &1))
      end
    end
  end

  describe "fn" do
    test "expands each clause" do
      before_expansion =
        quote do
          fn
            x -> x
            _ -> x
          end
        end

      after_expansion =
        quote do
          fn
            x -> x
            _ -> x()
          end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not share lexical scope between clauses" do
      before_expansion =
        quote do
          fn
            1 -> import List
            2 -> flatten([1, 2, 3])
          end
        end

      after_expansion =
        quote do
          fn
            1 -> :"Elixir.List"
            2 -> flatten([1, 2, 3])
          end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "expands guards" do
      assert expand(quote(do: fn x when x when __ENV__.context -> true end)) ==
               quote(do: fn x when x when :guard -> true end)
    end

    test "does not leak vars" do
      before_expansion =
        quote do
          fn x -> x end
          x
        end

      after_expansion =
        quote do
          fn x -> x end
          x()
        end

      assert expand(before_expansion) == after_expansion
    end

    test "raises on mixed arities" do
      message = ~r"cannot mix clauses with different arities in anonymous functions"

      assert_raise CompileError, message, fn ->
        code =
          quote do
            fn
              x -> x
              x, y -> x + y
            end
          end

        expand(code)
      end
    end
  end

  describe "cond" do
    test "expands each clause" do
      before_expansion =
        quote do
          cond do
            x = 1 -> x
            true -> x
          end
        end

      after_expansion =
        quote do
          cond do
            x = 1 -> x
            true -> x()
          end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not share lexical scope between clauses" do
      before_expansion =
        quote do
          cond do
            1 -> import List
            2 -> flatten([1, 2, 3])
          end
        end

      after_expansion =
        quote do
          cond do
            1 -> :"Elixir.List"
            2 -> flatten([1, 2, 3])
          end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not leaks vars on head" do
      before_expansion =
        quote do
          cond do
            x = 1 -> x
            y = 2 -> y
          end

          :erlang.+(x, y)
        end

      after_expansion =
        quote do
          cond do
            x = 1 -> x
            y = 2 -> y
          end

          :erlang.+(x(), y())
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not leak vars" do
      before_expansion =
        quote do
          cond do
            1 -> x = 1
            2 -> y = 2
          end

          :erlang.+(x, y)
        end

      after_expansion =
        quote do
          cond do
            1 -> x = 1
            2 -> y = 2
          end

          :erlang.+(x(), y())
        end

      assert expand(before_expansion) == after_expansion
    end

    test "expects exactly one do" do
      assert_raise CompileError, ~r"missing :do option in \"cond\"", fn ->
        expand(quote(do: cond([])))
      end

      assert_raise CompileError, ~r"duplicated :do clauses given for \"cond\"", fn ->
        expand(quote(do: cond(do: (x -> x), do: (y -> y))))
      end
    end

    test "expects clauses" do
      assert_raise CompileError, ~r"expected -> clauses for :do in \"cond\"", fn ->
        expand(quote(do: cond(do: :ok)))
      end

      assert_raise CompileError, ~r"expected -> clauses for :do in \"cond\"", fn ->
        expand(quote(do: cond(do: [:not, :clauses])))
      end
    end

    test "expects one argument in clauses" do
      assert_raise CompileError, ~r"expected one arg for :do clauses \(->\) in \"cond\"", fn ->
        code =
          quote do
            cond do
              _, _ -> :ok
            end
          end

        expand(code)
      end
    end

    test "raises for invalid arguments" do
      assert_raise CompileError, ~r"invalid arguments for \"cond\"", fn ->
        expand(quote(do: cond(:foo)))
      end
    end

    test "raises with invalid options" do
      assert_raise CompileError, ~r"unexpected option :foo in \"cond\"", fn ->
        expand(quote(do: cond(do: (1 -> 1), foo: :bar)))
      end
    end

    test "raises for _ in clauses" do
      message = ~r"invalid use of _ inside \"cond\"\. If you want the last clause"

      assert_raise CompileError, message, fn ->
        code =
          quote do
            cond do
              x -> x
              _ -> :raise
            end
          end

        expand(code)
      end
    end
  end

  describe "case" do
    test "expands each clause" do
      before_expansion =
        quote do
          case w do
            x -> x
            _ -> x
          end
        end

      after_expansion =
        quote do
          case w() do
            x -> x
            _ -> x()
          end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not share lexical scope between clauses" do
      before_expansion =
        quote do
          case w do
            1 -> import List
            2 -> flatten([1, 2, 3])
          end
        end

      after_expansion =
        quote do
          case w() do
            1 -> :"Elixir.List"
            2 -> flatten([1, 2, 3])
          end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "expands guards" do
      before_expansion =
        quote do
          case w do
            x when x when __ENV__.context -> true
          end
        end

      after_expansion =
        quote do
          case w() do
            x when x when :guard -> true
          end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not leaks vars on head" do
      before_expansion =
        quote do
          case w do
            x -> x
            y -> y
          end

          :erlang.+(x, y)
        end

      after_expansion =
        quote do
          case w() do
            x -> x
            y -> y
          end

          :erlang.+(x(), y())
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not leak vars" do
      before_expansion =
        quote do
          case w do
            x -> x = x
            y -> y = y
          end

          :erlang.+(x, y)
        end

      after_expansion =
        quote do
          case w() do
            x -> x = x
            y -> y = y
          end

          :erlang.+(x(), y())
        end

      assert expand(before_expansion) == after_expansion
    end

    test "expects exactly one do" do
      assert_raise CompileError, ~r"missing :do option in \"case\"", fn ->
        expand(quote(do: case(e, [])))
      end

      assert_raise CompileError, ~r"duplicated :do clauses given for \"case\"", fn ->
        expand(quote(do: case(e, do: (x -> x), do: (y -> y))))
      end
    end

    test "expects clauses" do
      assert_raise CompileError, ~r"expected -> clauses for :do in \"case\"", fn ->
        code =
          quote do
            case e do
              x
            end
          end

        expand(code)
      end

      assert_raise CompileError, ~r"expected -> clauses for :do in \"case\"", fn ->
        code =
          quote do
            case e do
              [:not, :clauses]
            end
          end

        expand(code)
      end
    end

    test "expects exactly one argument in clauses" do
      assert_raise CompileError, ~r"expected one arg for :do clauses \(->\) in \"case\"", fn ->
        code =
          quote do
            case e do
              _, _ -> :ok
            end
          end

        expand(code)
      end
    end

    test "fails with invalid arguments" do
      assert_raise CompileError, ~r"invalid arguments for \"case\"", fn ->
        expand(quote(do: case(:foo, :bar)))
      end
    end

    test "fails for invalid options" do
      assert_raise CompileError, ~r"unexpected option :foo in \"case\"", fn ->
        expand(quote(do: case(e, do: (x -> x), foo: :bar)))
      end
    end
  end

  describe "receive" do
    test "expands each clause" do
      before_expansion =
        quote do
          receive do
            x -> x
            _ -> x
          end
        end

      after_expansion =
        quote do
          receive do
            x -> x
            _ -> x()
          end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not share lexical scope between clauses" do
      before_expansion =
        quote do
          receive do
            1 -> import List
            2 -> flatten([1, 2, 3])
          end
        end

      after_expansion =
        quote do
          receive do
            1 -> :"Elixir.List"
            2 -> flatten([1, 2, 3])
          end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "expands guards" do
      before_expansion =
        quote do
          receive do
            x when x when __ENV__.context -> true
          end
        end

      after_expansion =
        quote do
          receive do
            x when x when :guard -> true
          end
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not leaks clause vars" do
      before_expansion =
        quote do
          receive do
            x -> x
            y -> y
          end

          :erlang.+(x, y)
        end

      after_expansion =
        quote do
          receive do
            x -> x
            y -> y
          end

          :erlang.+(x(), y())
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not leak vars" do
      before_expansion =
        quote do
          receive do
            x -> x = x
            y -> y = y
          end

          :erlang.+(x, y)
        end

      after_expansion =
        quote do
          receive do
            x -> x = x
            y -> y = y
          end

          :erlang.+(x(), y())
        end

      assert expand(before_expansion) == after_expansion
    end

    test "does not leak vars on after" do
      before_expansion =
        quote do
          receive do
            x -> x = x
          after
            y ->
              y
              w = y
          end

          :erlang.+(x, w)
        end

      after_expansion =
        quote do
          receive do
            x -> x = x
          after
            y() ->
              y()
              w = y()
          end

          :erlang.+(x(), w())
        end

      assert expand(before_expansion) == after_expansion
    end

    test "expects exactly one do or after" do
      assert_raise CompileError, ~r"missing :do/:after option in \"receive\"", fn ->
        expand(quote(do: receive([])))
      end

      assert_raise CompileError, ~r"duplicated :do clauses given for \"receive\"", fn ->
        expand(quote(do: receive(do: (x -> x), do: (y -> y))))
      end

      assert_raise CompileError, ~r"duplicated :after clauses given for \"receive\"", fn ->
        code =
          quote do
            receive do
              x -> x
            after
              y -> y
            after
              z -> z
            end
          end

        expand(code)
      end
    end

    test "expects clauses" do
      assert_raise CompileError, ~r"expected -> clauses for :do in \"receive\"", fn ->
        code =
          quote do
            receive do
              x
            end
          end

        expand(code)
      end

      assert_raise CompileError, ~r"expected -> clauses for :do in \"receive\"", fn ->
        code =
          quote do
            receive do
              [:not, :clauses]
            end
          end

        expand(code)
      end
    end

    test "expects on argument for do/after clauses" do
      assert_raise CompileError, ~r"expected one arg for :do clauses \(->\) in \"receive\"", fn ->
        code =
          quote do
            receive do
              _, _ -> :ok
            end
          end

        expand(code)
      end

      message = ~r"expected one arg for :after clauses \(->\) in \"receive\""

      assert_raise CompileError, message, fn ->
        code =
          quote do
            receive do
              x -> x
            after
              _, _ -> :ok
            end
          end

        expand(code)
      end
    end

    test "expects a single clause for \"after\"" do
      assert_raise CompileError, ~r"expected a single -> clause for :after in \"receive\"", fn ->
        code =
          quote do
            receive do
              x -> x
            after
              1 -> y
              2 -> z
            end
          end

        expand(code)
      end
    end

    test "raises for invalid arguments" do
      assert_raise CompileError, ~r"invalid arguments for \"receive\"", fn ->
        expand(quote(do: receive(:foo)))
      end
    end

    test "raises with invalid options" do
      assert_raise CompileError, ~r"unexpected option :foo in \"receive\"", fn ->
        expand(quote(do: receive(do: (x -> x), foo: :bar)))
      end
    end
  end

  describe "try" do
    test "expands catch" do
      before_expansion =
        quote do
          try do
            x
          catch
            x, y -> z = :erlang.+(x, y)
          end

          z
        end

      after_expansion =
        quote do
          try do
            x()
          catch
            x, y -> z = :erlang.+(x, y)
          end

          z()
        end

      assert expand(before_expansion) == after_expansion
    end

    test "expands after" do
      before_expansion =
        quote do
          try do
            x
          after
            z = y
          end

          z
        end

      after_expansion =
        quote do
          try do
            x()
          after
            z = y()
          end

          z()
        end

      assert expand(before_expansion) == after_expansion
    end

    test "expands else" do
      before_expansion =
        quote do
          try do
            x
          catch
            _, _ -> :ok
          else
            z -> z
          end

          z
        end

      after_expansion =
        quote do
          try do
            x()
          catch
            _, _ -> :ok
          else
            z -> z
          end

          z()
        end

      assert expand(before_expansion) == after_expansion
    end

    test "expands rescue" do
      before_expansion =
        quote do
          try do
            x
          rescue
            x -> x
            Error -> x
          end

          x
        end

      after_expansion =
        quote do
          try do
            x()
          rescue
            x -> x
            unquote(:in)(_, [:"Elixir.Error"]) -> x()
          end

          x()
        end

      assert expand(before_expansion) == after_expansion
    end

    test "expects more than do" do
      assert_raise CompileError, ~r"missing :catch/:rescue/:after option in \"try\"", fn ->
        code =
          quote do
            try do
              x = y
            end

            x
          end

        expand(code)
      end
    end

    test "raises if do is missing" do
      assert_raise CompileError, ~r"missing :do option in \"try\"", fn ->
        expand(quote(do: try([])))
      end
    end

    test "expects at most one clause" do
      assert_raise CompileError, ~r"duplicated :do clauses given for \"try\"", fn ->
        expand(quote(do: try(do: e, do: f)))
      end

      assert_raise CompileError, ~r"duplicated :rescue clauses given for \"try\"", fn ->
        code =
          quote do
            try do
              e
            rescue
              x -> x
            rescue
              y -> y
            end
          end

        expand(code)
      end

      assert_raise CompileError, ~r"duplicated :after clauses given for \"try\"", fn ->
        code =
          quote do
            try do
              e
            after
              x = y
            after
              x = y
            end
          end

        expand(code)
      end

      assert_raise CompileError, ~r"duplicated :else clauses given for \"try\"", fn ->
        code =
          quote do
            try do
              e
            else
              x -> x
            else
              y -> y
            end
          end

        expand(code)
      end

      assert_raise CompileError, ~r"duplicated :catch clauses given for \"try\"", fn ->
        code =
          quote do
            try do
              e
            catch
              x -> x
            catch
              y -> y
            end
          end

        expand(code)
      end
    end

    test "raises with invalid arguments" do
      assert_raise CompileError, ~r"invalid arguments for \"try\"", fn ->
        expand(quote(do: try(:foo)))
      end
    end

    test "raises with invalid options" do
      assert_raise CompileError, ~r"unexpected option :foo in \"try\"", fn ->
        expand(quote(do: try(do: x, foo: :bar)))
      end
    end

    test "expects exactly one argument in rescue clauses" do
      assert_raise CompileError, ~r"expected one arg for :rescue clauses \(->\) in \"try\"", fn ->
        code =
          quote do
            try do
              x
            rescue
              _, _ -> :ok
            end
          end

        expand(code)
      end
    end

    test "expects an alias, a variable, or \"var in [alias]\" as the argument of rescue clauses" do
      assert_raise CompileError, ~r"invalid \"rescue\" clause\. The clause should match", fn ->
        code =
          quote do
            try do
              x
            rescue
              function(:call) -> :ok
            end
          end

        expand(code)
      end
    end

    test "expects one or two args for catch clauses" do
      message = ~r"expected one or two args for :catch clauses \(->\) in \"try\""

      assert_raise CompileError, message, fn ->
        code =
          quote do
            try do
              x
            catch
              _, _, _ -> :ok
            end
          end

        expand(code)
      end
    end

    test "expects clauses for rescue, else, catch" do
      assert_raise CompileError, ~r"expected -> clauses for :rescue in \"try\"", fn ->
        code =
          quote do
            try do
              e
            rescue
              x
            end
          end

        expand(code)
      end

      assert_raise CompileError, ~r"expected -> clauses for :rescue in \"try\"", fn ->
        code =
          quote do
            try do
              e
            rescue
              [:not, :clauses]
            end
          end

        expand(code)
      end

      assert_raise CompileError, ~r"expected -> clauses for :catch in \"try\"", fn ->
        code =
          quote do
            try do
              e
            catch
              x
            end
          end

        expand(code)
      end

      assert_raise CompileError, ~r"expected -> clauses for :catch in \"try\"", fn ->
        code =
          quote do
            try do
              e
            catch
              [:not, :clauses]
            end
          end

        expand(code)
      end

      assert_raise CompileError, ~r"expected -> clauses for :else in \"try\"", fn ->
        code =
          quote do
            try do
              e
            catch
              _ -> :ok
            else
              x
            end
          end

        expand(code)
      end

      assert_raise CompileError, ~r"expected -> clauses for :else in \"try\"", fn ->
        code =
          quote do
            try do
              e
            catch
              _ -> :ok
            else
              [:not, :clauses]
            end
          end

        expand(code)
      end
    end
  end

  describe "bitstrings" do
    test "parallel match" do
      assert expand(quote(do: <<foo>> = <<bar>>)) |> clean_meta([:alignment]) ==
               quote(do: <<foo::integer()>> = <<bar()::integer()>>)

      assert expand(quote(do: <<foo>> = baz = <<bar>>)) |> clean_meta([:alignment]) ==
               quote(do: <<foo::integer()>> = baz = <<bar()::integer()>>)

      assert expand(quote(do: <<foo>> = {<<baz>>} = bar())) |> clean_meta([:alignment]) ==
               quote(do: <<foo::integer()>> = {<<baz::integer()>>} = bar())

      message = ~r"binary patterns cannot be matched in parallel using \"=\""

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<foo>> = <<baz>> = bar()))
      end

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<foo>> = qux = <<baz>> = bar()))
      end

      assert_raise CompileError, message, fn ->
        expand(quote(do: {<<foo>>} = {qux} = {<<baz>>} = bar()))
      end

      assert expand(quote(do: {:foo, <<foo>>} = {<<baz>>, :baz} = bar()))

      # two-element tuples are special cased
      assert_raise CompileError, message, fn ->
        expand(quote(do: {:foo, <<foo>>} = {:foo, <<baz>>} = bar()))
      end

      assert_raise CompileError, message, fn ->
        expand(quote(do: %{foo: <<foo>>} = %{baz: <<qux>>, foo: <<baz>>} = bar()))
      end

      assert expand(quote(do: %{foo: <<foo>>} = %{baz: <<baz>>} = bar()))

      assert_raise CompileError, message, fn ->
        expand(quote(do: %_{foo: <<foo>>} = %_{foo: <<baz>>} = bar()))
      end

      assert expand(quote(do: %_{foo: <<foo>>} = %_{baz: <<baz>>} = bar()))

      assert_raise CompileError, message, fn ->
        expand(quote(do: %_{foo: <<foo>>} = %{foo: <<baz>>} = bar()))
      end

      assert expand(quote(do: %_{foo: <<foo>>} = %{baz: <<baz>>} = bar()))

      assert_raise CompileError, message, fn ->
        code =
          quote do
            case bar() do
              <<foo>> = <<baz>> -> nil
            end
          end

        expand(code)
      end

      assert_raise CompileError, message, fn ->
        code =
          quote do
            case bar() do
              <<foo>> = qux = <<baz>> -> nil
            end
          end

        expand(code)
      end

      assert_raise CompileError, message, fn ->
        code =
          quote do
            case bar() do
              [<<foo>>] = [<<baz>>] -> nil
            end
          end

        expand(code)
      end
    end

    test "nested match" do
      assert expand(quote(do: <<foo = bar>>)) |> clean_meta([:alignment]) ==
               quote(do: <<foo = bar()::integer()>>)

      assert expand(quote(do: <<?-, <<_, _::binary>> = rest()::binary>>))
             |> clean_meta([:alignment]) ==
               quote(do: <<45::integer(), <<_::integer(), _::binary()>> = rest()::binary()>>)

      message = ~r"cannot pattern match inside a bitstring that is already in match"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<bar = baz>> = foo()))
      end

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<?-, <<_, _::binary>> = rest::binary>> = foo()))
      end
    end

    test "inlines binaries inside interpolation" do
      import Kernel.ExpansionTarget

      assert expand(quote(do: "foo#{bar()}" = "foobar")) |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary(), "bar"::binary()>> = "foobar")
    end

    test "inlines binaries inside interpolation is isomorphic after manual expansion" do
      import Kernel.ExpansionTarget

      quoted = Macro.prewalk(quote(do: "foo#{bar()}" = "foobar"), &Macro.expand(&1, __ENV__))

      assert expand(quoted) |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary(), "bar"::binary()>> = "foobar")
    end

    test "expands size * unit" do
      import Kernel, except: [-: 2]
      import Kernel.ExpansionTarget

      assert expand(quote(do: <<x::13>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer()-size(13)>>)

      assert expand(quote(do: <<x::13*6>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer()-unit(6)-size(13)>>)

      assert expand(quote(do: <<x::_*6-binary>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary()-unit(6)>>)

      assert expand(quote(do: <<x::13*6-binary>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary()-unit(6)-size(13)>>)

      assert expand(quote(do: <<x::binary-(13 * 6)-binary>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary()-unit(6)-size(13)>>)

      assert expand(quote(do: <<x::seventeen()*2>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer()-unit(2)-size(17)>>)

      assert expand(quote(do: <<x::seventeen()*seventeen()>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer()-unit(17)-size(17)>>)

      assert expand(quote(do: <<x::_*seventeen()-binary>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary()-unit(17)>>)
    end

    test "expands binary/bitstring specifiers" do
      import Kernel, except: [-: 2]

      assert expand(quote(do: <<x::binary>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary()>>)

      assert expand(quote(do: <<x::bytes>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary()>>)

      assert expand(quote(do: <<x::bitstring>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::bitstring()>>)

      assert expand(quote(do: <<x::bits>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::bitstring()>>)

      assert expand(quote(do: <<x::binary-little>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary()>>)

      message = ~r"signed and unsigned specifiers are supported only on integer and float type"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<x()::binary-signed>>))
      end
    end

    test "expands utf* specifiers" do
      import Kernel, except: [-: 2]

      assert expand(quote(do: <<x::utf8>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::utf8()>>)

      assert expand(quote(do: <<x::utf16>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::utf16()>>)

      assert expand(quote(do: <<x::utf32-little>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::utf32()-little()>>)

      message = ~r"signed and unsigned specifiers are supported only on integer and float type"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<x()::utf8-signed>>))
      end

      assert_raise CompileError, ~r"size and unit are not supported on utf types", fn ->
        expand(quote(do: <<x()::utf8-size(32)>>))
      end
    end

    test "expands numbers specifiers" do
      import Kernel, except: [-: 2]

      assert expand(quote(do: <<x::integer>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer()>>)

      assert expand(quote(do: <<x::little>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer()-little()>>)

      assert expand(quote(do: <<x::signed>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer()-signed()>>)

      assert expand(quote(do: <<x::signed-native>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer()-native()-signed()>>)

      assert expand(quote(do: <<x::float-signed-native>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::float()-native()-signed()>>)

      message =
        ~r"integer and float types require a size specifier if the unit specifier is given"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<x::unit(8)>>))
      end
    end

    test "expands macro specifiers" do
      import Kernel, except: [-: 2]
      import Kernel.ExpansionTarget

      assert expand(quote(do: <<x::seventeen>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer()-size(17)>>)

      assert expand(quote(do: <<seventeen::seventeen, x::size(seventeen)>> = 1))
             |> clean_meta([:alignment]) ==
               quote(do: <<seventeen::integer()-size(17), x::integer()-size(seventeen)>> = 1)
    end

    test "expands macro in args" do
      import Kernel, except: [-: 2]

      before_expansion =
        quote do
          require Kernel.ExpansionTarget
          <<x::size(Kernel.ExpansionTarget.seventeen())>>
        end

      after_expansion =
        quote do
          :"Elixir.Kernel.ExpansionTarget"
          <<x()::integer()-size(17)>>
        end

      assert expand(before_expansion) |> clean_meta([:alignment]) == after_expansion
    end

    test "supports dynamic size" do
      import Kernel, except: [-: 2]

      before_expansion =
        quote do
          var = 1
          <<x::size(var)-unit(8)>>
        end

      after_expansion =
        quote do
          var = 1
          <<x()::integer()-unit(8)-size(var)>>
        end

      assert expand(before_expansion) |> clean_meta([:alignment]) == after_expansion
    end

    test "merges bitstrings" do
      import Kernel, except: [-: 2]

      assert expand(quote(do: <<x, <<y::signed-native>>, z>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer(), y()::integer()-native()-signed(), z()::integer()>>)

      assert expand(quote(do: <<x, <<y::signed-native>>::bitstring, z>>))
             |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer(), y()::integer()-native()-signed(), z()::integer()>>)
    end

    test "merges binaries" do
      import Kernel, except: [-: 2]

      assert expand(quote(do: "foo" <> x)) |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary(), x()::binary()>>)

      assert expand(quote(do: "foo" <> <<x::size(4), y::size(4)>>)) |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary(), x()::integer()-size(4), y()::integer()-size(4)>>)

      assert expand(quote(do: <<"foo", <<x::size(4), y::size(4)>>::binary>>))
             |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary(), x()::integer()-size(4), y()::integer()-size(4)>>)
    end

    test "raises on unaligned binaries in match" do
      message = ~r"cannot verify size of binary expression in match"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<rest::bits>> <> _ = "foo"))
      end

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<rest::size(3)>> <> _ = "foo"))
      end
    end

    test "raises on size or unit for literal bitstrings" do
      message = ~r"literal <<>> in bitstring supports only type specifiers"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<(<<"foo">>)::32>>))
      end
    end

    test "raises on size or unit for literal strings" do
      message = ~r"literal string in bitstring supports only endianness and type specifiers"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<"foo"::32>>))
      end
    end

    test "raises for invalid size * unit for floats" do
      message = ~r"float requires size\*unit to be 32 or 64 \(default\), got: 128"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<12.3::32*4>>))
      end

      message = ~r"float requires size\*unit to be 32 or 64 \(default\), got: 256"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<12.3::256>>))
      end
    end

    test "raises for invalid size" do
      message = ~r"size in bitstring expects an integer or a variable as argument, got: :oops"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<"foo"::size(:oops)>>))
      end
    end

    test "raises for invalid unit" do
      message = ~r"unit in bitstring expects an integer as argument, got: :oops"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<"foo"::size(8)-unit(:oops)>>))
      end
    end

    test "raises for unknown specifier" do
      assert_raise CompileError, ~r"unknown bitstring specifier: unknown()", fn ->
        expand(quote(do: <<1::unknown>>))
      end
    end

    test "raises for conflicting specifiers" do
      assert_raise CompileError, ~r"conflicting endianness specification for bit field", fn ->
        expand(quote(do: <<1::little-big>>))
      end

      assert_raise CompileError, ~r"conflicting unit specification for bit field", fn ->
        expand(quote(do: <<x::bitstring-unit(2)>>))
      end
    end

    test "raises for invalid literals" do
      assert_raise CompileError, ~r"invalid literal :foo in <<>>", fn ->
        expand(quote(do: <<:foo>>))
      end

      assert_raise CompileError, ~r"invalid literal \[\] in <<>>", fn ->
        expand(quote(do: <<[]::size(8)>>))
      end
    end

    test "raises on binary fields with size in matches" do
      assert expand(quote(do: <<x::binary-size(3), y::binary>> = "foobar"))

      message = ~r"a binary field without size is only allowed at the end of a binary pattern"

      assert_raise CompileError, message, fn ->
        expand(quote(do: <<x::binary, y::binary>> = "foobar"))
      end
    end
  end

  describe "op ambiguity" do
    test "raises when a call is ambiguous" do
      message = ~r["a -1" looks like a function call but there is a variable named "a"]

      assert_raise CompileError, message, fn ->
        # We use string_to_quoted! here to avoid the formatter adding parentheses to "a -1".
        code =
          Code.string_to_quoted!("""
          a = 1
          a -1
          """)

        expand(code)
      end
    end
  end

  test "handles invalid expressions" do
    assert_raise CompileError, ~r"invalid quoted expression: {1, 2, 3}", fn ->
      expand(quote(do: unquote({1, 2, 3})))
    end

    assert_raise CompileError, ~r"invalid quoted expression: #Function<", fn ->
      expand(quote(do: unquote({:sample, fn -> nil end})))
    end

    assert_raise CompileError, ~r"invalid pattern in match", fn ->
      code =
        quote do
          x = & &1

          case true do
            x.(false) -> true
          end
        end

      expand(code)
    end

    assert_raise CompileError, ~r"invalid expression in guard", fn ->
      code =
        quote do
          x = & &1

          case true do
            true when x.(true) -> true
          end
        end

      expand(code)
    end

    assert_raise CompileError, ~r"invalid call foo\(1\)\(2\)", fn ->
      expand(quote(do: foo(1)(2)))
    end

    assert_raise CompileError, ~r"invalid call 1\.foo\(\)", fn ->
      expand(quote(do: 1.foo))
    end

    assert_raise CompileError, ~r"invalid call 0\.foo\(\)", fn ->
      expand(quote(do: __ENV__.line.foo))
    end

    assert_raise CompileError, ~r"unhandled operator ->", fn ->
      expand(quote(do: (foo -> bar)))
    end

    message = ~r/"wrong_fun" cannot handle clauses with the ->/

    assert_raise CompileError, message, fn ->
      code =
        quote do
          wrong_fun do
            _ -> :ok
          end
        end

      expand(code)
    end

    assert_raise CompileError, message, fn ->
      code =
        quote do
          wrong_fun do
            foo -> bar
          after
            :ok
          end
        end

      expand(code)
    end

    assert_raise CompileError, ~r/"length" cannot handle clauses with the ->/, fn ->
      code =
        quote do
          length do
            _ -> :ok
          end
        end

      expand(code)
    end

    assert_raise CompileError, ~r/undefined variable "foo"/, fn ->
      code =
        quote do
          fn <<_::size(foo)>> -> :ok end
        end

      expand(code)
    end

    message = ~r"size in bitstring expects an integer or a variable as argument, got: foo()"

    assert_raise CompileError, message, fn ->
      code =
        quote do
          fn <<_::size(foo())>> -> :ok end
        end

      expand(code)
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
      send(self(), {:expand_env, :elixir_expand.expand(expr, env)})
    end)

    receive do
      {:expand_env, {expr, env}} -> {clean_meta(expr, [:version]), env}
    end
  end
end
