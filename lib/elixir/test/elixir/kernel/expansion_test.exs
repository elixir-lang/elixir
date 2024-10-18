Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ExpansionTarget do
  defmacro seventeen, do: 17
  defmacro bar, do: "bar"

  defmacro message_hello(arg) do
    send(self(), :hello)
    arg
  end
end

defmodule Kernel.ExpansionTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

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

      assert_compile_error(message, fn ->
        expand(quote(do: alias(:lists, as: Sample.Lists)))
      end)

      message = ~r"invalid argument for alias, expected a compile time atom or alias, got: 1 \+ 2"

      assert_compile_error(message, fn ->
        expand(quote(do: alias(1 + 2)))
      end)

      message = ~r"invalid value for option :as, expected an alias, got: :foobar"

      assert_compile_error(message, fn ->
        expand(quote(do: alias(:lists, as: :foobar)))
      end)

      message = ~r"invalid value for option :as, expected an alias, got: :\"Elixir.foobar\""

      assert_compile_error(message, fn ->
        expand(quote(do: alias(:lists, as: :"Elixir.foobar")))
      end)

      message =
        ~r"alias cannot be inferred automatically for module: :lists, please use the :as option"

      assert_compile_error(message, fn ->
        expand(quote(do: alias(:lists)))
      end)
    end

    test "invalid expansion" do
      assert_compile_error(~r"invalid alias: \"foo\.Foo\"", fn ->
        code =
          quote do
            foo = :foo
            foo.Foo
          end

        expand(code)
      end)
    end

    test "raises if :as is passed to multi-alias aliases" do
      assert_compile_error(~r":as option is not supported by multi-alias call", fn ->
        expand(quote(do: alias(Foo.{Bar, Baz}, as: BarBaz)))
      end)
    end

    test "invalid options" do
      assert_compile_error(~r"unsupported option :ops given to alias", fn ->
        expand(quote(do: alias(Foo, ops: 1)))
      end)
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
    test "raises on conflicting options" do
      message =
        ~r":only and :except can only be given together to import when :only is :functions, :macros, or :sigils"

      assert_compile_error(message, fn ->
        expand(quote(do: import(Kernel, only: [], except: [])))
      end)
    end

    test "invalid import option" do
      assert_compile_error(~r"unsupported option :ops given to import", fn ->
        expand(quote(do: import(:lists, ops: 1)))
      end)
    end

    test "raises for non-compile-time module" do
      assert_compile_error(~r"invalid argument for import, .*, got: {:a, :tuple}", fn ->
        expand(quote(do: import({:a, :tuple})))
      end)
    end
  end

  describe "require" do
    test "raises for non-compile-time module" do
      assert_compile_error(~r"invalid argument for require, .*, got: {:a, :tuple}", fn ->
        expand(quote(do: require({:a, :tuple})))
      end)
    end

    test "invalid options" do
      assert_compile_error(~r"unsupported option :ops given to require", fn ->
        expand(quote(do: require(Foo, ops: 1)))
      end)
    end
  end

  describe "=" do
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

    test "errors on directly recursive definitions" do
      assert_compile_error(
        ~r"""
        recursive variable definition in patterns:

            x = x

        the variable "x" \(context Kernel.ExpansionTest\) is defined in function of itself
        """,
        fn -> expand(quote(do: (x = x) = :ok)) end
      )

      assert_compile_error(
        ~r"""
        recursive variable definition in patterns:

            \{x = \{:ok, x\}\}

        the variable "x" \(context Kernel.ExpansionTest\) is defined in function of itself
        """,
        fn -> expand(quote(do: {x = {:ok, x}} = :ok)) end
      )

      assert_compile_error(
        ~r"""
        recursive variable definition in patterns:

            \{\{x, y\} = \{y, x\}\}

        the variable "x" \(context Kernel.ExpansionTest\) is defined in function of itself
        """,
        fn -> expand(quote(do: {{x, y} = {y, x}} = :ok)) end
      )

      assert_compile_error(
        ~r"""
        recursive variable definition in patterns:

            \{\{:x, y\} = \{x, :y\}, x = y\}

        the variable "x" \(context Kernel.ExpansionTest\) is defined recursively in function of "y" \(context Kernel.ExpansionTest\)
        """,
        fn -> expand(quote(do: {{:x, y} = {x, :y}, x = y} = :ok)) end
      )

      assert_compile_error(
        ~r"""
        recursive variable definition in patterns:

            \{x = y, y = z, z = x\}

        the following variables form a cycle: "x" \(context Kernel.ExpansionTest\), "y" \(context Kernel.ExpansionTest\), "z" \(context Kernel.ExpansionTest\)
        """,
        fn -> expand(quote(do: {x = y, y = z, z = x} = :ok)) end
      )
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
      assert %{lexical_tracker: nil, tracers: []} = __ENV__
    end

    test "__ENV__.accessor" do
      env = %{__ENV__ | line: 0}
      assert expand_env(quote(do: __ENV__.file), env) == {__ENV__.file, env}

      assert expand_env(quote(do: __ENV__.unknown), env) ==
               {quote(do: unquote(Macro.escape(env)).unknown), env}

      assert __ENV__.lexical_tracker == nil
      assert __ENV__.tracers == []
    end

    test "on match" do
      assert_compile_error(
        ~r"invalid pattern in match, __ENV__ is not allowed in matches",
        fn -> expand(quote(do: __ENV__ = :ok)) end
      )

      assert_compile_error(
        ~r"invalid pattern in match, __CALLER__ is not allowed in matches",
        fn -> expand(quote(do: __CALLER__ = :ok)) end
      )

      assert_compile_error(
        ~r"invalid pattern in match, __STACKTRACE__ is not allowed in matches",
        fn -> expand(quote(do: __STACKTRACE__ = :ok)) end
      )
    end
  end

  describe "vars" do
    test "raises on undefined var by default" do
      assert_compile_error(~r"undefined variable \"a\"", fn ->
        expand_env({:a, [], nil}, __ENV__, [])
      end)
    end

    test "expands vars to local call when :on_undefined_variable is :warn" do
      Code.put_compiler_option(:on_undefined_variable, :warn)

      {output, env} = expand_env({:a, [], nil}, __ENV__, [])
      assert output == {:a, [if_undefined: :warn], []}
      assert Macro.Env.vars(env) == []
    after
      Code.put_compiler_option(:on_undefined_variable, :raise)
    end

    test "expands vars to local call without warning" do
      env = __ENV__

      {output, _, env} =
        :elixir_expand.expand({:a, [if_undefined: :apply], nil}, :elixir_env.env_to_ex(env), env)

      assert output == {:a, [if_undefined: :apply], []}
      assert Macro.Env.vars(env) == []
    end

    test "raises when expanding var to local call" do
      env = __ENV__

      assert_compile_error(~r"undefined variable \"a\"", fn ->
        :elixir_expand.expand({:a, [if_undefined: :raise], nil}, :elixir_env.env_to_ex(env), env)
      end)
    end

    test "forces variable to exist" do
      code =
        quote do
          var!(a) = 1
          var!(a)
        end

      assert expand(code)

      message = ~r"undefined variable \"a\""

      assert_compile_error(message, fn ->
        expand(quote(do: var!(a)))
      end)

      message = ~r"undefined variable \"a\" \(context Unknown\)"

      assert_compile_error(message, fn ->
        expand(quote(do: var!(a, Unknown)))
      end)
    end

    test "raises for _ used outside of a match" do
      assert_compile_error(~r"invalid use of _", fn ->
        expand(quote(do: {1, 2, _}))
      end)
    end

    defmacrop var_ver(var, version) do
      quote do
        {unquote(var), [version: unquote(version)], __MODULE__}
      end
    end

    defp expand_with_version(expr) do
      env = :elixir_env.reset_vars(__ENV__)
      {expr, _, _} = :elixir_expand.expand(expr, :elixir_env.env_to_ex(env), env)
      expr
    end

    test "tracks variable version" do
      assert {:__block__, _, [{:=, _, [var_ver(:x, 0), 0]}, {:=, _, [_, var_ver(:x, 0)]}]} =
               expand_with_version(
                 quote do
                   x = 0
                   _ = x
                 end
               )

      assert {:__block__, _,
              [
                {:=, _, [var_ver(:x, 0), 0]},
                {:=, _, [_, var_ver(:x, 0)]},
                {:=, _, [var_ver(:x, 1), 1]},
                {:=, _, [_, var_ver(:x, 1)]}
              ]} =
               expand_with_version(
                 quote do
                   x = 0
                   _ = x
                   x = 1
                   _ = x
                 end
               )

      assert {:__block__, _,
              [
                {:=, _, [var_ver(:x, 0), 0]},
                {:fn, _, [{:->, _, [[var_ver(:x, 1)], {:=, _, [var_ver(:x, 2), 2]}]}]},
                {:=, _, [_, var_ver(:x, 0)]},
                {:=, _, [var_ver(:x, 3), 3]}
              ]} =
               expand_with_version(
                 quote do
                   x = 0
                   fn x -> x = 2 end
                   _ = x
                   x = 3
                 end
               )

      assert {:__block__, _,
              [
                {:=, _, [var_ver(:x, 0), 0]},
                {:case, _, [:foo, [do: [{:->, _, [[var_ver(:x, 1)], var_ver(:x, 1)]}]]]},
                {:=, _, [_, var_ver(:x, 0)]},
                {:=, _, [var_ver(:x, 2), 2]}
              ]} =
               expand_with_version(
                 quote do
                   x = 0
                   case(:foo, do: (x -> x))
                   _ = x
                   x = 2
                 end
               )
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
      assert_compile_error(~r"misplaced operator \^a", fn ->
        expand(quote(do: ^a))
      end)
    end

    test "raises without var" do
      message =
        ~r"invalid argument for unary operator \^, expected an existing variable, got: \^1"

      assert_compile_error(message, fn ->
        expand(quote(do: ^1 = 1))
      end)
    end

    test "raises when the var is undefined" do
      assert_compile_error(~r"undefined variable \^foo", fn ->
        expand(quote(do: ^foo = :foo), [])
      end)
    end
  end

  describe "locals" do
    test "expands to remote calls" do
      assert {{:., _, [Kernel, :=~]}, _, [{:a, _, []}, {:b, _, []}]} = expand(quote(do: a =~ b))
    end

    test "in matches" do
      assert_compile_error(
        ~r"cannot find or invoke local foo/1 inside a match. .+ Called as: foo\(:bar\)",
        fn ->
          expand(quote(do: foo(:bar) = :bar))
        end
      )
    end

    test "in guards" do
      code = quote(do: fn pid when :erlang.==(pid, self) -> pid end)
      expanded_code = quote(do: fn pid when :erlang.==(pid, :erlang.self()) -> pid end)
      assert clean_meta(expand(code), [:imports, :context]) == expanded_code

      assert_compile_error(~r"cannot find or invoke local foo/1", fn ->
        expand(quote(do: fn arg when foo(arg) -> arg end))
      end)
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

    test "invalid metadata" do
      assert expand({:foo, [imports: 2, context: :unknown], [1, 2]}) ==
               {:foo, [imports: 2, context: :unknown], [1, 2]}
    end
  end

  describe "floats" do
    test "cannot be 0.0 inside match" do
      assert capture_io(:stderr, fn -> expand(quote(do: 0.0 = 0.0)) end) =~
               "pattern matching on 0.0 is equivalent to matching only on +0.0 from Erlang/OTP 27+"

      assert {:=, [], [+0.0, +0.0]} = expand(quote(do: +0.0 = 0.0))
      assert {:=, [], [-0.0, +0.0]} = expand(quote(do: -0.0 = 0.0))
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

    test "with variables on keys inside patterns" do
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

      ast =
        quote do
          x = 1
          %{{^x} => 1} = %{{1} => 1}
        end

      assert expand(ast) == ast

      assert_compile_error(~r"cannot use variable x as map key inside a pattern", fn ->
        expand(quote(do: %{x => 1} = %{}))
      end)

      assert_compile_error(~r"undefined variable \^x", fn ->
        expand(quote(do: {x, %{^x => 1}} = %{}), [])
      end)
    end

    test "with binaries in keys inside patterns" do
      before_ast =
        quote do
          %{<<0>> => nil} = %{<<0>> => nil}
        end

      after_ast =
        quote do
          %{<<0::integer>> => nil} = %{<<0::integer>> => nil}
        end

      assert expand(before_ast) |> clean_meta([:alignment]) == clean_bit_modifiers(after_ast)
      assert expand(after_ast) |> clean_meta([:alignment]) == clean_bit_modifiers(after_ast)

      ast =
        quote do
          x = 8
          %{<<0::integer-size(x)>> => nil} = %{<<0::integer>> => nil}
        end

      assert expand(ast) |> clean_meta([:alignment]) ==
               clean_bit_modifiers(ast) |> clean_meta([:context, :imports])

      assert_compile_error(~r"cannot use variable x as map key inside a pattern", fn ->
        expand(quote(do: %{<<x::integer>> => 1} = %{}), [])
      end)
    end

    test "expects key-value pairs" do
      assert_compile_error(~r"expected key-value pairs in a map, got: :foo", fn ->
        expand(quote(do: unquote({:%{}, [], [:foo]})))
      end)
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

      assert_compile_error(message, fn ->
        expand(quote(do: %unknown{a: 1}))
      end)

      message = ~r"expected struct name to be a compile time atom or alias"

      assert_compile_error(message, fn ->
        expand(quote(do: %unquote(1){a: 1}))
      end)

      message = ~r"expected struct name in a match to be a compile time atom, alias or a variable"

      assert_compile_error(message, fn ->
        expand(quote(do: %unquote(1){a: 1} = x))
      end)
    end

    test "update syntax" do
      expand(quote(do: %{%{a: 0} | a: 1}))

      assert_compile_error(~r"cannot use map/struct update syntax in match", fn ->
        expand(quote(do: %{%{a: 0} | a: 1} = %{}))
      end)
    end

    test "dynamic syntax expands to itself" do
      assert expand(quote(do: %x{} = 1)) == quote(do: %x{} = 1)
    end

    test "invalid keys in structs" do
      assert_compile_error(~r"invalid key :erlang\.\+\(1, 2\) for struct", fn ->
        expand(
          quote do
            %User{(1 + 2) => :my_value}
          end
        )
      end)
    end

    test "unknown key in structs" do
      message = ~r"unknown key :foo for struct Kernel\.ExpansionTest\.User"

      assert_compile_error(message, fn ->
        expand(
          quote do
            %User{foo: :my_value} = %{}
          end
        )
      end)
    end
  end

  describe "quote" do
    test "expanded to raw forms" do
      assert expand(quote(do: quote(do: hello)), []) == {:{}, [], [:hello, [], __MODULE__]}
    end

    test "raises if the :bind_quoted option is invalid" do
      assert_compile_error(~r"invalid :bind_quoted for quote", fn ->
        expand(quote(do: quote(bind_quoted: self(), do: :ok)))
      end)

      assert_compile_error(~r"invalid :bind_quoted for quote", fn ->
        expand(quote(do: quote(bind_quoted: [{1, 2}], do: :ok)))
      end)
    end

    test "raises for missing do" do
      assert_compile_error(~r"missing :do option in \"quote\"", fn ->
        expand(quote(do: quote(context: Foo)))
      end)
    end

    test "raises for invalid arguments" do
      assert_compile_error(~r"invalid arguments for \"quote\"", fn ->
        expand(quote(do: quote(1 + 1)))
      end)
    end

    test "raises unless its options are a keyword list" do
      assert_compile_error(~r"invalid options for quote, expected a keyword list", fn ->
        expand(quote(do: quote(:foo, do: :foo)))
      end)
    end
  end

  describe "anonymous calls" do
    test "expands base and args" do
      assert expand(quote(do: a.(b))) == quote(do: a().(b()))
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

    test "in matches" do
      message = ~r"cannot invoke remote function Hello.fun_that_does_not_exist/0 inside a match"

      assert_compile_error(message, fn ->
        expand(quote(do: Hello.fun_that_does_not_exist() = :foo))
      end)

      message = ~r"cannot invoke remote function :erlang.make_ref/0 inside a match"
      assert_compile_error(message, fn -> expand(quote(do: make_ref() = :foo)) end)

      message = ~r"invalid argument for \+\+ operator inside a match"

      assert_compile_error(message, fn ->
        expand(quote(do: "a" ++ "b" = "ab"))
      end)

      assert_compile_error(message, fn ->
        expand(quote(do: [1 | 2] ++ [3] = [1, 2, 3]))
      end)

      assert_compile_error(message, fn ->
        expand(quote(do: [1] ++ 2 ++ [3] = [1, 2, 3]))
      end)

      assert {:=, _, [-1, -1]} =
               expand(quote(do: -1 = -1))

      assert {:=, _, [1, 1]} =
               expand(quote(do: +1 = +1))

      assert {:=, _, [[{:|, _, [1, [{:|, _, [2, 3]}]]}], [1, 2, 3]]} =
               expand(quote(do: [1] ++ [2] ++ 3 = [1, 2, 3]))
    end

    test "in guards" do
      message =
        ~r"cannot invoke remote function Hello.something_that_does_not_exist/1 inside a guard"

      assert_compile_error(message, fn ->
        expand(quote(do: fn arg when Hello.something_that_does_not_exist(arg) -> arg end))
      end)

      message = ~r"cannot invoke remote function :erlang.make_ref/0 inside a guard"

      assert_compile_error(message, fn ->
        expand(quote(do: fn arg when make_ref() -> arg end))
      end)
    end

    test "in guards with bitstrings" do
      message = ~r"cannot invoke remote function String.Chars.to_string/1 inside a guard"

      assert_compile_error(message, fn ->
        expand(quote(do: fn arg when "#{arg}foo" == "argfoo" -> arg end))
      end)

      assert_compile_error(message, fn ->
        expand(
          quote do
            fn arg when <<:"Elixir.Kernel".to_string(arg)::binary, "foo">> == "argfoo" ->
              arg
            end
          end
        )
      end)
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
          for(<<(<<a::integer>> <- b())>>, do: c = 1)
          c()
        end

      assert expand(before_expansion) |> clean_meta([:alignment]) ==
               clean_bit_modifiers(after_expansion)
    end

    test "variables inside generator args do not leak" do
      before_expansion =
        quote do
          for(
            b <-
              (
                a = 1
                [2]
              ),
            do: {a, b}
          )

          a
        end

      after_expansion =
        quote do
          for(
            b <-
              (
                a = 1
                [2]
              ),
            do: {a(), b}
          )

          a()
        end

      assert expand(before_expansion) == after_expansion

      before_expansion =
        quote do
          for(
            b <-
              (
                a = 1
                [2]
              ),
            d <-
              (
                c = 3
                [4]
              ),
            do: {a, b, c, d}
          )
        end

      after_expansion =
        quote do
          for(
            b <-
              (
                a = 1
                [2]
              ),
            d <-
              (
                c = 3
                [4]
              ),
            do: {a(), b, c(), d},
            into: []
          )
        end

      assert expand(before_expansion) == after_expansion
    end

    test "variables inside filters are available in blocks" do
      assert expand(quote(do: for(a <- b, c = a, do: c))) ==
               quote(do: for(a <- b(), c = a, do: c, into: []))
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
      assert_compile_error(~r"for comprehensions must start with a generator", fn ->
        expand(quote(do: for(is_atom(:foo), do: :foo)))
      end)

      assert_compile_error(~r"for comprehensions must start with a generator", fn ->
        expand(quote(do: for(do: :foo)))
      end)
    end

    test "requires size on binary generators" do
      message = ~r"a binary field without size is only allowed at the end of a binary pattern"

      assert_compile_error(message, fn ->
        expand(quote(do: for(<<x::binary <- "123">>, do: x)))
      end)
    end

    test "require do option" do
      assert_compile_error(~r"missing :do option in \"for\"", fn ->
        expand(quote(do: for(_ <- 1..2)))
      end)
    end

    test "uniq option is boolean" do
      message = ~r":uniq option for comprehensions only accepts a boolean, got: x"

      assert_compile_error(message, fn ->
        expand(quote(do: for(x <- 1..2, uniq: x, do: x)))
      end)
    end

    test "raise error on invalid reduce" do
      assert_compile_error(
        ~r"cannot use :reduce alongside :into/:uniq in comprehension",
        fn ->
          expand(quote(do: for(x <- 1..3, reduce: %{}, into: %{}, do: (acc -> acc))))
        end
      )

      assert_compile_error(
        ~r"the do block was written using acc -> expr clauses but the :reduce option was not given",
        fn -> expand(quote(do: for(x <- 1..3, do: (acc -> acc)))) end
      )

      assert_compile_error(
        ~r"when using :reduce with comprehensions, the do block must be written using acc -> expr clauses",
        fn -> expand(quote(do: for(x <- 1..3, reduce: %{}, do: x))) end
      )

      assert_compile_error(
        ~r"when using :reduce with comprehensions, the do block must be written using acc -> expr clauses",
        fn -> expand(quote(do: for(x <- 1..3, reduce: %{}, do: (acc, x -> x)))) end
      )

      assert_compile_error(
        ~r"when using :reduce with comprehensions, the do block must be written using acc -> expr clauses",
        fn -> expand(quote(do: for(x <- 1..3, reduce: %{}, do: (acc, x when 1 == 1 -> x)))) end
      )
    end

    test "raise error for unknown options" do
      assert_compile_error(~r"unsupported option :else given to for", fn ->
        expand(quote(do: for(_ <- 1..2, do: 1, else: 1)))
      end)

      assert_compile_error(~r"unsupported option :other given to for", fn ->
        expand(quote(do: for(_ <- 1..2, do: 1, other: 1)))
      end)
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

    test "variables inside args expression do not leak" do
      before_expansion =
        quote do
          with(
            b <-
              (
                a = 1
                2
              ),
            do: {a, b}
          )

          a
        end

      after_expansion =
        quote do
          with(
            b <-
              (
                a = 1
                2
              ),
            do: {a(), b}
          )

          a()
        end

      assert expand(before_expansion) == after_expansion

      before_expansion =
        quote do
          with(
            b <-
              (
                a = 1
                2
              ),
            d <-
              (
                c = 3
                4
              ),
            do: {a, b, c, d}
          )
        end

      after_expansion =
        quote do
          with(
            b <-
              (
                a = 1
                2
              ),
            d <-
              (
                c = 3
                4
              ),
            do: {a(), b, c(), d}
          )
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
      assert_compile_error(~r"missing :do option in \"with\"", fn ->
        expand(quote(do: with(_ <- true, [])))
      end)
    end

    test "fails on invalid else option" do
      assert_compile_error(~r"expected -> clauses for :else in \"with\"", fn ->
        expand(quote(do: with(_ <- true, do: :ok, else: [:error])))
      end)

      assert_compile_error(~r"expected -> clauses for :else in \"with\"", fn ->
        expand(quote(do: with(_ <- true, do: :ok, else: :error)))
      end)

      assert_compile_error(~r"expected -> clauses for :else in \"with\"", fn ->
        expand(quote(do: with(_ <- true, do: :ok, else: [])))
      end)
    end

    test "fails for invalid options" do
      # Only the required "do" is present alongside the unexpected option.
      assert_compile_error(~r"unexpected option :foo in \"with\"", fn ->
        expand(quote(do: with(_ <- true, foo: :bar, do: :ok)))
      end)

      # More options are present alongside the unexpected option.
      assert_compile_error(~r"unexpected option :foo in \"with\"", fn ->
        expand(quote(do: with(_ <- true, do: :ok, else: (_ -> :ok), foo: :bar)))
      end)

      assert_compile_error(~r"unexpected option :foo in \"with\"", fn ->
        expand(
          quote do
            with _ <- true, foo: :bar do
              :ok
            end
          end
        )
      end)
    end
  end

  describe "&" do
    test "keeps locals" do
      assert expand(quote(do: &unknown/2)) == {:&, [], [{:/, [], [{:unknown, [], nil}, 2]}]}
      assert expand(quote(do: &unknown(&1, &2))) == {:&, [], [{:/, [], [{:unknown, [], nil}, 2]}]}
    end

    test "keeps position meta on & variables" do
      assert expand(Code.string_to_quoted!("& &1")) |> clean_meta([:counter]) ==
               {:fn, [{:line, 1}],
                [{:->, [{:line, 1}], [[{:capture, [line: 1], nil}], {:capture, [line: 1], nil}]}]}
    end

    test "expands remotes" do
      assert expand(quote(do: &List.flatten/2)) ==
               quote(do: &:"Elixir.List".flatten/2)
               |> clean_meta([:imports, :context])

      assert expand(quote(do: &Kernel.is_atom/1)) ==
               quote(do: &:erlang.is_atom/1) |> clean_meta([:imports, :context])
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

      assert clean_meta(expand(before_expansion), [:imports, :context, :no_parens]) ==
               after_expansion
    end

    test "fails on non-continuous" do
      assert_compile_error(~r"capture argument &0 must be numbered between 1 and 255", fn ->
        expand(quote(do: &foo(&0)))
      end)

      assert_compile_error(~r"capture argument &2 cannot be defined without &1", fn ->
        expand(quote(do: & &2))
      end)

      assert_compile_error(~r"capture argument &255 cannot be defined without &1", fn ->
        expand(quote(do: & &255))
      end)
    end

    test "fails on block" do
      message = ~r"block expressions are not allowed inside the capture operator &, got: 1\n2"

      assert_compile_error(message, fn ->
        code =
          quote do
            &(
              1
              2
            )
          end

        expand(code)
      end)
    end

    test "fails on other types" do
      assert_compile_error(~r"invalid args for &, expected one of:", fn ->
        expand(quote(do: &:foo))
      end)
    end

    test "fails on invalid arity" do
      message = ~r"capture argument &256 must be numbered between 1 and 255"

      assert_compile_error(message, fn ->
        expand(quote(do: &Mod.fun/256))
      end)
    end

    test "fails when no captures" do
      assert_compile_error(~r"invalid args for &, expected one of:", fn ->
        expand(quote(do: &foo()))
      end)
    end

    test "fails on nested capture" do
      assert_compile_error(~r"nested captures are not allowed", fn ->
        expand(quote(do: &(& &1)))
      end)
    end

    test "fails on integers" do
      assert_compile_error(
        ~r"capture argument &1 must be used within the capture operator &",
        fn -> expand(quote(do: &1)) end
      )
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

      assert_compile_error(message, fn ->
        code =
          quote do
            fn
              x -> x
              x, y -> x + y
            end
          end

        expand(code)
      end)
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
      assert_compile_error(~r"missing :do option in \"cond\"", fn ->
        expand(quote(do: cond([])))
      end)

      assert_compile_error(~r"duplicate :do clauses given for \"cond\"", fn ->
        expand(quote(do: cond(do: (x -> x), do: (y -> y))))
      end)
    end

    test "expects clauses" do
      assert_compile_error(~r"expected -> clauses for :do in \"cond\"", fn ->
        expand(quote(do: cond(do: :ok)))
      end)

      assert_compile_error(~r"expected -> clauses for :do in \"cond\"", fn ->
        expand(quote(do: cond(do: [:not, :clauses])))
      end)
    end

    test "expects one argument in clauses" do
      assert_compile_error(
        ~r"expected one argument for :do clauses \(->\) in \"cond\"",
        fn ->
          code =
            quote do
              cond do
                _, _ -> :ok
              end
            end

          expand(code)
        end
      )
    end

    test "raises for invalid arguments" do
      assert_compile_error(~r"invalid arguments for \"cond\"", fn ->
        expand(quote(do: cond(:foo)))
      end)
    end

    test "raises with invalid options" do
      assert_compile_error(~r"unexpected option :foo in \"cond\"", fn ->
        expand(quote(do: cond(do: (1 -> 1), foo: :bar)))
      end)
    end

    test "raises for _ in clauses" do
      message = ~r"invalid use of _ inside \"cond\"\. If you want the last clause"

      assert_compile_error(message, fn ->
        code =
          quote do
            cond do
              x -> x
              _ -> :raise
            end
          end

        expand(code)
      end)
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
      assert_compile_error(~r"missing :do option in \"case\"", fn ->
        expand(quote(do: case(e, [])))
      end)

      assert_compile_error(~r"duplicate :do clauses given for \"case\"", fn ->
        expand(quote(do: case(e, do: (x -> x), do: (y -> y))))
      end)
    end

    test "expects clauses" do
      assert_compile_error(~r"expected -> clauses for :do in \"case\"", fn ->
        code =
          quote do
            case e do
              x
            end
          end

        expand(code)
      end)

      assert_compile_error(~r"expected -> clauses for :do in \"case\"", fn ->
        code =
          quote do
            case e do
              [:not, :clauses]
            end
          end

        expand(code)
      end)
    end

    test "expects exactly one argument in clauses" do
      assert_compile_error(
        ~r"expected one argument for :do clauses \(->\) in \"case\"",
        fn ->
          code =
            quote do
              case e do
                _, _ -> :ok
              end
            end

          expand(code)
        end
      )
    end

    test "fails with invalid arguments" do
      assert_compile_error(~r"invalid arguments for \"case\"", fn ->
        expand(quote(do: case(:foo, :bar)))
      end)
    end

    test "fails for invalid options" do
      assert_compile_error(~r"unexpected option :foo in \"case\"", fn ->
        expand(quote(do: case(e, do: (x -> x), foo: :bar)))
      end)
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
      assert_compile_error(~r"missing :do/:after option in \"receive\"", fn ->
        expand(quote(do: receive([])))
      end)

      assert_compile_error(~r"duplicate :do clauses given for \"receive\"", fn ->
        expand(quote(do: receive(do: (x -> x), do: (y -> y))))
      end)

      assert_compile_error(~r"duplicate :after clauses given for \"receive\"", fn ->
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
      end)
    end

    test "expects clauses" do
      assert_compile_error(~r"expected -> clauses for :do in \"receive\"", fn ->
        code =
          quote do
            receive do
              x
            end
          end

        expand(code)
      end)

      assert_compile_error(~r"expected -> clauses for :do in \"receive\"", fn ->
        code =
          quote do
            receive do
              [:not, :clauses]
            end
          end

        expand(code)
      end)
    end

    test "expects on argument for do/after clauses" do
      assert_compile_error(
        ~r"expected one argument for :do clauses \(->\) in \"receive\"",
        fn ->
          code =
            quote do
              receive do
                _, _ -> :ok
              end
            end

          expand(code)
        end
      )

      message = ~r"expected one argument for :after clauses \(->\) in \"receive\""

      assert_compile_error(message, fn ->
        code =
          quote do
            receive do
              x -> x
            after
              _, _ -> :ok
            end
          end

        expand(code)
      end)
    end

    test "expects a single clause for \"after\"" do
      assert_compile_error(~r"expected a single -> clause for :after in \"receive\"", fn ->
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
      end)
    end

    test "raises for invalid arguments" do
      assert_compile_error(~r"invalid arguments for \"receive\"", fn ->
        expand(quote(do: receive(:foo)))
      end)
    end

    test "raises with invalid options" do
      assert_compile_error(~r"unexpected option :foo in \"receive\"", fn ->
        expand(quote(do: receive(do: (x -> x), foo: :bar)))
      end)
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
      assert_compile_error(~r"missing :catch/:rescue/:after option in \"try\"", fn ->
        code =
          quote do
            try do
              x = y
            end

            x
          end

        expand(code)
      end)
    end

    test "raises if do is missing" do
      assert_compile_error(~r"missing :do option in \"try\"", fn ->
        expand(quote(do: try([])))
      end)
    end

    test "expects at most one clause" do
      assert_compile_error(~r"duplicate :do clauses given for \"try\"", fn ->
        expand(quote(do: try(do: e, do: f)))
      end)

      assert_compile_error(~r"duplicate :rescue clauses given for \"try\"", fn ->
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
      end)

      assert_compile_error(~r"duplicate :after clauses given for \"try\"", fn ->
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
      end)

      assert_compile_error(~r"duplicate :else clauses given for \"try\"", fn ->
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
      end)

      assert_compile_error(~r"duplicate :catch clauses given for \"try\"", fn ->
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
      end)
    end

    test "raises with invalid arguments" do
      assert_compile_error(~r"invalid arguments for \"try\"", fn ->
        expand(quote(do: try(:foo)))
      end)
    end

    test "raises with invalid options" do
      assert_compile_error(~r"unexpected option :foo in \"try\"", fn ->
        expand(quote(do: try(do: x, foo: :bar)))
      end)
    end

    test "expects exactly one argument in rescue clauses" do
      assert_compile_error(
        ~r"expected one argument for :rescue clauses \(->\) in \"try\"",
        fn ->
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
      )
    end

    test "expects an alias, a variable, or \"var in [alias]\" as the argument of rescue clauses" do
      assert_compile_error(~r"invalid \"rescue\" clause\. The clause should match", fn ->
        code =
          quote do
            try do
              x
            rescue
              function(:call) -> :ok
            end
          end

        expand(code)
      end)
    end

    test "expects one or two args for catch clauses" do
      message = ~r"expected one or two args for :catch clauses \(->\) in \"try\""

      assert_compile_error(message, fn ->
        code =
          quote do
            try do
              x
            catch
              _, _, _ -> :ok
            end
          end

        expand(code)
      end)

      assert_compile_error(message, fn ->
        code =
          quote do
            try do
              x
            catch
              _, _, _ when 1 == 1 -> :ok
            end
          end

        expand(code)
      end)
    end

    test "expects clauses for rescue, else, catch" do
      assert_compile_error(~r"expected -> clauses for :rescue in \"try\"", fn ->
        code =
          quote do
            try do
              e
            rescue
              x
            end
          end

        expand(code)
      end)

      assert_compile_error(~r"expected -> clauses for :rescue in \"try\"", fn ->
        code =
          quote do
            try do
              e
            rescue
              [:not, :clauses]
            end
          end

        expand(code)
      end)

      assert_compile_error(~r"expected -> clauses for :rescue in \"try\"", fn ->
        code =
          quote do
            try do
              e
            rescue
              []
            end
          end

        expand(code)
      end)

      assert_compile_error(~r"expected -> clauses for :catch in \"try\"", fn ->
        code =
          quote do
            try do
              e
            catch
              x
            end
          end

        expand(code)
      end)

      assert_compile_error(~r"expected -> clauses for :catch in \"try\"", fn ->
        code =
          quote do
            try do
              e
            catch
              [:not, :clauses]
            end
          end

        expand(code)
      end)

      assert_compile_error(~r"expected -> clauses for :catch in \"try\"", fn ->
        code =
          quote do
            try do
              e
            catch
              []
            end
          end

        expand(code)
      end)

      assert_compile_error(~r"expected -> clauses for :else in \"try\"", fn ->
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
      end)

      assert_compile_error(~r"expected -> clauses for :else in \"try\"", fn ->
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
      end)

      assert_compile_error(~r"expected -> clauses for :else in \"try\"", fn ->
        code =
          quote do
            try do
              e
            catch
              _ -> :ok
            else
              []
            end
          end

        expand(code)
      end)
    end
  end

  describe "bitstrings" do
    test "parallel match" do
      assert expand(quote(do: <<foo>> = <<bar>>)) |> clean_meta([:alignment]) ==
               quote(do: <<foo::integer>> = <<bar()::integer>>)
               |> clean_bit_modifiers()

      assert expand(quote(do: <<foo>> = baz = <<bar>>)) |> clean_meta([:alignment]) ==
               quote(do: <<foo::integer>> = baz = <<bar()::integer>>)
               |> clean_bit_modifiers()

      assert expand(quote(do: <<foo>> = <<bar>> = baz)) |> clean_meta([:alignment]) ==
               quote(do: <<foo::integer>> = <<bar::integer>> = baz())
               |> clean_bit_modifiers()
    end

    test "nested match" do
      assert expand(quote(do: <<foo = bar>>)) |> clean_meta([:alignment]) ==
               quote(do: <<foo = bar()::integer>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<?-, <<_, _::binary>> = rest()::binary>>))
             |> clean_meta([:alignment]) ==
               quote(do: <<45::integer, <<_::integer, _::binary>> = rest()::binary>>)
               |> clean_bit_modifiers()

      message = ~r"cannot pattern match inside a bitstring that is already in match"

      assert_compile_error(message, fn ->
        expand(quote(do: <<bar = baz>> = foo()))
      end)

      assert_compile_error(message, fn ->
        expand(quote(do: <<?-, <<_, _::binary>> = rest::binary>> = foo()))
      end)
    end

    test "inlines binaries inside interpolation" do
      import Kernel.ExpansionTarget

      # Check expansion happens only once
      assert expand(quote(do: "foo#{message_hello("bar")}")) |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary, "bar"::binary>>) |> clean_bit_modifiers()

      assert_received :hello
      refute_received :hello

      # And it also works in match
      assert expand(quote(do: "foo#{bar()}" = "foobar")) |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary, "bar"::binary>> = "foobar")
               |> clean_bit_modifiers()
    end

    test "inlines binaries inside interpolation is isomorphic after manual expansion" do
      import Kernel.ExpansionTarget

      quoted = Macro.prewalk(quote(do: "foo#{bar()}" = "foobar"), &Macro.expand(&1, __ENV__))

      assert expand(quoted) |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary, "bar"::binary>> = "foobar")
               |> clean_bit_modifiers()
    end

    test "expands size * unit" do
      import Kernel, except: [-: 1, -: 2]
      import Kernel.ExpansionTarget

      assert expand(quote(do: <<x::13>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer-size(13)>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::13*6>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer-unit(6)-size(13)>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::_*6-binary>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary-unit(6)>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::13*6-binary>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary-unit(6)-size(13)>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::binary-(13 * 6)-binary>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary-unit(6)-size(13)>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::seventeen()>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer-size(17)>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::seventeen()*2>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer-unit(2)-size(17)>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::seventeen()*seventeen()>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer-unit(17)-size(17)>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::_*seventeen()-binary>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary-unit(17)>>) |> clean_bit_modifiers()
    end

    test "expands binary/bitstring specifiers" do
      import Kernel, except: [-: 1, -: 2]

      assert expand(quote(do: <<x::binary>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::bytes>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::bitstring>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::bitstring>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::bits>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::bitstring>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::binary-little>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::binary>>) |> clean_bit_modifiers()

      message = ~r"signed and unsigned specifiers are supported only on integer and float type"

      assert_compile_error(message, fn ->
        expand(quote(do: <<x()::binary-signed>>))
      end)
    end

    test "expands utf* specifiers" do
      import Kernel, except: [-: 1, -: 2]

      assert expand(quote(do: <<x::utf8>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::utf8>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::utf16>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::utf16>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::utf32-little>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::utf32-little>>) |> clean_bit_modifiers()

      message = ~r"signed and unsigned specifiers are supported only on integer and float type"

      assert_compile_error(message, fn ->
        expand(quote(do: <<x()::utf8-signed>>))
      end)

      assert_compile_error(~r"size and unit are not supported on utf types", fn ->
        expand(quote(do: <<x()::utf8-size(32)>>))
      end)
    end

    test "expands numbers specifiers" do
      import Kernel, except: [-: 1, -: 2]

      assert expand(quote(do: <<x::integer>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::little>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer-little>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::signed>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer-signed>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::signed-native>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer-native-signed>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<x::float-signed-native>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::float-native-signed>>) |> clean_bit_modifiers()

      message =
        ~r"integer and float types require a size specifier if the unit specifier is given"

      assert_compile_error(message, fn ->
        expand(quote(do: <<x::unit(8)>>))
      end)
    end

    test "expands macro specifiers" do
      import Kernel, except: [-: 1, -: 2]
      import Kernel.ExpansionTarget

      assert expand(quote(do: <<x::seventeen()>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer-size(17)>>) |> clean_bit_modifiers()

      assert expand(quote(do: <<seventeen::seventeen(), x::size(seventeen)>> = 1))
             |> clean_meta([:alignment]) ==
               quote(do: <<seventeen::integer-size(17), x::integer-size(seventeen)>> = 1)
               |> clean_bit_modifiers()
    end

    test "expands macro in args" do
      import Kernel, except: [-: 1, -: 2]

      before_expansion =
        quote do
          require Kernel.ExpansionTarget
          <<x::size(Kernel.ExpansionTarget.seventeen())>>
        end

      after_expansion =
        quote do
          :"Elixir.Kernel.ExpansionTarget"
          <<x()::integer-size(17)>>
        end

      assert expand(before_expansion) |> clean_meta([:alignment]) ==
               clean_bit_modifiers(after_expansion)
    end

    test "supports dynamic size" do
      import Kernel, except: [-: 1, -: 2]

      before_expansion =
        quote do
          var = 1
          <<x::size(var)-unit(8)>>
        end

      after_expansion =
        quote do
          var = 1
          <<x()::integer-unit(8)-size(var)>>
        end

      assert expand(before_expansion) |> clean_meta([:alignment]) ==
               clean_bit_modifiers(after_expansion)
    end

    defmacro offset(size, binary) do
      quote do
        offset = unquote(size)
        <<_::size(^offset)>> = unquote(binary)
      end
    end

    test "supports size from counters" do
      assert offset(8, <<0>>)
    end

    test "merges bitstrings" do
      import Kernel, except: [-: 1, -: 2]

      assert expand(quote(do: <<x, <<y::signed-native>>, z>>)) |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer, y()::integer-native-signed, z()::integer>>)
               |> clean_bit_modifiers()

      assert expand(quote(do: <<x, <<y::signed-native>>::bitstring, z>>))
             |> clean_meta([:alignment]) ==
               quote(do: <<x()::integer, y()::integer-native-signed, z()::integer>>)
               |> clean_bit_modifiers()
    end

    test "merges binaries" do
      import Kernel, except: [-: 1, -: 2]

      assert expand(quote(do: "foo" <> x)) |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary, x()::binary>>) |> clean_bit_modifiers()

      assert expand(quote(do: "foo" <> <<x::size(4), y::size(4)>>)) |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary, x()::integer-size(4), y()::integer-size(4)>>)
               |> clean_bit_modifiers()

      assert expand(quote(do: <<"foo", <<x::size(4), y::size(4)>>::binary>>))
             |> clean_meta([:alignment]) ==
               quote(do: <<"foo"::binary, x()::integer-size(4), y()::integer-size(4)>>)
               |> clean_bit_modifiers()
    end

    test "guard expressions on size" do
      import Kernel, except: [-: 1, -: 2, +: 1, +: 2, length: 1]

      # Arithmetic operations with literals and variables are valid expressions
      # for bitstring size in OTP 23+

      before_expansion =
        quote do
          var = 1
          <<x::size(var + 3)>>
        end

      after_expansion =
        quote do
          var = 1
          <<x()::integer-size(:erlang.+(var, 3))>>
        end

      assert expand(before_expansion) |> clean_meta([:alignment]) ==
               clean_bit_modifiers(after_expansion)

      # Other valid guard expressions are also legal for bitstring size in OTP 23+

      before_expansion = quote(do: <<x::size(length(~c"test"))>>)
      after_expansion = quote(do: <<x()::integer-size(:erlang.length([?t, ?e, ?s, ?t]))>>)

      assert expand(before_expansion) |> clean_meta([:alignment]) ==
               clean_bit_modifiers(after_expansion)
    end

    test "map lookup on size" do
      import Kernel, except: [-: 1, -: 2]

      before_expansion =
        quote do
          var = %{foo: 3}
          <<x::size(var.foo)>>
        end

      after_expansion =
        quote do
          var = %{foo: 3}
          <<x()::integer-size(var.foo)>>
        end

      assert expand(before_expansion) |> clean_meta([:alignment]) ==
               clean_bit_modifiers(after_expansion)
    end

    test "raises on unaligned binaries in match" do
      message = ~r"its number of bits is not divisible by 8"

      assert_compile_error(message, fn ->
        expand(quote(do: <<rest::size(3)>> <> _ = "foo"))
      end)

      assert_compile_error(message, fn ->
        expand(quote(do: <<1::4>> <> "foo"))
      end)
    end

    test "raises on size or unit for literal bitstrings" do
      message = ~r"literal <<>> in bitstring supports only type specifiers"

      assert_compile_error(message, fn ->
        expand(quote(do: <<(<<"foo">>)::32>>))
      end)
    end

    test "raises on size or unit for literal strings" do
      message = ~r"literal string in bitstring supports only endianness and type specifiers"

      assert_compile_error(message, fn ->
        expand(quote(do: <<"foo"::32>>))
      end)
    end

    test "16-bit floats" do
      import Kernel, except: [-: 1, -: 2]

      assert expand(quote(do: <<12.3::float-16>>)) |> clean_meta([:alignment]) ==
               quote(do: <<12.3::float-size(16)>>) |> clean_bit_modifiers()
    end

    test "raises for invalid size * unit for floats" do
      message = ~r"float requires size\*unit to be 16, 32, or 64 \(default\), got: 128"

      assert_compile_error(message, fn ->
        expand(quote(do: <<12.3::32*4>>))
      end)

      message = ~r"float requires size\*unit to be 16, 32, or 64 \(default\), got: 256"

      assert_compile_error(message, fn ->
        expand(quote(do: <<12.3::256>>))
      end)
    end

    test "raises for invalid size" do
      assert_compile_error(~r/undefined variable "foo"/, fn ->
        code =
          quote do
            fn <<_::size(foo)>> -> :ok end
          end

        expand(code, [])
      end)

      assert_compile_error(~r/undefined variable "foo"/, fn ->
        code =
          quote do
            fn <<_::size(foo), foo::size(8)>> -> :ok end
          end

        expand(code, [])
      end)

      assert_compile_error(~r/undefined variable "foo"/, fn ->
        code =
          quote do
            fn foo, <<_::size(foo)>> -> :ok end
          end

        expand(code, [])
      end)

      assert_compile_error(~r/undefined variable "foo"/, fn ->
        code =
          quote do
            fn foo, <<_::size(foo + 1)>> -> :ok end
          end

        expand(code, [])
      end)

      assert_compile_error(
        ~r"cannot find or invoke local foo/0 inside a bitstring size specifier",
        fn ->
          code =
            quote do
              fn <<_::size(foo())>> -> :ok end
            end

          expand(code, [])
        end
      )

      message = ~r"anonymous call is not allowed inside a bitstring size specifier"

      assert_compile_error(message, fn ->
        code =
          quote do
            fn <<_::size(foo.())>> -> :ok end
          end

        expand(code, [])
      end)

      message = ~r"cannot invoke remote function inside a bitstring size specifier"

      assert_compile_error(message, fn ->
        code =
          quote do
            foo = %{bar: true}
            fn <<_::size(foo.bar())>> -> :ok end
          end

        expand(code, [])
      end)

      message = ~r"cannot invoke remote function Foo.bar/0 inside a bitstring size specifier"

      assert_compile_error(message, fn ->
        code =
          quote do
            fn <<_::size(Foo.bar())>> -> :ok end
          end

        expand(code, [])
      end)
    end

    test "raises for invalid unit" do
      message = ~r"unit in bitstring expects an integer as argument, got: :oops"

      assert_compile_error(message, fn ->
        expand(quote(do: <<"foo"::size(8)-unit(:oops)>>))
      end)
    end

    test "raises for unknown specifier" do
      assert_compile_error(~r"unknown bitstring specifier: unknown()", fn ->
        expand(quote(do: <<1::unknown()>>))
      end)
    end

    test "raises for conflicting specifiers" do
      assert_compile_error(~r"conflicting endianness specification for bit field", fn ->
        expand(quote(do: <<1::little-big>>))
      end)

      assert_compile_error(~r"conflicting unit specification for bit field", fn ->
        expand(quote(do: <<x::bitstring-unit(2)>>))
      end)
    end

    test "raises on binary fields with size in matches" do
      assert expand(quote(do: <<x::binary-size(3), y::binary>> = "foobar"))

      message = ~r"a binary field without size is only allowed at the end of a binary pattern"

      assert_compile_error(message, fn ->
        expand(quote(do: <<x::binary, y::binary>> = "foobar"))
      end)

      assert_compile_error(message, fn ->
        expand(quote(do: <<(<<x::binary>>), y::binary>> = "foobar"))
      end)

      assert_compile_error(message, fn ->
        expand(quote(do: <<(<<x::bitstring>>), y::bitstring>> = "foobar"))
      end)

      assert_compile_error(message, fn ->
        expand(quote(do: <<(<<x::bitstring>>)::bitstring, y::bitstring>> = "foobar"))
      end)
    end
  end

  describe "op ambiguity" do
    test "raises when a call is ambiguous" do
      # We use string_to_quoted! here to avoid the formatter adding parentheses
      message = ~r["a -1" looks like a function call but there is a variable named "a"]

      assert_compile_error(message, fn ->
        code =
          Code.string_to_quoted!("""
          a = 1
          a -1
          """)

        expand(code)
      end)

      message =
        ~r["a -1\.\.\(a \+ 1\)" looks like a function call but there is a variable named "a"]

      assert_compile_error(message, fn ->
        code =
          Code.string_to_quoted!("""
          a = 1
          a -1 .. a + 1
          """)

        expand(code)
      end)
    end
  end

  test "handles invalid expressions" do
    assert_compile_error(~r"invalid quoted expression: {1, 2, 3}", fn ->
      expand_env(quote(do: unquote({1, 2, 3})), __ENV__)
    end)

    assert_compile_error(~r"invalid quoted expression: #Function\<", fn ->
      expand(quote(do: unquote({:sample, fn -> nil end})))
    end)

    assert_compile_error(~r"invalid pattern in match", fn ->
      code =
        quote do
          x = & &1

          case true do
            x.(false) -> true
          end
        end

      expand(code)
    end)

    assert_compile_error(~r"anonymous call is not allowed in guards", fn ->
      code =
        quote do
          x = & &1

          case true do
            true when x.(true) -> true
          end
        end

      expand(code)
    end)

    assert_compile_error(~r"invalid call foo\(1\)\(2\)", fn ->
      expand(quote(do: foo(1)(2)))
    end)

    assert_compile_error(~r"invalid call 1\.foo", fn ->
      expand(quote(do: 1.foo))
    end)

    assert_compile_error(~r"invalid call 0\.foo", fn ->
      expand(quote(do: __ENV__.line.foo))
    end)

    assert_compile_error(~r"misplaced operator ->", fn ->
      expand(quote(do: (foo -> bar)))
    end)
  end

  ## Helpers

  defmacro thirteen do
    13
  end

  defp assert_compile_error(message, fun) do
    assert capture_io(:stderr, fn ->
             assert_raise CompileError, fun
           end) =~ message
  end

  defp clean_meta(expr, vars) do
    cleaner = &Keyword.drop(&1, vars)
    Macro.prewalk(expr, &Macro.update_meta(&1, cleaner))
  end

  @bitstring_modifiers [
    :integer,
    :float,
    :binary,
    :utf8,
    :utf16,
    :utf32,
    :native,
    :signed,
    :bitstring,
    :little
  ]

  defp clean_bit_modifiers(expr) do
    Macro.prewalk(expr, fn
      {expr, meta, atom} when expr in @bitstring_modifiers and is_atom(atom) ->
        {expr, meta, nil}

      other ->
        other
    end)
  end

  defp expand(expr, extra_meta \\ [if_undefined: :apply]) do
    add_meta = &Keyword.merge(&1, extra_meta)

    expr
    |> Macro.postwalk(&Macro.update_meta(&1, add_meta))
    |> expand_env(__ENV__)
    |> elem(0)
  end

  defp expand_env(expr, env, to_clean \\ [:version, :inferred_bitstring_spec, :if_undefined]) do
    {{expr, scope, env}, _capture} =
      with_io(:stderr, fn ->
        :elixir_expand.expand(expr, :elixir_env.env_to_ex(env), env)
      end)

    env = :elixir_env.to_caller({env.line, scope, env})
    {clean_meta(expr, to_clean), env}
  end
end
