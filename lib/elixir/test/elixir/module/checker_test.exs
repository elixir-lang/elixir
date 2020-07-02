Code.require_file("../test_helper.exs", __DIR__)

defmodule Module.CheckerTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  setup_all do
    previous = Application.get_env(:elixir, :ansi_enabled, false)
    Application.put_env(:elixir, :ansi_enabled, false)
    on_exit(fn -> Application.put_env(:elixir, :ansi_enabled, previous) end)
  end

  describe "ExCk chunk" do
    test "writes exports" do
      files = %{
        "a.ex" => """
        defmodule A do
          defp a, do: :ok
          defmacrop b, do: a()
          def c, do: b()
          defmacro d, do: b()
          @deprecated "oops"
          def e, do: :ok
        end
        """,
        "b.ex" => """
        defmodule B do
          @callback f() :: :ok
        end
        """,
        "c.ex" => """
        defmodule C do
          @macrocallback g() :: :ok
        end
        """
      }

      modules = compile(files)

      assert read_chunk(modules[A]).exports == [
               {{:c, 0}, %{deprecated_reason: nil, kind: :def}},
               {{:d, 0}, %{deprecated_reason: nil, kind: :defmacro}},
               {{:e, 0}, %{deprecated_reason: "oops", kind: :def}}
             ]

      assert read_chunk(modules[B]).exports == [
               {{:behaviour_info, 1}, %{deprecated_reason: nil, kind: :def}}
             ]

      assert read_chunk(modules[C]).exports == [
               {{:behaviour_info, 1}, %{deprecated_reason: nil, kind: :def}}
             ]
    end
  end

  describe "undefined" do
    test "handles Erlang modules" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: :not_a_module.no_module()
          def b, do: :lists.no_func()
        end
        """
      }

      warning = """
      warning: :not_a_module.no_module/0 is undefined (module :not_a_module is not available or is yet to be defined)
        a.ex:2: A.a/0

      warning: :lists.no_func/0 is undefined or private
        a.ex:3: A.b/0

      """

      assert_warnings(files, warning)
    end

    test "handles built in functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: Kernel.module_info()
          def b, do: Kernel.module_info(:functions)
          def c, do: Kernel.__info__(:functions)
          def d, do: GenServer.behaviour_info(:callbacks)
          def e, do: Kernel.behaviour_info(:callbacks)
        end
        """
      }

      warning = """
      warning: Kernel.behaviour_info/1 is undefined or private
        a.ex:6: A.e/0

      """

      assert_warnings(files, warning)
    end

    test "handles module body conditionals" do
      files = %{
        "a.ex" => """
        defmodule A do
          if function_exported?(List, :flatten, 1) do
            List.flatten([1, 2, 3])
          else
            List.old_flatten([1, 2, 3])
          end

          if function_exported?(List, :flatten, 1) do
            def flatten(arg), do: List.flatten(arg)
          else
            def flatten(arg), do: List.old_flatten(arg)
          end

          if function_exported?(List, :flatten, 1) do
            def flatten2(arg), do: List.old_flatten(arg)
          else
            def flatten2(arg), do: List.flatten(arg)
          end
        end
        """
      }

      warning = """
      warning: List.old_flatten/1 is undefined or private. Did you mean one of:

            * flatten/1
            * flatten/2

        a.ex:15: A.flatten2/1

      """

      assert_warnings(files, warning)
    end

    test "aliases" do
      files = %{
        "a.ex" => """
        defmodule A do
          alias Enum, as: E

          def a(a, b), do: E.map2(a, b)
          def b, do: &E.map2/2

          @file "external_source.ex"
          def c do
            alias Enum, as: EE
            &EE.map2/2
          end
        end
        """
      }

      warning = """
      warning: Enum.map2/2 is undefined or private. Did you mean one of:

            * map/2

      Found at 3 locations:
        a.ex:4: A.a/2
        a.ex:5: A.b/0
        external_source.ex:10: A.c/0

      """

      assert_warnings(files, warning)
    end

    test "reports missing functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: A.no_func()
          def b, do: A.a()

          @file "external_source.ex"
          def c, do: &A.no_func/1
        end
        """
      }

      warning = """
      warning: A.no_func/0 is undefined or private
        a.ex:2: A.a/0

      warning: A.no_func/1 is undefined or private
        external_source.ex:6: A.c/0

      """

      assert_warnings(files, warning)
    end

    test "reports missing functions respecting arity" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: :ok
          def b, do: A.a(1)

          @file "external_source.ex"
          def c, do: A.b(1)
        end
        """
      }

      warning = """
      warning: A.a/1 is undefined or private. Did you mean one of:

            * a/0

        a.ex:3: A.b/0

      warning: A.b/1 is undefined or private. Did you mean one of:

            * b/0

        external_source.ex:6: A.c/0

      """

      assert_warnings(files, warning)
    end

    test "reports missing modules" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: D.no_module()

          @file "external_source.ex"
          def c, do: E.no_module()
        end
        """
      }

      warning = """
      warning: D.no_module/0 is undefined (module D is not available or is yet to be defined)
        a.ex:2: A.a/0

      warning: E.no_module/0 is undefined (module E is not available or is yet to be defined)
        external_source.ex:5: A.c/0

      """

      assert_warnings(files, warning)
    end

    test "reports missing captures" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: &A.no_func/0

          @file "external_source.ex"
          def c, do: &A.no_func/1
        end
        """
      }

      warning = """
      warning: A.no_func/0 is undefined or private
        a.ex:2: A.a/0

      warning: A.no_func/1 is undefined or private
        external_source.ex:5: A.c/0

      """

      assert_warnings(files, warning)
    end

    test "doesn't report missing funcs at compile time" do
      files = %{
        "a.ex" => """
          Enum.map([], fn _ -> BadReferencer.no_func4() end)

          if function_exported?(List, :flatten, 1) do
            List.flatten([1, 2, 3])
          else
            List.old_flatten([1, 2, 3])
          end
        """
      }

      assert_no_warnings(files)
    end

    test "handles multiple modules in one file" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: B.no_func()
          def b, do: B.a()
        end
        """,
        "b.ex" => """
        defmodule B do
          def a, do: A.no_func()
          def b, do: A.b()
        end
        """
      }

      warnings = [
        """
        warning: B.no_func/0 is undefined or private
          a.ex:2: A.a/0
        """,
        """
        warning: A.no_func/0 is undefined or private
          b.ex:2: B.a/0
        """
      ]

      assert_warnings(files, warnings)
    end

    test "groups multiple warnings in one file" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: A.no_func()

          @file "external_source.ex"
          def b, do: A2.no_func()

          def c, do: A.no_func()
          def d, do: A2.no_func()
        end
        """
      }

      warning = """
      warning: A2.no_func/0 is undefined (module A2 is not available or is yet to be defined)
      Found at 2 locations:
        a.ex:8: A.d/0
        external_source.ex:5: A.b/0

      warning: A.no_func/0 is undefined or private
      Found at 2 locations:
        a.ex:2: A.a/0
        a.ex:7: A.c/0

      """

      assert_warnings(files, warning)
    end

    test "protocols are checked, ignoring missing built-in impls" do
      files = %{
        "a.ex" => """
        defprotocol AProtocol do
          def func(arg)
        end

        defmodule AImplementation do
          defimpl AProtocol do
            def func(_), do: B.no_func()
          end
        end
        """
      }

      warning = """
      warning: B.no_func/0 is undefined (module B is not available or is yet to be defined)
        a.ex:7: AProtocol.AImplementation.func/1

      """

      assert_warnings(files, warning)
    end

    test "handles Erlang ops" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(a, b), do: a and b
          def b(a, b), do: a or b
        end
        """
      }

      assert_no_warnings(files)
    end

    test "hints exclude deprecated functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          def to_charlist(a), do: a

          @deprecated "Use String.to_charlist/1 instead"
          def to_char_list(a), do: a

          def c(a), do: A.to_list(a)
        end
        """
      }

      warning = """
      warning: A.to_list/1 is undefined or private. Did you mean one of:

            * to_charlist/1

        a.ex:7: A.c/1

      """

      assert_warnings(files, warning)
    end

    test "imports" do
      files = %{
        "a.ex" => """
        defmodule A do
          import Record

          def a(a, b), do: extract(a, b)
          def b(arg), do: is_record(arg)
        end
        """
      }

      assert_no_warnings(files)
    end

    test "requires" do
      files = %{
        "a.ex" => """
        defmodule A do
          require Integer

          def a(a), do: Integer.is_even(a)
        end
        """
      }

      assert_no_warnings(files)
    end

    test "do not warn for module defined in local context" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a() do
            defmodule B do
              def b(), do: :ok
            end

            B.b()
          end
        end
        """
      }

      assert_no_warnings(files)
    end

    test "warn for unrequired module" do
      files = %{
        "ab.ex" => """
        defmodule A do
          def a(), do: B.b()
        end

        defmodule B do
          defmacro b(), do: :ok
        end
        """
      }

      warning = """
      warning: you must require B before invoking the macro B.b/0
        ab.ex:2: A.a/0

      """

      assert_warnings(files, warning)
    end

    test "excludes local no_warn_undefined" do
      files = %{
        "a.ex" => """
        defmodule A do
          @compile {:no_warn_undefined, [MissingModule, {MissingModule2, :func, 2}]}
          @compile {:no_warn_undefined, {B, :func, 2}}

          def a, do: MissingModule.func(1)
          def b, do: MissingModule2.func(1, 2)
          def c, do: MissingModule2.func(1)
          def d, do: MissingModule3.func(1, 2)
          def e, do: B.func(1)
          def f, do: B.func(1, 2)
          def g, do: B.func(1, 2, 3)
        end
        """,
        "b.ex" => """
        defmodule B do
          def func(_), do: :ok
        end
        """
      }

      warning = """
      warning: MissingModule2.func/1 is undefined (module MissingModule2 is not available or is yet to be defined)
        a.ex:7: A.c/0

      warning: MissingModule3.func/2 is undefined (module MissingModule3 is not available or is yet to be defined)
        a.ex:8: A.d/0

      warning: B.func/3 is undefined or private. Did you mean one of:

            * func/1

        a.ex:11: A.g/0

      """

      assert_warnings(files, warning)
    end

    test "excludes global no_warn_undefined" do
      no_warn_undefined = Code.get_compiler_option(:no_warn_undefined)

      try do
        Code.compiler_options(
          no_warn_undefined: [MissingModule, {MissingModule2, :func, 2}, {B, :func, 2}]
        )

        files = %{
          "a.ex" => """
          defmodule A do
            @compile {:no_warn_undefined, [MissingModule, {MissingModule2, :func, 2}]}
            @compile {:no_warn_undefined, {B, :func, 2}}

            def a, do: MissingModule.func(1)
            def b, do: MissingModule2.func(1, 2)
            def c, do: MissingModule2.func(1)
            def d, do: MissingModule3.func(1, 2)
            def e, do: B.func(1)
            def f, do: B.func(1, 2)
            def g, do: B.func(1, 2, 3)
          end
          """,
          "b.ex" => """
          defmodule B do
            def func(_), do: :ok
          end
          """
        }

        warning = """
        warning: MissingModule2.func/1 is undefined (module MissingModule2 is not available or is yet to be defined)
          a.ex:7: A.c/0

        warning: MissingModule3.func/2 is undefined (module MissingModule3 is not available or is yet to be defined)
          a.ex:8: A.d/0

        warning: B.func/3 is undefined or private. Did you mean one of:

              * func/1

          a.ex:11: A.g/0

        """

        assert_warnings(files, warning)
      after
        Code.compiler_options(no_warn_undefined: no_warn_undefined)
      end
    end

    test "global no_warn_undefined :all" do
      no_warn_undefined = Code.get_compiler_option(:no_warn_undefined)

      try do
        Code.compiler_options(no_warn_undefined: :all)

        files = %{
          "a.ex" => """
          defmodule A do
            def a, do: MissingModule.func(1)
          end
          """
        }

        assert_no_warnings(files)
      after
        Code.compiler_options(no_warn_undefined: no_warn_undefined)
      end
    end

    test "global no_warn_undefined :all and local exclude" do
      no_warn_undefined = Code.get_compiler_option(:no_warn_undefined)

      try do
        Code.compiler_options(no_warn_undefined: :all)

        files = %{
          "a.ex" => """
          defmodule A do
            @compile {:no_warn_undefined, MissingModule}

            def a, do: MissingModule.func(1)
            def b, do: MissingModule2.func(1, 2)
          end
          """
        }

        assert_no_warnings(files)
      after
        Code.compiler_options(no_warn_undefined: no_warn_undefined)
      end
    end
  end

  describe "deprecated" do
    test "reports functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          @deprecated "oops"
          def a, do: A.a()
        end
        """
      }

      warning = """
      warning: A.a/0 is deprecated. oops
        a.ex:3: A.a/0

      """

      assert_warnings(files, warning)
    end

    test "reports imported functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          @deprecated "oops"
          def a, do: :ok
        end
        """,
        "b.ex" => """
        defmodule B do
          import A
          def b, do: a()
        end
        """
      }

      warning = """
      warning: A.a/0 is deprecated. oops
        b.ex:3: B.b/0

      """

      assert_warnings(files, warning)
    end

    test "reports structs" do
      files = %{
        "a.ex" => """
        defmodule A do
          @deprecated "oops"
          defstruct [:x, :y]
          def match(%A{}), do: :ok
          def build(:ok), do: %A{}
        end
        """,
        "b.ex" => """
        defmodule B do
          def match(%A{}), do: :ok
          def build(:ok), do: %A{}
        end
        """
      }

      warnings = [
        """
        warning: A.__struct__/0 is deprecated. oops
        Found at 2 locations:
          a.ex:4: A.match/1
          a.ex:5: A.build/1
        """,
        """
        warning: A.__struct__/0 is deprecated. oops
        Found at 2 locations:
          b.ex:2: B.match/1
          b.ex:3: B.build/1

        """
      ]

      assert_warnings(files, warnings)
    end

    test "reports module body" do
      files = %{
        "a.ex" => """
        defmodule A do
          @deprecated "oops"
          def a, do: :ok
        end
        """,
        "b.ex" => """
        defmodule B do
          require A
          A.a()
        end
        """
      }

      warning = """
      warning: A.a/0 is deprecated. oops
        b.ex:3: B

      """

      assert_warnings(files, warning)
    end
  end

  describe "function header inference" do
    test "warns on literals" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(var = 123, var = "abc"), do: var
        end
        """
      }

      warning = """
      warning: incompatible types:

          integer() !~ binary()

      in expression:

          # a.ex:2
          var = "abc"

      where "var" was given the type binary() in:

          # a.ex:2
          var = "abc"

      where "var" was given the type integer() in:

          # a.ex:2
          var = 123

      Conflict found at
        a.ex:2: A.a/2

      """

      assert_warnings(files, warning)
    end

    test "warns on binary patterns" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(<<var::integer, var::binary>>), do: var
        end
        """
      }

      warning = """
      warning: incompatible types:

          integer() !~ binary()

      in expression:

          # a.ex:2
          <<var::integer(), var::binary()>>

      where "var" was given the type binary() in:

          # a.ex:2
          <<var::integer(), var::binary()>>

      where "var" was given the type integer() in:

          # a.ex:2
          <<var::integer(), var::binary()>>

      Conflict found at
        a.ex:2: A.a/1

      """

      assert_warnings(files, warning)
    end

    test "warns on recursive patterns" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a({var} = var), do: var
        end
        """
      }

      warning = """
      warning: incompatible types:

          {var0} !~ var0

      in expression:

          # a.ex:2
          {var} = var

      where "var" was given the type {var0} in:

          # a.ex:2
          {var} = var

      Conflict found at
        a.ex:2: A.a/1

      """

      assert_warnings(files, warning)
    end

    test "warns on guards" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(var) when is_integer(var) and is_binary(var), do: var
        end
        """
      }

      warning = """
      warning: incompatible types:

          integer() !~ binary()

      in expression:

          # a.ex:2
          is_integer(var) and is_binary(var)

      where "var" was given the type binary() in:

          # a.ex:2
          is_binary(var)

      where "var" was given the type integer() in:

          # a.ex:2
          is_integer(var)

      Conflict found at
        a.ex:2: A.a/1

      """

      assert_warnings(files, warning)
    end

    test "warns on guards with multiple variables" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(x = y) when is_integer(x) and is_binary(y), do: {x, y}
        end
        """
      }

      warning = """
      warning: incompatible types:

          integer() !~ binary()

      in expression:

          # a.ex:2
          is_integer(x) and is_binary(y)

      where "x" was given the type integer() in:

          # a.ex:2
          is_integer(x)

      where "y" was given the type binary() in:

          # a.ex:2
          is_binary(y)

      where "y" was given the same type as "x" in:

          # a.ex:2
          x = y

      Conflict found at
        a.ex:2: A.a/1

      """

      assert_warnings(files, warning)
    end

    test "only show relevant traces in warning" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(x = y, z) when is_integer(x) and is_binary(y) and is_boolean(z), do: {x, y, z}
        end
        """
      }

      warning = """
      warning: incompatible types:

          integer() !~ binary()

      in expression:

          # a.ex:2
          is_integer(x) and is_binary(y) and is_boolean(z)

      where "x" was given the type integer() in:

          # a.ex:2
          is_integer(x)

      where "y" was given the type binary() in:

          # a.ex:2
          is_binary(y)

      where "y" was given the same type as "x" in:

          # a.ex:2
          x = y

      Conflict found at
        a.ex:2: A.a/2

      """

      assert_warnings(files, warning)
    end

    test "check body" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(x) when is_integer(x), do: :foo = x
        end
        """
      }

      warning = """
      warning: incompatible types:

          integer() !~ :foo

      in expression:

          # a.ex:2
          :foo = x

      where \"x\" was given the type :foo in:

          # a.ex:2
          :foo = x

      where \"x\" was given the type integer() in:

          # a.ex:2
          is_integer(x)

      Conflict found at
        a.ex:2: A.a/1

      """

      assert_warnings(files, warning)
    end

    test "check binary" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(foo) when is_binary(foo), do: <<foo>>
        end
        """
      }

      warning = """
      warning: incompatible types:

          binary() !~ integer()

      in expression:

          # a.ex:2
          <<foo>>

      where \"foo\" was given the type integer() in:

          # a.ex:2
          <<foo>>

      where \"foo\" was given the type binary() in:

          # a.ex:2
          is_binary(foo)

      Conflict found at
        a.ex:2: A.a/1

      """

      assert_warnings(files, warning)

      files = %{
        "a.ex" => """
        defmodule A do
          def a(foo) when is_binary(foo), do: <<foo::integer>>
        end
        """
      }

      warning = """
      warning: incompatible types:

          binary() !~ integer()

      in expression:

          # a.ex:2
          <<foo::integer()>>

      where \"foo\" was given the type integer() in:

          # a.ex:2
          <<foo::integer()>>

      where \"foo\" was given the type binary() in:

          # a.ex:2
          is_binary(foo)

      Conflict found at
        a.ex:2: A.a/1

      """

      assert_warnings(files, warning)
    end
  end

  describe "map checks" do
    test "show map() when comparing against non-map" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(foo) do
            foo.bar
            :atom = foo
          end
        end
        """
      }

      warning = """
      warning: incompatible types:

          map() !~ :atom

      in expression:

          # a.ex:4
          :atom = foo

      where "foo" was given the type :atom in:

          # a.ex:4
          :atom = foo

      where "foo" was given the type %{bar: var1, optional(dynamic()) => dynamic()} in:

          # a.ex:3
          foo.bar

      Conflict found at
        a.ex:4: A.a/1

      """

      assert_warnings(files, warning)
    end

    test "use module as map (without parentheses)" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(foo) do
            %module{} = foo
            module.__struct__
          end
        end
        """
      }

      warning = """
      warning: parentheses are required when dynamically invoking zero-arity functions in expression:

          # a.ex:4
          module.__struct__

      "module" is an atom and you attempted to fetch the field __struct__. Make sure that \
      "module" is a map or add parentheses to invoke a function instead:

          module.__struct__()

      Conflict found at
        a.ex:4: A.a/1

      """

      assert_warnings(files, warning)
    end

    test "use map as module (with parentheses)" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(foo) when is_map(foo) do
            foo.__struct__()
          end
        end
        """
      }

      warning = """
      warning: parentheses are not allowed when fetching fields from a map in expression:

          # a.ex:3
          foo.__struct__()

      "foo" is a map and you attempted to invoke the function __struct__/0. Make sure that \
      "foo" is an atom or remove parentheses to fetch a field:

          foo.__struct__

      Conflict found at
        a.ex:3: A.a/1

      """

      assert_warnings(files, warning)
    end

    test "non-existant map field warning" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a() do
            map = %{foo: 1}
            map.bar
          end
        end
        """
      }

      warning = """
      warning: undefined field "bar" in expression:

          # a.ex:4
          map.bar

      where "map" was given the type %{foo: integer()} in:

          # a.ex:3
          map = %{foo: 1}

      Conflict found at
        a.ex:4: A.a/0

      """

      assert_warnings(files, warning)
    end

    test "non-existant struct field warning" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a(foo) do
            %File.Stat{} = foo
            foo.bar
          end
        end
        """
      }

      warning = """
      warning: undefined field "bar" in expression:

          # a.ex:4
          foo.bar

      where "foo" was given the type %File.Stat{} in:

          # a.ex:3
          %File.Stat{} = foo

      Conflict found at
        a.ex:4: A.a/1

      """

      assert_warnings(files, warning)
    end
  end

  defp assert_warnings(files, expected) when is_binary(expected) do
    assert capture_compile_warnings(files) == expected
  end

  defp assert_warnings(files, expecteds) when is_list(expecteds) do
    output = capture_compile_warnings(files)

    Enum.each(expecteds, fn expected ->
      assert output =~ expected
    end)
  end

  defp assert_no_warnings(files) do
    assert capture_compile_warnings(files) == ""
  end

  defp capture_compile_warnings(files) do
    in_tmp(fn ->
      paths = generate_files(files)
      capture_io(:stderr, fn -> compile_files(paths) end)
    end)
  end

  defp compile(files) do
    in_tmp(fn ->
      paths = generate_files(files)
      compile_files(paths)
    end)
  end

  defp compile_files(paths) do
    {:ok, modules, _warnings} = Kernel.ParallelCompiler.compile_to_path(paths, ".")

    Map.new(modules, fn module ->
      {^module, binary, _filename} = :code.get_object_code(module)
      :code.purge(module)
      :code.delete(module)
      {module, binary}
    end)
  end

  defp generate_files(files) do
    for {file, contents} <- files do
      File.write!(file, contents)
      file
    end
  end

  defp read_chunk(binary) do
    assert {:ok, {_module, [{'ExCk', chunk}]}} = :beam_lib.chunks(binary, ['ExCk'])
    assert {:elixir_checker_v1, map} = :erlang.binary_to_term(chunk)
    map
  end

  defp in_tmp(fun) do
    path = PathHelpers.tmp_path("checker")

    File.rm_rf!(path)
    File.mkdir_p!(path)
    File.cd!(path, fun)
  end
end
