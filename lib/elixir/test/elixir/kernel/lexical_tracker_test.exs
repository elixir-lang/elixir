Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.LexicalTrackerTest do
  use ExUnit.Case, async: true

  alias Kernel.LexicalTracker, as: D

  setup do
    {:ok, pid} = D.start_link()
    {:ok, [pid: pid]}
  end

  test "can add remote dispatch", config do
    D.remote_dispatch(config[:pid], String, :runtime)
    assert D.references(config[:pid]) == {[], [], [String], []}

    D.remote_dispatch(config[:pid], String, :compile)
    assert D.references(config[:pid]) == {[String], [], [], []}

    D.remote_dispatch(config[:pid], String, :runtime)
    assert D.references(config[:pid]) == {[String], [], [], []}
  end

  test "can add requires", config do
    D.add_export(config[:pid], URI)
    assert D.references(config[:pid]) == {[], [URI], [], []}

    D.remote_dispatch(config[:pid], URI, :runtime)
    assert D.references(config[:pid]) == {[], [URI], [URI], []}

    D.remote_dispatch(config[:pid], URI, :compile)
    assert D.references(config[:pid]) == {[URI], [URI], [], []}
  end

  test "can add module imports", config do
    D.add_export(config[:pid], String)
    D.add_import(config[:pid], String, [], 1, true)

    D.import_dispatch(config[:pid], String, {:upcase, 1}, :runtime)
    assert D.references(config[:pid]) == {[], [String], [String], []}

    D.import_dispatch(config[:pid], String, {:upcase, 1}, :compile)
    assert D.references(config[:pid]) == {[String], [String], [], []}
  end

  test "can add module with {function, arity} imports", config do
    D.add_export(config[:pid], String)
    D.add_import(config[:pid], String, [upcase: 1], 1, true)

    D.import_dispatch(config[:pid], String, {:upcase, 1}, :compile)
    assert D.references(config[:pid]) == {[String], [String], [], []}
  end

  test "can add aliases", config do
    D.add_alias(config[:pid], String, 1, true)
    D.alias_dispatch(config[:pid], String)
    assert D.references(config[:pid]) == {[], [], [], []}
  end

  test "unused module imports", config do
    D.add_import(config[:pid], String, [], 1, true)
    assert D.collect_unused_imports(config[:pid]) == [{String, %{String => 1}}]
  end

  test "used module imports are not unused", config do
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1}, :compile)
    assert D.collect_unused_imports(config[:pid]) == [{String, %{}}]
  end

  test "unused {module, function, arity} imports", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    assert D.collect_unused_imports(config[:pid]) == [{String, %{String => 1, {:upcase, 1} => 1}}]
  end

  test "used {module, function, arity} imports are not unused", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.add_import(config[:pid], String, [downcase: 1], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1}, :compile)
    assert D.collect_unused_imports(config[:pid]) == [{String, %{{:downcase, 1} => 1}}]
  end

  test "overwriting {module, function, arity} import with module import", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], String, {:downcase, 1}, :compile)
    assert D.collect_unused_imports(config[:pid]) == [{String, %{}}]
  end

  test "imports with no warn are not unused", config do
    D.add_import(config[:pid], String, [], 1, false)
    assert D.collect_unused_imports(config[:pid]) == []
  end

  test "unused aliases", config do
    D.add_alias(config[:pid], String, 1, true)
    assert D.collect_unused_aliases(config[:pid]) == [{String, 1}]
  end

  test "used aliases are not unused", config do
    D.add_alias(config[:pid], String, 1, true)
    D.alias_dispatch(config[:pid], String)
    assert D.collect_unused_aliases(config[:pid]) == []
  end

  test "aliases with no warn are not unused", config do
    D.add_alias(config[:pid], String, 1, false)
    assert D.collect_unused_aliases(config[:pid]) == []
  end

  describe "references" do
    test "typespecs do not tag aliases nor types" do
      Code.eval_string("""
      defmodule Kernel.LexicalTrackerTest.AliasTypespecs do
        alias Foo.Bar, as: Bar, warn: false
        @type bar :: Foo.Bar | Foo.Bar.t
        @opaque bar2 :: Foo.Bar.t
        @typep bar3 :: Foo.Bar.t
        @callback foo :: Foo.Bar.t
        @macrocallback foo2(Foo.Bar.t) :: Foo.Bar.t
        @spec foo(bar3) :: Foo.Bar.t
        def foo(_), do: :ok

        # References from specs are processed only late
        @after_compile __MODULE__
        def __after_compile__(env, _) do
          send(self(), {:references, Kernel.LexicalTracker.references(env.lexical_tracker)})
        end
      end
      """)

      assert_received {:references, {compile, _exports, runtime, _}}

      refute Elixir.Bar in runtime
      refute Elixir.Bar in compile

      refute Foo.Bar in runtime
      refute Foo.Bar in compile
    end

    test "typespecs track structs as exports" do
      Code.eval_string("""
      defmodule Kernel.LexicalTrackerTest.StructTypespecs do
        @type uri :: %URI{}

        # References from specs are processed only late
        @after_compile __MODULE__
        def __after_compile__(env, _) do
          send(self(), {:references, Kernel.LexicalTracker.references(env.lexical_tracker)})
        end
      end
      """)

      assert_received {:references, {compile, exports, runtime, _}}

      assert URI in runtime
      assert URI in exports
      refute URI in compile
    end

    test "attributes adds dependency based on expansion" do
      {{compile, _, _, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Attribute1 do
          @example [String, Enum, 3 + 10]
          def foo(atom) when atom in @example, do: atom
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute String in compile
      refute Enum in compile

      {{compile, _, _, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Attribute2 do
          @example [String, Enum]
          def foo(atom) when atom in @example, do: atom
          _ = @example
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert String in compile
      assert Enum in compile

      {{compile, _, _, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Attribute3 do
          @example [String, Enum]
          _ = Module.get_attribute(__MODULE__, :example)
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert String in compile
      assert Enum in compile

      {{compile, _, _, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Attribute4 do
          Module.register_attribute(__MODULE__, :example, accumulate: true)
          @example String
          def foo(atom) when atom in @example, do: atom
          @example Enum
          def bar(atom) when atom in @example, do: atom
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute String in compile
      refute Enum in compile

      {{compile, _, _, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Attribute5 do
          Module.register_attribute(__MODULE__, :example, accumulate: true)
          @example String
          def foo(atom) when atom in @example, do: atom
          @example Enum
          def bar(atom) when atom in @example, do: atom
          _ = Module.get_attribute(__MODULE__, :example)
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert String in compile
      assert Enum in compile

      {{compile, _, _, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Attribute6 do
          @example %{foo: Application.compile_env(:elixir, Enum, String)}
          def foo(atom) when atom == @example.foo, do: atom
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute String in compile
      refute Enum in compile

      {{compile, _, _, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Attribute7 do
          Module.register_attribute(__MODULE__, :example, accumulate: true)
          @example String
          @example Enum
          _ = Module.get_last_attribute(__MODULE__, :example)

          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert String in compile
      assert Enum in compile
    end

    test "@compile adds a runtime dependency" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Compile do
          @compile {:no_warn_undefined, String}
          @compile {:no_warn_undefined, {Enum, :concat, 1}}
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute String in compile
      refute String in exports
      assert String in runtime

      refute Enum in compile
      refute Enum in exports
      assert Enum in runtime
    end

    def __before_compile__(_), do: :ok
    def __after_compile__(_, _), do: :ok
    def __on_definition__(_, _, _, _, _, _), do: :ok

    test "module callbacks add a compile dependency" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.BeforeCompile do
          @before_compile Kernel.LexicalTrackerTest
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert Kernel.LexicalTrackerTest in compile
      refute Kernel.LexicalTrackerTest in exports
      refute Kernel.LexicalTrackerTest in runtime

      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.AfterCompile do
          @after_compile Kernel.LexicalTrackerTest
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert Kernel.LexicalTrackerTest in compile
      refute Kernel.LexicalTrackerTest in exports
      refute Kernel.LexicalTrackerTest in runtime

      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.OnDefinition do
          @on_definition Kernel.LexicalTrackerTest
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert Kernel.LexicalTrackerTest in compile
      refute Kernel.LexicalTrackerTest in exports
      refute Kernel.LexicalTrackerTest in runtime
    end

    test "defdelegate with literal adds runtime dependency" do
      {{compile, _exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Defdelegate do
          defdelegate decode_query(query), to: URI

          opts = [to: Enum]
          defdelegate concat(enum), opts

          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute URI in compile
      assert Enum in compile
      assert URI in runtime
    end

    test "dbg adds a compile dependency" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Dbg do
          def foo, do: dbg(:ok)
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert Macro in compile
      refute Macro in exports
      refute Macro in runtime

      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.NoDbg do
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute Macro in compile
      refute Macro in exports
      refute Macro in runtime
    end

    test "imports adds an export dependency" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Imports do
          import String, warn: false
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute String in compile
      assert String in exports
      refute String in runtime
    end

    test "structs are exports or compile time" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.StructRuntime do
          def expand, do: %URI{}
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute URI in compile
      assert URI in exports
      assert URI in runtime

      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.StructCompile do
          _ = %URI{}
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert URI in compile
      assert URI in exports
      refute URI in runtime

      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.StructPattern do
          def is_uri(%URI{}), do: true
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute URI in compile
      assert URI in exports
      assert URI in runtime
    end

    test "Macro.struct_info! adds an export dependency" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.MacroStruct do
          # We do not use the alias because it would be a compile time
          # dependency. The alias may happen in practice, which is the
          # mechanism to make this expansion become a compile-time one.
          # However, in some cases, such as typespecs, we don't necessarily
          # want the compile-time dependency to happen.
          Macro.struct_info!(:"Elixir.URI", __ENV__)
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute URI in compile
      assert URI in exports
      refute URI in runtime
    end

    test "aliases in patterns and guards inside functions do not add runtime dependency" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.PatternGuardsRuntime do
          def is_uri_atom(URI), do: true
          def is_range_struct(range) when is_struct(range, Range), do: true
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute URI in compile
      refute URI in exports
      refute URI in runtime
      refute Range in compile
      refute Range in exports
      refute Range in runtime
    end

    test "aliases in patterns and guards outside functions do add compile dependency" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.PatternGuardsCompile do
          %URI{} = URI.parse("/")
          case 1..3 do
            range when is_struct(range, Range) -> :ok
          end
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert URI in compile
      assert URI in exports
      refute URI in runtime
      assert Range in compile
      refute Range in exports
      refute Range in runtime
    end

    test "compile_env! does not add a compile dependency" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.CompileEnvStruct do
          require Application
          Application.compile_env(:elixir, URI)
          Application.compile_env(:elixir, [:foo, URI, :bar])
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute URI in compile
      refute URI in exports
      assert URI in runtime
    end

    test "defmodule does not add a compile dependency" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Defmodule do
          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      refute Kernel.LexicalTrackerTest.Defmodule in compile
      refute Kernel.LexicalTrackerTest.Defmodule in exports
      refute Kernel.LexicalTrackerTest.Defmodule in runtime
    end

    test "defmacro adds a compile-time dependency for local calls" do
      {{compile, exports, runtime, _}, _binding} =
        Code.eval_string("""
        defmodule Kernel.LexicalTrackerTest.Defmacro do
          defmacro uri(path) do
            Macro.escape(URI.parse(path))
          end

          def fun() do
            uri("/hello")
          end

          Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
        end |> elem(3)
        """)

      assert URI in compile
      refute URI in exports
      refute URI in runtime
    end
  end
end
