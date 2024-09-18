Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.TracersTest do
  use ExUnit.Case

  defp compile_string(string) do
    string
    |> Code.string_to_quoted!(columns: true)
    |> Code.compile_quoted()
  end

  def trace(event, %Macro.Env{} = env) do
    send(self(), {event, env})
    :ok
  end

  setup_all do
    Code.put_compiler_option(:tracers, [__MODULE__])

    on_exit(fn ->
      Code.put_compiler_option(:tracers, [])
    end)
  end

  test "traces start and stop" do
    compile_string("""
    Foo
    """)

    assert_received {:start, %{lexical_tracker: pid}} when is_pid(pid)
    assert_received {:stop, %{lexical_tracker: pid}} when is_pid(pid)
  end

  test "traces alias references" do
    compile_string("""
    Foo
    """)

    assert_received {{:alias_reference, meta, Foo}, _}
    assert meta[:line] == 1
    assert meta[:column] == 1
  end

  test "traces aliases" do
    compile_string("""
    alias Hello.World
    World

    alias Foo, as: Bar, warn: true
    Bar
    """)

    assert_received {{:alias, meta, Hello.World, World, []}, _}
    assert meta[:line] == 1
    assert meta[:column] == 1
    assert_received {{:alias_expansion, meta, World, Hello.World}, _}
    assert meta[:line] == 2
    assert meta[:column] == 1

    assert_received {{:alias, meta, Foo, Bar, [as: Bar, warn: true]}, _}
    assert meta[:line] == 4
    assert meta[:column] == 1
    assert_received {{:alias_expansion, meta, Bar, Foo}, _}
    assert meta[:line] == 5
    assert meta[:column] == 1
  end

  test "traces imports" do
    compile_string("""
    import Integer, only: [is_odd: 1, parse: 1]
    true = is_odd(1)
    {1, ""} = parse("1")
    """)

    assert_received {{:import, meta, Integer, only: [is_odd: 1, parse: 1]}, _}
    assert meta[:line] == 1
    assert meta[:column] == 1

    assert_received {{:imported_macro, meta, Integer, :is_odd, 1}, _}
    assert meta[:line] == 2
    assert meta[:column] == 8

    assert_received {{:imported_function, meta, Integer, :parse, 1}, _}
    assert meta[:line] == 3
    assert meta[:column] == 11

    refute_received {{:remote_function, _, Integer, :parse, 1}, _}
  end

  test "traces imports via capture" do
    compile_string("""
    import Integer, only: [is_odd: 1, parse: 1]
    &is_odd/1
    &parse/1
    """)

    assert_received {{:import, meta, Integer, only: [is_odd: 1, parse: 1]}, _}
    assert meta[:line] == 1
    assert meta[:column] == 1

    assert_received {{:imported_macro, meta, Integer, :is_odd, 1}, _}
    assert meta[:line] == 2
    assert meta[:column] == 2

    assert_received {{:imported_function, meta, Integer, :parse, 1}, _}
    assert meta[:line] == 3
    assert meta[:column] == 2

    refute_received {{:remote_function, _meta, Integer, :parse, 1}, _}
  end

  test "traces structs" do
    compile_string("""
    %URI{path: "/"}
    """)

    assert_received {{:struct_expansion, meta, URI, [:path]}, _}
    assert meta[:line] == 1
    assert meta[:column] == 1
  end

  test "traces remote" do
    compile_string("""
    require Integer
    true = Integer.is_odd(1)
    {1, ""} = Integer.parse("1")
    "foo" = Atom.to_string(:foo)
    """)

    assert_received {{:remote_macro, meta, Integer, :is_odd, 1}, _}
    assert meta[:line] == 2
    assert meta[:column] == 16

    assert_received {{:remote_function, meta, Integer, :parse, 1}, _}
    assert meta[:line] == 3
    assert meta[:column] == 19

    assert_received {{:remote_function, meta, Atom, :to_string, 1}, _}
    assert meta[:line] == 4
    assert meta[:column] == 14
  end

  test "traces remote via captures" do
    compile_string("""
    require Integer
    &Integer.is_odd/1
    &Integer.parse/1
    """)

    assert_received {{:remote_macro, meta, Integer, :is_odd, 1}, _}
    assert meta[:line] == 2
    assert meta[:column] == 10

    assert_received {{:remote_function, meta, Integer, :parse, 1}, _}
    assert meta[:line] == 3
    assert meta[:column] == 10
  end

  test "traces locals" do
    compile_string("""
    defmodule Sample do
      defmacro foo(arg), do: arg
      def bar(arg), do: arg
      def baz(arg), do: foo(arg) + bar(arg)
    end
    """)

    assert_received {{:local_macro, meta, :foo, 1}, _}
    assert meta[:line] == 4
    assert meta[:column] == 21

    assert_received {{:local_function, meta, :bar, 1}, _}
    assert meta[:line] == 4
    assert meta[:column] == 32
  after
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "traces locals with capture" do
    compile_string("""
    defmodule Sample do
      defmacro foo(arg), do: arg
      def bar(arg), do: arg
      def baz(_), do: {&foo/1, &bar/1}
    end
    """)

    assert_received {{:local_macro, meta, :foo, 1}, _}
    assert meta[:line] == 4
    assert meta[:column] == 21

    assert_received {{:local_function, meta, :bar, 1}, _}
    assert meta[:line] == 4
    assert meta[:column] == 29
  after
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "traces modules" do
    compile_string("""
    defmodule Sample do
      :ok
    end
    """)

    assert_received {:defmodule, %{module: Sample, function: nil}}
    assert_received {{:on_module, <<_::binary>>, :none}, %{module: Sample, function: nil}}
  after
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "traces dynamic modules" do
    compile_string("""
    Module.create(Sample, :ok, __ENV__)
    """)

    assert_received {:defmodule, %{module: Sample, function: nil}}
    assert_received {{:on_module, <<_::binary>>, :none}, %{module: Sample, function: nil}}
  after
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "traces module attribute expansion" do
    compile_string("""
    defmodule TracersModuleAttribute do
      @module URI
      @module
    end
    """)

    assert_received {{:alias_reference, [line: 3], URI}, %{file: "@module"}}
  end

  test "traces string interpolation" do
    compile_string("""
    arg = 1 + 2
    "foo\#{arg}"
    """)

    assert_received {{:remote_macro, meta, Kernel, :to_string, 1}, _env}
    assert meta[:from_interpolation]
  end

  test "traces bracket access" do
    compile_string("""
    foo = %{bar: 3}
    foo[:bar]
    """)

    assert_received {{:remote_function, meta, Access, :get, 2}, _env}
    assert meta[:from_brackets]

    compile_string("""
    defmodule Foo do
      @foo %{bar: 3}
      def a() do
        @foo[:bar]
      end
    end
    """)

    assert_received {{:remote_function, meta, Access, :get, 2}, _env}
    assert meta[:from_brackets]

    compile_string("""
    %{bar: 3}[:bar]
    """)

    assert_received {{:remote_function, meta, Access, :get, 2}, _env}
    assert meta[:from_brackets]
  end

  test "does not trace bind quoted twice" do
    compile_string("""
    quote bind_quoted: [foo: List.flatten([])] do
      foo
    end
    """)

    assert_received {{:remote_function, _, List, :flatten, 1}, _}
    refute_received {{:remote_function, _, List, :flatten, 1}, _}
  end

  test "does not trace captures twice" do
    compile_string("""
    &List.flatten/1
    """)

    assert_received {{:remote_function, _, List, :flatten, 1}, _}
    refute_received {{:remote_function, _, List, :flatten, 1}, _}
  end

  """
  # Make sure this module is compiled with column information
  defmodule MacroWithColumn do
    defmacro some_macro(list) do
      quote do
        Enum.map(unquote(list), fn str -> String.upcase(str) end)
      end
    end
  end
  """
  |> Code.string_to_quoted!(columns: true)
  |> Code.compile_quoted()

  test "traces quoted from macro expansion without column information" do
    compile_string("""
    require MacroWithColumn
    MacroWithColumn.some_macro(["hello", "world", "!"])
    """)

    assert_received {{:alias_reference, meta, Enum}, _env}
    refute meta[:column]
  end
end
