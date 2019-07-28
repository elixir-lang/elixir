Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.TracersTest do
  use ExUnit.Case

  import Code, only: [compile_string: 1]

  def trace(event, %Macro.Env{}) do
    send(self(), event)
    :ok
  end

  setup_all do
    Code.put_compiler_option(:tracers, [__MODULE__])
    Code.put_compiler_option(:parser_options, columns: true)

    on_exit(fn ->
      Code.put_compiler_option(:tracers, [])
      Code.put_compiler_option(:parser_options, [])
    end)
  end

  test "traces remote references" do
    compile_string("""
    Foo
    """)

    assert_receive {:remote_reference, meta, Foo}
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

    assert_receive {:alias, meta, Hello.World, World, []}
    assert meta[:line] == 1
    assert meta[:column] == 1
    assert_receive {:alias_expansion, meta, World, Hello.World}
    assert meta[:line] == 2
    assert meta[:column] == 1

    assert_receive {:alias, meta, Foo, Bar, [as: Bar, warn: true]}
    assert meta[:line] == 4
    assert meta[:column] == 1
    assert_receive {:alias_expansion, meta, Bar, Foo}
    assert meta[:line] == 5
    assert meta[:column] == 1
  end

  test "traces imports" do
    compile_string("""
    import Integer, only: [is_odd: 1, parse: 1]
    true = is_odd(1)
    {1, ""} = parse("1")
    """)

    assert_receive {:import, meta, Integer, only: [is_odd: 1, parse: 1]}
    assert meta[:line] == 1
    assert meta[:column] == 1

    assert_receive {:imported_macro, meta, Integer, :is_odd, 1}
    assert meta[:line] == 2
    assert meta[:column] == 8

    assert_receive {:imported_function, meta, Integer, :parse, 1}
    assert meta[:line] == 3
    assert meta[:column] == 11
  end

  test "traces structs" do
    compile_string("""
    %URI{}
    """)

    assert_receive {:struct_expansion, meta, URI}
    assert meta[:line] == 1
    assert meta[:column] == 1
  end

  test "traces remote" do
    compile_string("""
    require Integer
    true = Integer.is_odd(1)
    {1, ""} = Integer.parse("1")
    """)

    assert_receive {:remote_macro, meta, Integer, :is_odd, 1}
    assert meta[:line] == 2
    assert meta[:column] == 15

    assert_receive {:remote_function, meta, Integer, :parse, 1}
    assert meta[:line] == 3
    assert meta[:column] == 18
  end

  test "traces remote via captures" do
    compile_string("""
    require Integer
    &Integer.is_odd/1
    &Integer.parse/1
    """)

    assert_receive {:remote_macro, meta, Integer, :is_odd, 1}
    assert meta[:line] == 2
    assert meta[:column] == 1

    assert_receive {:remote_function, meta, Integer, :parse, 1}
    assert meta[:line] == 3
    assert meta[:column] == 1
  end

  test "traces locals" do
    compile_string("""
    defmodule Sample do
      defmacro foo(arg), do: arg
      def bar(arg), do: arg
      def baz(arg), do: foo(arg) + bar(arg)
    end
    """)

    assert_receive {:local_macro, meta, :foo, 1}
    assert meta[:line] == 4
    assert meta[:column] == 21

    assert_receive {:local_function, meta, :bar, 1}
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

    assert_receive {:local_macro, meta, :foo, 1}
    assert meta[:line] == 4
    assert meta[:column] == 20

    assert_receive {:local_function, meta, :bar, 1}
    assert meta[:line] == 4
    assert meta[:column] == 28
  after
    :code.purge(Sample)
    :code.delete(Sample)
  end
end
