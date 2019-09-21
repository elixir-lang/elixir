Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.CaseTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  ExUnit.Case.register_attribute(__MODULE__, :foo)
  ExUnit.Case.register_attribute(__MODULE__, :bar, accumulate: true)
  ExUnit.Case.register_attribute(__MODULE__, :baz)
  ExUnit.Case.register_describe_attribute(__MODULE__, :describe_foo)
  ExUnit.Case.register_describe_attribute(__MODULE__, :describe_bar, accumulate: true)
  ExUnit.Case.register_describe_attribute(__MODULE__, :describe_baz)
  ExUnit.Case.register_module_attribute(__MODULE__, :module_foo)
  ExUnit.Case.register_module_attribute(__MODULE__, :module_bar, accumulate: true)
  ExUnit.Case.register_module_attribute(__MODULE__, :module_baz)

  @moduletag :moduletag
  @module_foo :hello
  @module_bar :world

  test "defines __ex_unit__" do
    assert %ExUnit.TestModule{name: __MODULE__, tests: tests} = __ex_unit__()
    assert length(tests) > 0
  end

  @tag hello: false
  @tag :hello
  @tag world: :bad
  @tag world: :good
  test "tags", context do
    line = __ENV__.line - 1
    assert context[:module] == __MODULE__
    assert context[:case] == __MODULE__
    assert context[:test] == __ENV__.function |> elem(0)
    assert context[:line] == line
    assert context[:async] == true
    assert context[:hello] == true
    assert context[:world] == :good
  end

  test "reset tags", context do
    assert is_nil(context[:hello])
    assert is_nil(context[:world])
  end

  test "module tags", context do
    assert context[:moduletag] == true
  end

  @tag moduletag: :overridden
  test "module tags can be overridden", context do
    assert context[:moduletag] == :overridden
  end

  @foo :hello
  @bar :world
  test "registered attributes are in context", context do
    assert context.registered.foo == :hello
    assert context.registered.bar == [:world]
    assert context.registered.baz == nil
  end

  test "registered attributes are set per test", context do
    assert context.registered.foo == nil
    assert context.registered.bar == []
  end

  describe "with attributes" do
    @describe_foo :hello
    @describe_bar :world

    test "registered subscribe attributes are in context", context do
      assert context.registered.describe_foo == :hello
      assert context.registered.describe_bar == [:world]
      assert context.registered.describe_baz == nil
    end
  end

  describe "without attributes" do
    test "registered subscribe attributes are set per subscribe", context do
      assert context.registered.describe_foo == nil
      assert context.registered.describe_bar == []
    end
  end

  test "registered module attributes are in context", context do
    assert context.registered.module_foo == :hello
    assert context.registered.module_bar == [:world]
    assert context.registered.module_baz == nil
  end

  test "registered module attributes stay in context", context do
    assert context.registered.module_foo == :hello
    assert context.registered.module_bar == [:world]
  end

  test "raises when same name is registered twice" do
    assert_raise ArgumentError, "cannot register attribute :foo multiple times", fn ->
      defmodule AcrossLevelDoubleRegisterTest do
        use ExUnit.Case
        ExUnit.Case.register_attribute(__MODULE__, :foo)
        ExUnit.Case.register_module_attribute(__MODULE__, :foo)
      end
    end
  end

  test "raises when attribute is set before being registered" do
    assert_raise RuntimeError, "you must set @foo after it has been registered", fn ->
      defmodule SetBeforeRegisterTest do
        use ExUnit.Case
        @foo true
        ExUnit.Case.register_attribute(__MODULE__, :foo, accumulate: true)
      end
    end
  end

  test "warns for using it twice with different options" do
    assert capture_io(:stderr, fn ->
             defmodule WarnsUsedTwice do
               use ExUnit.Case
               use ExUnit.Case, async: true
             end
           end) == ""

    assert capture_io(:stderr, fn ->
             defmodule WarnsUsedTwice do
               use ExUnit.Case
               use ExUnit.Case, async: false
             end
           end) =~ "ExUnit.Case was already"
  end
end
