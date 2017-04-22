Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.Overridable do
  def sample do
    1
  end

  def with_super do
    1
  end

  def without_super do
    1
  end

  def super_with_multiple_args(x, y) do
    x + y
  end

  def many_clauses(0) do
    11
  end

  def many_clauses(1) do
    13
  end

  defoverridable [sample: 0, with_super: 0, without_super: 0,
                  super_with_multiple_args: 2, many_clauses: 1]

  true = Module.overridable? __MODULE__, {:without_super, 0}
  true = Module.overridable? __MODULE__, {:with_super, 0}

  def without_super do
    :without_super
  end

  def with_super do
    super() + 2
  end

  def super_with_multiple_args(x, y) do
    super x, y * 2
  end

  def many_clauses(2) do
    17
  end

  def many_clauses(3) do
    super(0) + super(1)
  end

  def many_clauses(x) do
    super(x)
  end

  ## Macros

  defmacro overridable_macro(x) do
    quote do
      unquote(x) + 100
    end
  end

  defoverridable overridable_macro: 1

  defmacro overridable_macro(x) do
    quote do
      unquote(super(x)) + 1_000
    end
  end

  defmacrop private_macro(x \\ raise "never called")

  defmacrop private_macro(x) do
    quote do
      unquote(x) + 100
    end
  end

  defoverridable private_macro: 1

  defmacrop private_macro(x) do
    quote do
      unquote(super(x)) + 1_000
    end
  end

  def private_macro_call(val \\ 11) do
    private_macro(val)
  end
end

defmodule Kernel.OverridableExampleBehaviour do
  @callback required_callback :: any
  @callback optional_callback :: any
  @macrocallback required_macro_callback(arg :: any) :: Macro.t
  @macrocallback optional_macro_callback(arg :: any, arg2 :: any) :: Macro.t
  @optional_callbacks optional_callback: 0, optional_macro_callback: 1
end

defmodule Kernel.OverridableWithBehaviour do
  @behaviour Kernel.OverridableExampleBehaviour

  def required_callback(), do: "original"

  def optional_callback(), do: "original"

  def not_a_behaviour_callback(), do: "original"

  defmacro required_macro_callback(boolean) do
    quote do
      if unquote(boolean) do
        "original"
      end
    end
  end

  defoverridable Kernel.OverridableExampleBehaviour

  def required_callback(), do: "overridden"

  defmacro required_macro_callback(boolean) do
    quote do
      if unquote(boolean) do
        "overridden"
      end
    end
  end

  defmacro optional_macro_callback(boolean, boolean2) do
    quote do
      if unquote(boolean) && unquote(boolean2) do
        "defined optional for the first time"
      end
    end
  end

  def optional_callback(), do: "overridden"

  def not_a_behaviour_callback(), do: "overridden"
end

defmodule Kernel.OverridableTest do
  defmodule OverridableOrder do
    def not_private(str) do
      process_url(str)
    end

    def process_url(_str) do
      :first
    end

    # There was a bug where the order in which we removed
    # overridable expressions lead to errors. This module
    # aims to guarantee removing process_url/1 before we
    # remove the function that depends on it does not cause
    # errors. If it compiles, it works!
    defoverridable [process_url: 1, not_private: 1]

    def process_url(_str) do
      :second
    end
  end

  require Kernel.Overridable, as: Overridable
  use ExUnit.Case

  defp purge(module) do
    :code.purge(module)
    :code.delete(module)
  end

  test "overridable is made concrete if no other is defined" do
    assert Overridable.sample == 1
  end

  test "overridable overridden with super" do
    assert Overridable.with_super == 3
  end

  test "overridable overridden without super" do
    assert Overridable.without_super == :without_super
  end

  test "calling super with multiple args" do
    assert Overridable.super_with_multiple_args(1, 2) == 5
  end

  test "overridable with many clauses" do
    assert Overridable.many_clauses(0) == 11
    assert Overridable.many_clauses(1) == 13
    assert Overridable.many_clauses(2) == 17
    assert Overridable.many_clauses(3) == 24
  end

  test "overridable definitions are private" do
    refute {:"with_super (overridable 0)", 0} in Overridable.module_info(:exports)
    refute {:"with_super (overridable 1)", 0} in Overridable.module_info(:exports)
  end

  test "overridable macros" do
    a = 11
    assert Overridable.overridable_macro(a) == 1111
    assert Overridable.private_macro_call() == 1111
  end

  test "invalid super call" do
    message =
      "nofile:4: no super defined for foo/0 in module Kernel.OverridableOrder.Forwarding. " <>
      "Overridable functions available are: bar/0"
    assert_raise CompileError, message, fn ->
      Code.eval_string """
      defmodule Kernel.OverridableOrder.Forwarding do
        def bar(), do: 1
        defoverridable bar: 0
        def foo(), do: super()
      end
      """
    end

    purge Kernel.OverridableOrder.Forwarding
  end

  test "undefined functions can't be marked as overridable" do
    message = "cannot make function foo/2 overridable because it was not defined"
    assert_raise ArgumentError, message, fn ->
      Code.eval_string """
      defmodule Kernel.OverridableOrder.Foo do
        defoverridable foo: 2
      end
      """
    end

    purge Kernel.OverridableOrder.Foo
  end

  test "overrides required callback with behaviour as argument" do
    assert Kernel.OverridableWithBehaviour.required_callback == "overridden"
  end

  test "overrides optional callback with behaviour as argument" do
    assert Kernel.OverridableWithBehaviour.optional_callback == "overridden"
  end

  test "does not override function that is not a callback for this behaviour" do
    assert Kernel.OverridableWithBehaviour.not_a_behaviour_callback == "original"
  end

  test "overrides required macro callback with behaviour as argument" do
    require Kernel.OverridableWithBehaviour

    assert Kernel.OverridableWithBehaviour.required_macro_callback(true) == "overridden"
  end

  test "specifies optional macro callback with behaviour as argument" do
    require Kernel.OverridableWithBehaviour

    assert Kernel.OverridableWithBehaviour.optional_macro_callback(true, true) == "defined optional for the first time"
  end

  test "undefined module can't be passed as argument to defoverridable" do
    message = "cannot pass module Kernel.OverridableTest.Bar as argument to defoverridable/1 because it was not defined"
    assert_raise ArgumentError, message, fn ->
      Code.eval_string """
      defmodule Kernel.OverridableTest.Foo do
        defoverridable Kernel.OverridableTest.Bar
      end
      """
    end
    purge Kernel.OverridableTest.Foo
  end

  test "module without @behaviour can't be passed as argument to defoverridable" do
    message = "cannot pass module Kernel.OverridableExampleBehaviour as argument to defoverridable/1" <>
              " because its corresponding behaviour is missing. Did you forget to add " <>
              "@behaviour Kernel.OverridableExampleBehaviour ?"
    assert_raise ArgumentError, message, fn ->
      Code.eval_string """
      defmodule Kernel.OverridableTest.Foo do
        defoverridable Kernel.OverridableExampleBehaviour
      end
      """
    end
    purge Kernel.OverridableTest.Foo
  end

  test "module with no callbacks can't be passed as argument to defoverridable" do
    message = "cannot pass module Kernel.OverridableTest.Bar as argument to defoverridable/1 because it does not define any callbacks"
    assert_raise ArgumentError, message, fn ->
      Code.eval_string """
      defmodule Kernel.OverridableTest.Bar do
      end
      defmodule Kernel.OverridableTest.Foo do
        @behaviour Kernel.OverridableTest.Bar
        defoverridable Kernel.OverridableTest.Bar
      end
      """
    end
    purge Kernel.OverridableTest.Bar
    purge Kernel.OverridableTest.Foo
  end

  test "atom which is not a module can't be passed as argument to defoverridable" do
    message = "cannot pass module :abc as argument to defoverridable/1 because it was not defined"
    assert_raise ArgumentError, message, fn ->
      Code.eval_string """
      defmodule Kernel.OverridableTest.Foo do
        defoverridable :abc
      end
      """
    end
    purge Kernel.OverridableTest.Foo
  end
end
