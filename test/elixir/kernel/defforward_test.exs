Code.require_file "../../test_helper", __FILE__

defmodule Kernel::ForwardingLibrary do
  defmacro __using__(_, _) do
    quote do
      defforward [sample: 1], to: unquote(__MODULE__)
      defforward :private, [other: 1], to: unquote(__MODULE__)
      def pointer(arg), do: other(arg)
    end
  end

  def sample(module, arg) do
    { module, arg }
  end

  def other(module, arg) do
    { module, arg }
  end
end

defmodule Kernel::ForwardedExample do
  use Kernel::ForwardingLibrary

  def sample(arg) do
    arg
  end

  def other(arg) do
    arg
  end
end

defmodule Kernel::ForwardingExample do
  use Kernel::ForwardingLibrary
end

defmodule Kernel::ForwardingTest do
  use ExUnit::Case

  test "defforward is a noop when forwarding function is explicitly defined" do
    assert_equal 1, Kernel::ForwardedExample.sample(1)
  end

  test "defforward defines a function when one is not defined" do
    assert_equal { Kernel::ForwardingExample, 1 }, Kernel::ForwardingExample.sample(1)
  end

  test "defforward also defines private functions" do
    assert_equal { Kernel::ForwardingExample, 1 }, Kernel::ForwardingExample.pointer(1)

    try do
      Kernel::ForwardingExample.other(1)
      flunk "expected function to be private"
    rescue: UndefinedFunctionError
    end
  end
end