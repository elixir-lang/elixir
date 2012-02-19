Code.require_file "../../test_helper", __FILE__

defmodule Kernel::ForwardingLibrary do
  defmacro __using__(_, _) do
    quote do
      defforward [sample: 1], to: unquote(__MODULE__)
      defforward [final: 0], to: unquote(__MODULE__)
      defforward [other: 1], to: unquote(__MODULE__), via: :defp
      def pointer(arg), do: other(arg)
    end
  end

  def sample(module, arg) do
    { module, arg }
  end

  def other(module, arg) do
    { module, arg }
  end

  def final(module) do
    { 1, module }
  end
end

defmodule Kernel::XForwardingLibrary do
  defmacro __using__(_, _) do
    quote do
      Module.remove_forwarding __MODULE__, [final: 0]
      defforward [final: 0], to: unquote(__MODULE__)
    end
  end

  def final(module) do
    { 2, module }
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
  use Kernel::XForwardingLibrary
end

defmodule Kernel::ForwardingTest do
  use ExUnit::Case

  test "defforward is a noop when forwarding function is explicitly defined" do
    assert_equal 1, Kernel::ForwardedExample.sample(1)
  end

  test "defforward defines a function when one is not defined" do
    assert_equal { Kernel::ForwardingExample, 1 }, Kernel::ForwardingExample.sample(1)
  end

  test "defforward can be overriden in a later module" do
    assert_equal { 2, Kernel::ForwardingExample }, Kernel::ForwardingExample.final
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