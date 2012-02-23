Code.require_file "../../test_helper", __FILE__

defmodule Kernel::ForwardingLibrary do
  defmacro __using__(_, _) do
    quote do
      defforward [sample: 1],     to: unquote(__MODULE__)
      defforward [with_super: 1], to: unquote(__MODULE__), via: :defp

      defforward [implicit_super: 1], to: unquote(__MODULE__)
      defforward [argless_super: 0],  to: unquote(__MODULE__)
      defforward [nested_super: 1],   to: unquote(__MODULE__)

      def pointer(arg), do: with_super(arg)
    end
  end

  def sample(module, _, arg) do
    { module, arg }
  end

  def with_super(module, _, arg) do
    { module, arg }
  end

  def argless_super(module, _) do
    module
  end

  def implicit_super(module, _, arg) do
    { module, arg }
  end
end

defmodule Kernel::ForwardedExample do
  use Kernel::ForwardingLibrary

  def sample(arg) do
    arg
  end

  def with_super(arg) do
    { super(arg), arg + 1 }
  end

  def argless_super do
    { :ok, super }
  end

  def implicit_super({ x, y }) do
    { :ok, super, x + y }
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

  test "defforward with super invokes the forwarded function" do
    assert_equal { { Kernel::ForwardedExample, 1 }, 2 }, Kernel::ForwardedExample.with_super(1)
  end

  test "defforward with super without arguments also invokes the forwarded function" do
    assert_equal { :ok, Kernel::ForwardedExample }, Kernel::ForwardedExample.argless_super
  end

  test "defforward with anonymous super automtically forwards arguments" do
    assert_equal { :ok, { Kernel::ForwardedExample, { 1, 2 } }, 3 },
      Kernel::ForwardedExample.implicit_super({ 1, 2 })
  end

  test "invalid super call" do
    try do
      Erlang.elixir.eval 'defmodule Foo::Forwarding do\nuse Kernel::ForwardingLibrary\ndef foo, do: super\nend'
      flunk "expected eval to fail"
    rescue: error
      assert_match("nofile:3: no super defined for foo/0 in module '::Foo::Forwarding'." <> _, error.message)
    end
  end
end