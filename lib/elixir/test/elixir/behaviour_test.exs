Code.require_file "test_helper.exs", __DIR__

defmodule BehaviourTest do
  use ExUnit.Case, async: true

  {_, _, sample_binary, _} =
    defmodule Sample do
      use Behaviour

      defcallback first(integer) :: integer

      defcallback foo(atom(), binary) :: binary

      defcallback bar(External.hello, my_var :: binary) :: binary

      defcallback guarded(my_var) :: my_var when my_var: binary

      defcallback orr(atom | integer) :: atom

      defcallback literal(123, {atom}, :atom, [integer], true) :: atom

      defmacrocallback last(integer) :: Macro.t
    end

  @sample_binary sample_binary

  test "callbacks" do
    assert Sample.__behaviour__(:callbacks) == [first: 1, guarded: 1, "MACRO-last": 2, literal: 5, orr: 1, foo: 2, bar: 2]
  end

  test "specs" do
    assert length(Kernel.Typespec.beam_callbacks(@sample_binary)) == 7
  end

  test "default is not supported" do
    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        use Behaviour
        defcallback hello(num \\ 0 :: integer) :: integer
      end
    end

    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        use Behaviour
        defcallback hello(num :: integer \\ 0) :: integer
      end
    end
  end
end
