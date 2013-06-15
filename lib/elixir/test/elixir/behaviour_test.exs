Code.require_file "test_helper.exs", __DIR__

defmodule BehaviourTest do
  use ExUnit.Case, async: true

  defmodule Sample do
    use Behaviour

    @doc "Foo"
    defcallback foo(atom, binary) :: binary

    @doc "Bar"
    defcallback bar(External.hello, my_var :: binary) :: binary
  end

  test :docs do
    docs = Enum.sort(Sample.__behaviour__(:docs))
    assert docs == [
      {{:bar, 2}, 13, "Bar"},
      {{:foo, 2}, 10, "Foo"}
    ]
  end

  test :callbacks do
    assert Sample.__behaviour__(:callbacks) == [foo: 2, bar: 2]
  end

  test :specs do
    assert length(Keyword.get_values(Sample.module_info[:attributes], :callback)) == 2
  end

  test :default_is_not_supported do
    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        use Behaviour
        defcallback hello(num // 0 :: integer) :: integer
      end
    end

    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        use Behaviour
        defcallback hello(num :: integer // 0) :: integer
      end
    end
  end
end
