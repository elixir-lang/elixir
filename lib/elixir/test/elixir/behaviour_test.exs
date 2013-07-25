Code.require_file "test_helper.exs", __DIR__

defmodule BehaviourTest do
  use ExUnit.Case, async: true

  defmodule Sample do
    use Behaviour

    @doc "I should be first."
    defcallback first(integer) :: integer

    @doc "Foo"
    defcallback foo(atom, binary) :: binary

    @doc "Bar"
    defcallback bar(External.hello, my_var :: binary) :: binary

    @doc "I should be last."
    defmacrocallback last(integer) :: Macro.t
  end

  test :docs do
    docs = Sample.__behaviour__(:docs)
    assert docs == [
      {{:first, 1}, 10, :def, "I should be first."},
      {{:foo, 2}, 13, :def, "Foo"},
      {{:bar, 2}, 16, :def, "Bar"},
      {{:last, 1}, 19, :defmacro, "I should be last."}
    ]
  end

  test :callbacks do
    assert Sample.__behaviour__(:callbacks) == [first: 1, foo: 2, bar: 2, "MACRO-last": 2]
  end

  test :specs do
    assert length(Keyword.get_values(Sample.module_info[:attributes], :callback)) == 4
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
