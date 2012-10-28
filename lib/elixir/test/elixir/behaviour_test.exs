Code.require_file "../test_helper.exs", __FILE__

defmodule BehaviourTest do
  use ExUnit.Case, async: true

  defmodule Sample do
    use Behaviour

    @doc "Foo"
    defcallback foo(atom, binary), do: binary

    @doc "Bar"
    defcallback bar(External.hello, my_var :: binary), do: binary
  end

  test :docs_are_defined do
    docs = Enum.qsort(Sample.__info__(:docs))
    assert docs == [
      {{:bar,2},13,:defcallback,[{:arg1,13,:guess},{:my_var,13,nil}],"Bar"},
      {{:foo,2},10,:defcallback,[{:atom,10,nil},{:binary,10,nil}],"Foo"}
    ]
  end

  test :callbacks do
    assert Sample.behaviour_info(:callbacks) == [foo: 2, bar: 2]
  end

  test :default_is_not_supported do
    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        use Behaviour
        defcallback hello(num // 0 :: integer), do: integer
      end
    end

    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        use Behaviour
        defcallback hello(num :: integer // 0), do: integer
      end
    end
  end
end

