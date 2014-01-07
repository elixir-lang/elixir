Code.require_file "test_helper.exs", __DIR__

defmodule BehaviourTest do
  use ExUnit.Case, async: true

  defmodule Sample do
    use Behaviour

    @doc "I should be first."
    defcallback first(integer) :: integer

    @doc "Foo"
    defcallback foo(atom(), binary) :: binary

    @doc "Bar"
    defcallback bar(External.hello, my_var :: binary) :: binary

    defcallback guarded(my_var) :: my_var when my_var: binary

    defcallback orr(atom | integer) :: atom

    defcallback literal(123, { atom }, :atom, [integer], true) :: atom

    @doc "I should be last."
    defmacrocallback last(integer) :: Macro.t
  end

  test :docs do
    docs = Sample.__behaviour__(:docs)
    assert [
      {{:first, 1}, 10, :def, _, "I should be first."},
      {{:foo, 2}, 13, :def, _, "Foo"},
      {{:bar, 2}, 16, :def, _, "Bar"},
      {{:guarded, 1}, 18, :def, _, nil},
      {{:orr, 1}, 20, :def, _, nil},
      {{:literal, 5}, 22, :def, _, nil},
      {{:last, 1}, 25, :defmacro, _, "I should be last."}
    ] = docs

    assert [{ :integer, _, nil }] = List.keyfind(docs, { :first, 1 }, 0) |> elem(3)
    assert [{ :atom, _, nil }, { :binary, _, nil }] = List.keyfind(docs, { :foo, 2 }, 0) |> elem(3)
    assert [{ :hello, _, nil }, { :my_var, _, nil }] = List.keyfind(docs, { :bar, 2 }, 0) |> elem(3)
    assert [{ :my_var, _, nil }] = List.keyfind(docs, { :guarded, 1 }, 0) |> elem(3)
    assert [{ :atom, _, nil }] = List.keyfind(docs, { :orr, 1 }, 0) |> elem(3)
    assert [{ :int1, _, nil }, { :tuple2, _, nil }, { :atom3, _, nil }, { :list4, _, nil }, { :bool5, _, nil} ] = List.keyfind(docs, { :literal, 5 }, 0) |> elem(3)
    assert [{ :integer, _, nil }] = List.keyfind(docs, { :last, 1 }, 0) |> elem(3)
  end

  test :callbacks do
    assert Sample.__behaviour__(:callbacks) == [first: 1, foo: 2, bar: 2, guarded: 1, orr: 1, literal: 5, "MACRO-last": 2]
  end

  test :specs do
    assert length(Keyword.get_values(Sample.module_info[:attributes], :callback)) == 7
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
