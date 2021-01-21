defmodule Module.IfAvailableAttributeTestTest do
  use ExUnit.Case, async: true

  defmodule IfAvailable do
    # "Foo42 (conditional, false)"
    @if_available Foo42
    @spec foo(integer(), any()) :: integer()
    def foo(i, _), do: 42 + i

    @doc "Converts anything to integer"
    @if_available Foo42
    # "Foo42 (conditional, false)"
    @spec foo(Foo42.t()) :: integer()
    def foo(%Foo42{integer: i}), do: 42 + i

    @if_available DateTime
    # "DateTime (conditional, true)"
    @spec foo(DateTime.t()) :: integer()
    def foo(%DateTime{} = dt), do: 42 + DateTime.to_unix(dt)

    # "integer (not conditional)"
    @spec foo(integer()) :: integer()
    def foo(i), do: 42 + i

    #########################################################

    @if_available Bar42
    # "Bar42 (macro, conditional, false)"
    defmacro bar(i), do: quote(do: unquote(i))

    @if_available DateTime
    # "DateTime (macro, conditional, true)"
    defmacro bar(i, j), do: quote(do: unquote(i) + unquote(j))
  end

  test "functions" do
    assert [foo: 1] == IfAvailable.__info__(:functions)

    dt = DateTime.utc_now()
    assert DateTime.to_unix(DateTime.add(dt, 42)) == IfAvailable.foo(dt)

    assert 43 == IfAvailable.foo(1)

    assert_raise UndefinedFunctionError,
                 ~r/function Module\.IfAvailableAttributeTestTest\.IfAvailable.foo\/2 is undefined or private/,
                 fn ->
                   IfAvailable.foo(42, nil)
                 end
  end

  test "macros" do
    require IfAvailable

    assert [bar: 2] == IfAvailable.__info__(:macros)
    assert 43 == IfAvailable.bar(1, 42)
  end
end
