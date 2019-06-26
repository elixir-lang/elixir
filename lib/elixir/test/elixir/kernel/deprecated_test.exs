Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.DeprecatedTest do
  use ExUnit.Case

  import PathHelpers

  test "raises on invalid @deprecated" do
    assert_raise ArgumentError, ~r"should be a string with the reason", fn ->
      defmodule InvalidDeprecated do
        @deprecated 1.2
        def foo, do: :bar
      end
    end
  end

  test "takes into account deprecated from defaults" do
    defmodule DefaultDeprecated do
      @deprecated "reason"
      def foo(x \\ true), do: x
    end

    assert DefaultDeprecated.__info__(:deprecated) == [
             {{:foo, 0}, "reason"},
             {{:foo, 1}, "reason"}
           ]
  end

  test "add deprecated to __info__ and beam chunks" do
    write_beam(
      defmodule SampleDeprecated do
        @deprecated "Use SampleDeprecated.bar/0 instead"
        def foo, do: true

        def bar, do: false
      end
    )

    deprecated = [
      {{:foo, 0}, "Use SampleDeprecated.bar/0 instead"}
    ]

    assert SampleDeprecated.__info__(:deprecated) == deprecated

    {SampleDeprecated, bin, _beam_path} = :code.get_object_code(SampleDeprecated)
    {:ok, {SampleDeprecated, [{'ExDp', deprecated_bin}]}} = :beam_lib.chunks(bin, ['ExDp'])

    assert :erlang.binary_to_term(deprecated_bin) == {:elixir_deprecated_v1, deprecated}
  end
end
