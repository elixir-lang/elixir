Code.require_file("../test_helper.exs", __DIR__)

defmodule Autocomplete.Custom do
  @behaviour IEx.Autocomplete.Behaviour

  # note we're pattern-matching on the reversed 'Bar'
  @impl true
  def expandable_fragment('raB' ++ _rest = fragment), do: fragment

  def expandable_fragment(_), do: []

  @impl true
  def expand('raB' ++ _rest, _shell) do
    hint = ""
    possible_custom_completions = ["id", "name", "address"]

    IEx.Autocomplete.yes(hint, possible_custom_completions)
  end

  def expand(_, _), do: IEx.Autocomplete.no()
end

defmodule IEx.AutocompleteTest do
  use ExUnit.Case, async: true

  setup do
    evaluator = IEx.Server.start_evaluator([])
    Process.put(:evaluator, evaluator)
    :ok
  end

  defp expand(expr) do
    IEx.Autocomplete.expand(Enum.reverse(expr), self())
  end

  test "it autocompletes filesystem paths" do
    assert {:yes, [], files} = expand('"./')
    assert length(files) > 0
  end

  test "it uses the broader code completion" do
    assert {:yes, [], functions} = expand('Enum.')
    assert length(functions) > 0
  end

  describe "with a custom autocompletion mechanism" do
    setup do
      IEx.configure(autocompletion: Autocomplete.Custom)
    end

    test "it uses the custom autocompletion first if it's expandable" do
      assert {:yes, '', custom_completions} = expand('Foo~>Bar')
      assert length(custom_completions) > 0
    end

    test "if it isn't expandable, it relies on the remaining autocompletion mechanisms" do
      assert {:yes, [], functions} = expand('String.')
      assert length(functions) > 0
    end
  end
end
