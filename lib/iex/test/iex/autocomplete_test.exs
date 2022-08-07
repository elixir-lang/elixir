Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.AutocompleteTest do
  use ExUnit.Case, async: true

  setup do
    evaluator = IEx.Server.start_evaluator([])
    Process.put(:evaluator, evaluator)
    :ok
  end

  defp eval(line) do
    ExUnit.CaptureIO.capture_io(fn ->
      evaluator = Process.get(:evaluator)
      Process.group_leader(evaluator, Process.group_leader())
      send(evaluator, {:eval, self(), line <> "\n", %IEx.State{}})
      assert_receive {:evaled, _, _, _}
    end)
  end

  defp expand(expr) do
    IEx.Autocomplete.expand(Enum.reverse(expr), self())
  end

  @tag :tmp_dir
  test "it autocompletes filesystem paths", %{tmp_dir: dir} do
    # TODO
    assert true
  end

  test "it uses the broader code completion" do
    # TODO
    assert true
  end

  describe "with a custom autocompletion mechanism" do
    test "it uses the custom autocompletion first if it's expandable" do
      # TODO
      assert true
    end

    test "if it isn't expandable, it relies  on the remaining autocompletion mechanisms" do
      # TODO
      assert true
    end
  end
end
