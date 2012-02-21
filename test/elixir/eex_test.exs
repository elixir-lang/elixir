Code.require_file "../test_helper", __FILE__

defmodule EExTest do
  use ExUnit::Case

  test "compile simple string" do
    assert_eval "foo bar", "foo bar"
  end

  defp assert_eval(expected, atual) do
    compiled = EEx.compile(atual)
    { result, _ } = Code.eval_quoted(compiled, [], __FILE__, __LINE__)
    assert_equal expected, result
  end
end
