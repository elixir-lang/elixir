Code.require_file "../../test_helper.exs", __FILE__

defmodule Mix.UmbrellaTest do
  use MixTest.Case

  test "compile umbrella" do
    in_fixture "umbrella", fn ->
      output = mix "compile"
      lines = String.split(output, "\n")
      lines = Enum.filter(lines, &1 != "")

      assert lines == [
        "==> bar",
        "Compiled lib/bar.ex",
        "Generated bar.app",
        "==> foo",
        "Compiled lib/foo.ex",
        "Generated foo.app" ]
    end
  end
end
