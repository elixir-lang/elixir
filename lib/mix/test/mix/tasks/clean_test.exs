Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.CleanTest do
  use MixTest.Case

  defmodule Sample do
    def project do
      [
        app: :sample,
        version: "0.1.0",
        deps: [
          { :tidy, "0.1.0", path: "elixir-lang/tidy" }
        ]
      ]
    end
  end

  setup do
    Mix.Project.push Sample
    :ok
  end

  teardown do
    Mix.Project.pop
    :ok
  end

  test "cleans all repos" do
    in_fixture "deps_status", fn ->
      Mix.Tasks.Clean.run ["--all"]
      assert_received { :mix_shell, :info, ["* Cleaning tidy (elixir-lang/tidy)"] }
    end
  end
end
