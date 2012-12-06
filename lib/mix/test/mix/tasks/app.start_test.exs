Code.require_file "../../../test_helper.exs", __FILE__

defmodule Mix.Tasks.App.StartTest do
  use MixTest.Case

  defmodule CustomApp do
    def project do
      [app: :app_start_sample, version: "0.1.0"]
    end
  end

  test "compile and starts the project" do
    Mix.Project.push CustomApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.App.Start.run ["--no-compile"]
      assert_received { :mix_shell, :error,["Could not start application app_start_sample: " <> _]}

      Mix.Tasks.App.Start.run ["--no-start"]
      assert File.regular?("ebin/Elixir-A.beam")
      assert File.regular?("ebin/app_start_sample.app")
      refute List.keyfind(:application.loaded_applications, :app_start_sample, 0)

      Mix.Tasks.App.Start.run []
      assert List.keyfind(:application.loaded_applications, :app_start_sample, 0)
    end
  after
    purge [A, B, C]
    Mix.Project.pop
  end
end