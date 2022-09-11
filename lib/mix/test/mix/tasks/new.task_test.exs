Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.New.TaskTest do
  use MixTest.Case

  test "new.task" do
    in_fixture("git_repo", fn ->
      Mix.Tasks.New.Task.run(["gen.controller"])

      assert_received {:mix_shell, :info, ["* creating lib/mix/tasks"]}
      assert_received {:mix_shell, :info, ["* creating lib/mix/tasks/gen.controller.ex"]}

      assert_file("lib/mix/tasks/gen.controller.ex", fn file ->
        assert file == """
               defmodule Mix.Tasks.Gen.Controller do
                 use Mix.Task

                 @shortdoc "A placeholder shortdoc for mix gen.controller"
                 @moduledoc @shortdoc

                 @doc false
                 def run(argv) do
                   # Fill me in!
                 end
               end
               """
      end)
    end)
  end

  defp assert_file(file) do
    assert File.regular?(file), "Expected #{file} to exist, but does not"
  end

  defp assert_file(file, match) do
    cond do
      is_struct(match, Regex) ->
        assert_file(file, &assert(&1 =~ match))

      is_function(match, 1) ->
        assert_file(file)
        match.(File.read!(file))
    end
  end
end
