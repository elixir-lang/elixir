Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.EvalTest do
  use MixTest.Case

  setup do
    Mix.Project.push(MixTest.Case.Sample)
  end

  test "does not start applications", context do
    in_tmp(context.test, fn ->
      expr = "send self(), {:apps, Application.started_applications()}"
      Mix.Tasks.Eval.run([expr])
      assert_received {:apps, apps}
      refute List.keyfind(apps, :sample, 0)
    end)
  end

  test "runs multiple commands", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Eval.run(["send self(), {:eval, :foo}"])
      assert_received {:eval, :foo}

      Mix.Tasks.Eval.run(["send self(), {:eval, :bar}"])
      assert_received {:eval, :bar}
    end)
  end

  test "accepts arguments", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Eval.run(["send self(), {:argv, System.argv()}", "bar", "baz"])
      assert_received {:argv, ~w[bar baz]}
    end)
  end

  test "resets argv to previous value", context do
    argv = System.argv()

    try do
      in_tmp(context.test, fn ->
        System.argv(["foo"])

        Mix.Tasks.Eval.run(["send self(), {:argv, System.argv()}", "bar", "baz"])
        assert_received {:argv, ~w[bar baz]}

        assert ["foo"] == System.argv()
      end)
    after
      System.argv(argv)
    end
  end

  test "runs without mix.exs" do
    Mix.Project.pop()

    assert_raise Mix.Error, ~r/Cannot execute "mix eval" without a Mix.Project/, fn ->
      Mix.Tasks.Eval.run(["send self(), {:eval, :foo}"])
    end

    Mix.Tasks.Eval.run(["--no-mix-exs", "send self(), {:eval, :foo}"])
    assert_received {:eval, :foo}
  end
end
