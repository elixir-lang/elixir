Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Release.InitTest do
  use MixTest.Case

  test "copies templates as is" do
    in_tmp("release_init", fn ->
      Mix.Task.run("release.init", [])
      assert_received {:mix_shell, :info, ["* creating rel/vm.args.eex"]}
      assert_received {:mix_shell, :info, ["* creating rel/start.eex"]}
      assert_received {:mix_shell, :info, ["* creating rel/start.bat.eex"]}

      assert File.exists?("rel/vm.args.eex")
      assert File.read!("rel/start.eex") =~ "<%= @release.name %>"
      assert File.read!("rel/start.bat.eex") =~ "<%= @release.name %>"
    end)
  end

  test "can be set to --force and --quiet" do
    in_tmp("release_init", fn ->
      Mix.Task.run("release.init", ["--force", "--quiet"])
      refute_received {:mix_shell, :info, ["* creating rel/vm.args.eex"]}
      refute_received {:mix_shell, :info, ["* creating rel/start.eex"]}
      refute_received {:mix_shell, :info, ["* creating rel/start.bat.eex"]}
    end)
  end

  test "raises on bad input" do
    assert_raise OptionParser.ParseError,
                 ~r"--unknown : Unknown option",
                 fn -> Mix.Tasks.Release.Init.run(["--unknown"]) end

    assert_raise Mix.Error,
                 ~r/Expected "mix release.init" without arguments, got: \["foo", "bar"\]/,
                 fn -> Mix.Tasks.Release.Init.run(["foo", "bar"]) end
  end
end
