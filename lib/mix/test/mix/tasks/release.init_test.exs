Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Release.InitTest do
  use MixTest.Case

  test "copies templates as is" do
    in_tmp("release_init", fn ->
      Mix.Task.run("release.init", [])
      assert_received {:mix_shell, :info, ["* creating rel/vm.args.eex"]}
      assert_received {:mix_shell, :info, ["* creating rel/env.sh.eex"]}
      assert_received {:mix_shell, :info, ["* creating rel/env.bat.eex"]}

      assert File.exists?("rel/vm.args.eex")
      assert File.exists?("rel/env.sh.eex")
      assert File.exists?("rel/env.bat.eex")
    end)
  end

  test "can be set to --ovewrite and --quiet" do
    in_tmp("release_init", fn ->
      Mix.Task.run("release.init", ["--overwrite", "--quiet"])
      Mix.Task.run("release.init", ["--overwrite", "--quiet"])
      refute_received {:mix_shell, :info, ["* creating rel/vm.args.eex"]}
      refute_received {:mix_shell, :info, ["* creating rel/env.sh.eex"]}
      refute_received {:mix_shell, :info, ["* creating rel/env.bat.eex"]}
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
