Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.LocalTest do
  use MixTest.Case

  test "MIX_PATH" do
    File.rm_rf!(tmp_path("mixpath"))
    System.put_env("MIX_PATH", tmp_path("mixpath/ebin"))

    File.mkdir_p!(tmp_path("mixpath/ebin"))
    Mix.Local.append_paths()

    # Install on MIX_PATH manually
    File.copy!(
      fixture_path("archive/ebin/Elixir.Mix.Tasks.Local.Sample.beam"),
      tmp_path("mixpath/ebin/Elixir.Mix.Tasks.Local.Sample.beam")
    )

    # Run it
    Mix.Task.run("local.sample")
    assert_received {:mix_shell, :info, ["sample"]}
  after
    purge([Mix.Tasks.Local.Sample])
  end
end
