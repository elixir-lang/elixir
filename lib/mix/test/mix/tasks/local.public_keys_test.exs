Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Local.PublicKeysTest do
  use MixTest.Case

  @pub """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAmlRL//AgfszL2vkk7G3K
  t+vrI2d3aG+fGD4BZ1k1Qd/mS0EoDNfDQCpqtLzdM7oOjtu0BNCBvG3HC0tlAU92
  i3EFqEC7RSHNIGYrQ4g5WwGQMw8+Qp3D32esw7iRZKvtJu8IpS7l8x4KQq1QbfTh
  s+3yFOQyytnY6of7w2Rv37WtdA3wKAIP6yPgvl8QzwxHRORX4zNcadsJOuZt3ncv
  Bq7pyQN8g2ddF86Ycep/+cU4/onnGW3zHn5pGdvuiHqxGOq+s3+UzljLyLlTCwzz
  ncYdESIw29EEIB5xTcT0Q4qRQEuDwcp8/fASTZ7c6lv8AfpRKb69tGWLoXcx/V9P
  dQIDAQAB
  -----END PUBLIC KEY-----
  """

  setup do
    File.rm_rf!(tmp_path(".mix/public_keys"))
    :ok
  end

  test "lists public keys" do
    Mix.Tasks.Local.PublicKeys.run([])
    assert_received {:mix_shell, :info, ["* in-memory public key for Elixir" <> _]}

    Mix.Tasks.Local.PublicKeys.run(["--detailed"])
    assert_received {:mix_shell, :info, ["\n-----BEGIN PUBLIC KEY-----\n" <> _]}
  end

  test "installs public keys" do
    path = tmp_path("sample.pub")
    File.write!(path, @pub)

    send(self(), {:mix_shell_input, :yes?, true})
    Mix.Tasks.Local.PublicKeys.run([path])
    assert_received {:mix_shell, :yes?, ["Are you sure you want to install public key" <> _]}
    assert_received {:mix_shell, :info, ["* creating " <> _]}

    send(self(), {:mix_shell_input, :yes?, true})
    Mix.Tasks.Local.PublicKeys.run([path])
    assert_received {:mix_shell, :yes?, ["There is already a public key named sample.pub." <> _]}
    assert_received {:mix_shell, :info, ["* creating " <> _]}
  end

  test "raises on bad public keys on install" do
    assert_raise Mix.Error, ~r/Could not decode public key:/, fn ->
      path = tmp_path("bad.pub")
      File.write!(path, "oops")
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Local.PublicKeys.run([path])
    end
  end
end
