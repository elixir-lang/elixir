# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Local.HexTest do
  use MixTest.Case

  @moduletag :tmp_dir

  setup %{tmp_dir: tmp_dir} do
    url = System.get_env("HEX_BUILDS_URL")
    purge([Hex])

    on_exit(fn ->
      purge([Hex])
      if url, do: System.put_env("HEX_BUILDS_URL", url), else: System.delete_env("HEX_BUILDS_URL")
    end)

    defmodule Elixir.Hex do
      def version, do: "0.2.0"
    end

    System.put_env("HEX_BUILDS_URL", tmp_dir)
    File.mkdir_p!(Path.join(tmp_dir, "installs"))
    File.write!(Path.join(tmp_dir, "installs/hex.csv"), "")
    :ok
  end

  test "--if-missing attempts to upgrade to the requested version" do
    assert_raise Mix.Error, "Could not find a version of Hex matching: 0.3.0", fn ->
      Mix.Tasks.Local.Hex.run(["0.3.0", "--if-missing", "--force"])
    end
  end

  test "--if-missing attempts to downgrade to the requested version" do
    assert_raise Mix.Error, "Could not find a version of Hex matching: 0.1.0", fn ->
      Mix.Tasks.Local.Hex.run(["0.1.0", "--if-missing", "--force"])
    end
  end

  test "--if-missing skips the installed version" do
    refute Mix.Tasks.Local.Hex.run(["0.2.0", "--if-missing", "--force"])
  end
end
