# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.LocalTest do
  use MixTest.Case

  @csv """
  1.2.5,ABC,0.9.0,26
  1.2.3,DEF,1.0.0,26
  1.2.4,GHI,1.0.0,26
  """

  @tag :tmp_dir
  test "select correct versions from csv", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.write!("csv", @csv)

      assert {"1.0.0", "1.2.4", "GHI", "26"} =
               Mix.Local.find_matching_versions!("name", nil, "csv")
    end)
  end

  @tag :tmp_dir
  test "select specific version from csv", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.write!("csv", @csv)

      assert {"0.9.0", "1.2.5", "ABC", "26"} =
               Mix.Local.find_matching_versions!("name", "1.2.5", "csv")

      assert {"1.0.0", "1.2.3", "DEF", "26"} =
               Mix.Local.find_matching_versions!("name", "1.2.3", "csv")

      assert_raise Mix.Error, "Could not find a version of name matching: 1.3.0", fn ->
        Mix.Local.find_matching_versions!("name", "1.3.0", "csv")
      end
    end)
  end
end
