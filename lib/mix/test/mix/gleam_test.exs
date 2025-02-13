# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.GleamTest do
  use MixTest.Case

  @compile {:no_warn_undefined, [:gleam_dep, :gleam@int]}

  defmodule GleamAsDep do
    def project do
      [
        app: :gleam_as_dep,
        version: "0.1.0",
        deps: [
          {:gleam_dep, path: MixTest.Case.tmp_path("gleam_dep"), app: false}
        ]
      ]
    end
  end

  describe "load_config/1" do
    test "loads gleam.toml" do
      path = MixTest.Case.fixture_path("gleam_dep")
      config = Mix.Gleam.load_config(path)

      assert config[:deps] == [
               {:gleam_stdlib, ">= 0.44.0 and < 2.0.0"},
               {:gleam_otp, ">= 0.16.1 and < 1.0.0"},
               {:gleeunit, ">= 1.0.0 and < 2.0.0", only: :dev}
             ]
    end
  end
end
