# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.GleamTest do
  use MixTest.Case
  @moduletag :gleam

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

      expected = [
        {:gleam_stdlib, ">= 0.44.0 and < 2.0.0"},
        {:gleam_otp, ">= 0.16.1 and < 1.0.0"},
        {:gleeunit, ">= 1.0.0 and < 2.0.0", only: :dev}
      ]

      assert Enum.sort(config[:deps]) == Enum.sort(expected)
    end
  end

  describe "gleam export package-information format" do
    test "parse_config" do
      config =
        %{
          "name" => "gael",
          "version" => "1.0.0",
          "gleam" => ">= 1.8.0",
          "dependencies" => %{
            "git_dep" => %{"git" => "../git_dep", "ref" => "957b83b"},
            "gleam_stdlib" => %{"version" => ">= 0.18.0 and < 2.0.0"},
            "my_other_project" => %{"path" => "../my_other_project"}
          },
          "dev-dependencies" => %{"gleeunit" => %{"version" => ">= 1.0.0 and < 2.0.0"}}
        }
        |> Mix.Gleam.parse_config()

      assert config == %{
               name: "gael",
               version: "1.0.0",
               gleam: ">= 1.8.0",
               deps: [
                 {:git_dep, git: "../git_dep", ref: "957b83b"},
                 {:gleam_stdlib, ">= 0.18.0 and < 2.0.0"},
                 {:my_other_project, path: "../my_other_project"},
                 {:gleeunit, ">= 1.0.0 and < 2.0.0", only: :dev}
               ]
             }
    end
  end

  describe "integration with Mix" do
    test "gets and compiles dependencies" do
      in_tmp("get and compile dependencies", fn ->
        Mix.Project.push(GleamAsDep)

        Mix.Tasks.Deps.Get.run([])
        assert_received {:mix_shell, :info, ["* Getting gleam_stdlib " <> _]}
        assert_received {:mix_shell, :info, ["* Getting gleam_otp " <> _]}
        assert_received {:mix_shell, :info, ["* Getting gleeunit " <> _]}

        Mix.Tasks.Deps.Compile.run([])
        assert :gleam_dep.main()
        assert :gleam@int.to_string(1) == "1"

        load_paths =
          Mix.Dep.Converger.converge([])
          |> Enum.map(&Mix.Dep.load_paths(&1))
          |> Enum.concat()

        assert Enum.any?(load_paths, &String.ends_with?(&1, "gleam_dep/ebin"))
        assert Enum.any?(load_paths, &String.ends_with?(&1, "gleam_stdlib/ebin"))
        # Dep of a dep
        assert Enum.any?(load_paths, &String.ends_with?(&1, "gleam_erlang/ebin"))
      end)
    end
  end
end
