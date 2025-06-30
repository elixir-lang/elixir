# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.GleamTest do
  use MixTest.Case
  @moduletag :gleam

  @compile {:no_warn_undefined, [:gleam_dep, :gleam@int, :deeper_gleam_dep]}

  defmodule GleamAsDep do
    def project do
      [
        app: :gleam_as_dep,
        version: "0.1.0",
        deps: [
          {:deeper_gleam_dep, path: MixTest.Case.tmp_path("subfolder/deeper_gleam_dep")}
        ]
      ]
    end
  end

  describe "load_config/1" do
    test "loads gleam.toml" do
      path = MixTest.Case.fixture_path("gleam_dep")
      config = Mix.Gleam.load_config(path)

      expected = [
        {:gleam_stdlib, "0.59.0"},
        {:gleam_otp, "0.16.1"},
        {:gleeunit, ">= 1.0.0 and < 2.0.0", only: [:dev, :test]}
      ]

      assert Enum.sort(config[:deps]) == Enum.sort(expected)
    end
  end

  describe "Gleam export package-information format" do
    test "parse_config" do
      config =
        %{
          "name" => "gael",
          "version" => "1.0.0",
          "gleam" => ">= 1.8.0",
          "dependencies" => %{
            "git_dep" => %{"git" => "../git_dep", "ref" => "957b83b"},
            "gleam_stdlib" => %{"version" => ">= 0.18.0 and < 2.0.0"}
          },
          "dev-dependencies" => %{
            "gleeunit" => %{"version" => ">= 1.0.0 and < 2.0.0"}
          },
          "erlang" => %{
            "application_start_module" => "some@application",
            "extra_applications" => ["some_app"]
          }
        }
        |> Mix.Gleam.parse_config()

      assert config == %{
               name: "gael",
               version: "1.0.0",
               gleam: ">= 1.8.0",
               deps: [
                 {:git_dep, git: "../git_dep", ref: "957b83b"},
                 {:gleam_stdlib, ">= 0.18.0 and < 2.0.0"},
                 {:gleeunit, ">= 1.0.0 and < 2.0.0", only: [:dev, :test]}
               ],
               application: [
                 mod: {:some@application, []},
                 extra_applications: [:some_app]
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

        Mix.Tasks.Deps.Compile.run([])
        assert :gleam_dep.main()
        assert :gleam_dep.erl() == ~c'Hello from Collocated Erlang!'
        assert :gleam@int.to_string(1) == "1"
        assert :deeper_gleam_dep.main()

        {:ok, content} = :file.consult("_build/dev/lib/gleam_dep/ebin/gleam_dep.app")

        assert content == [
                 {
                   :application,
                   :gleam_dep,
                   [
                     {:modules, [:collocated_erlang, :gleam_dep]},
                     {:optional_applications, []},
                     {:applications, [:kernel, :stdlib, :elixir, :gleam_otp, :gleam_stdlib]},
                     {:description, ~c"gleam_dep"},
                     {:registered, []},
                     {:vsn, ~c"1.0.0"}
                   ]
                 }
               ]

        assert File.exists?("_build/dev/lib/deeper_gleam_dep/ebin/deeper_gleam_dep.app")
        assert :ok == Mix.Tasks.Deps.Loadpaths.run([])
      end)
    end
  end
end
