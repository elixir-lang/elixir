# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.GleamTest do
  use MixTest.Case
  @moduletag :gleam

  @compile {:no_warn_undefined, [:gleam_dep]}

  defmodule GleamAsDep do
    def project do
      [
        app: :gleam_as_dep,
        version: "0.1.0",
        deps: [
          {:gleam_dep, path: MixTest.Case.tmp_path("gleam_dep")}
        ]
      ]
    end
  end

  defmodule BrokenGleamAsDep do
    def project do
      [
        app: :broken_gleam_as_dep,
        version: "0.1.0",
        deps: [
          {:broken_gleam_dep, path: MixTest.Case.tmp_path("broken_gleam_dep")}
        ]
      ]
    end
  end

  describe "load_config/1" do
    test "loads gleam.toml" do
      path = tmp_path("gleam_load_config")
      File.rm_rf!(path)
      File.mkdir_p!(path)

      File.write!(Path.join(path, "gleam.toml"), """
      name = "gleam_load_config"
      version = "1.0.0"

      [dependencies]
      gleam_stdlib = "0.59.0"
      gleam_otp = "0.16.1"

      [dev-dependencies]
      gleeunit = ">= 1.0.0 and < 2.0.0"
      """)

      config = Mix.Gleam.load_config(path)

      expected = [
        {:gleam_stdlib, "0.59.0"},
        {:gleam_otp, "0.16.1"}
      ]

      assert Enum.sort(config[:deps]) == Enum.sort(expected)
    end
  end

  describe "Gleam export package-information format" do
    test "parse_config" do
      {:ok, config} =
        %{
          "name" => "gael",
          "version" => "1.0.0",
          "gleam" => ">= 1.8.0",
          "dependencies" => %{
            "git_dep" => %{"git" => "../git_dep", "ref" => "957b83b"},
            "gleam_stdlib" => %{"version" => ">= 0.18.0 and < 2.0.0"}
          }
        }
        |> Mix.Gleam.parse_config()

      assert config == %{
               name: "gael",
               version: "1.0.0",
               deps: [
                 {:git_dep, git: "../git_dep", ref: "957b83b"},
                 {:gleam_stdlib, ">= 0.18.0 and < 2.0.0"}
               ]
             }
    end
  end

  describe "integration with Mix" do
    test "gets and compiles dependencies" do
      dep_path = tmp_path("gleam_dep")
      File.rm_rf!(dep_path)
      File.mkdir_p!(Path.join(dep_path, "src"))

      File.write!(Path.join(dep_path, "gleam.toml"), """
      name = "gleam_dep"
      version = "1.0.0"
      description = "GleamDep description"

      [erlang]
      extra_applications = ["ssl"]
      """)

      File.write!(Path.join(dep_path, "src/gleam_dep.gleam"), """
      pub fn main() {
        True
      }

      @external(erlang, "collocated_erlang", "hello")
      pub fn erl() -> String
      """)

      File.write!(Path.join(dep_path, "src/collocated_erlang.erl"), """
      -module(collocated_erlang).
      -export([hello/0]).

      hello() ->
          "Hello from Collocated Erlang!".
      """)

      in_tmp("get and compile dependencies", fn ->
        Mix.Project.push(GleamAsDep)

        Mix.Tasks.Deps.Get.run([])
        Mix.Tasks.Deps.Compile.run([])
        assert :gleam_dep.main()
        assert :gleam_dep.erl() == ~c'Hello from Collocated Erlang!'

        {:ok, [{:application, :gleam_dep, specs}]} =
          :file.consult("_build/dev/lib/gleam_dep/ebin/gleam_dep.app")

        assert Keyword.equal?(specs, [
                 {:modules, [:collocated_erlang, :gleam_dep]},
                 {:applications, [:ssl]},
                 {:description, ~c"GleamDep description"},
                 {:registered, []},
                 {:vsn, ~c"1.0.0"}
               ])

        assert :ok == Mix.Tasks.Deps.Loadpaths.run([])
      end)
    end

    test "raises the Gleam CLI's own message when compile-package fails" do
      dep_path = MixTest.Case.tmp_path("broken_gleam_dep")
      File.rm_rf!(dep_path)
      File.mkdir_p!(dep_path)

      File.write!(Path.join(dep_path, "gleam.toml"), """
      name = "broken_gleam_dep"
      version = "1.0.0"
      """)

      in_tmp("compile gleam dependency failure", fn ->
        Mix.Project.push(BrokenGleamAsDep)

        Mix.Tasks.Deps.Get.run([])

        assert_raise Mix.Error, ~r/File IO failure/, fn ->
          Mix.Tasks.Deps.Compile.run([])
        end
      end)
    end
  end
end
