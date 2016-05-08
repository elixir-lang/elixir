Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.AppTest do
  use MixTest.Case

  defmodule CustomProject do
    def project do
      [app: :custom_project, version: "0.2.0"]
    end

    def application do
      [maxT: :infinity,
       applications: [:example_app]]
    end
  end

  defmodule InvalidProject do
    def project do
      [app: :invalid_project, version: "0.3.0"]
    end

    def application do
      case Process.get(:error) do
        :modules -> [modules: :invalid]
        :maxT -> [maxT: :invalid]
        :registered -> [registered: ["invalid"]]
        :included_applications -> [included_applications: ["invalid"]]
        :applications -> [applications: ["invalid"]]
        :env -> [env: [:a]]
        :mod -> [mod: {Mod}]
        :start_phases -> [start_phases: [:invalid]]
      end
    end
  end

  defmodule InvalidVsnProject do
    def project do
      [app: :invalid_vsn_project, version: "0.3"]
    end
  end

  defmodule NonASCIIDescriptionProject do
    def project do
      [app: :non_ascii_description_project,
       version: "0.3.0",
       description: "São Paulo"]
    end
  end

  test "generates .app file when changes happen" do
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      assert Mix.Tasks.Compile.App.run([]) == :ok

      contents = File.read!("_build/dev/lib/sample/ebin/sample.app")
      assert contents =~ "{application,sample"
      assert contents =~ "0.1.0"
      assert contents =~ "'Elixir.A'"
      assert contents =~ "{applications,[kernel,stdlib,elixir]}"

      assert Mix.Tasks.Compile.App.run([]) == :noop
    end
  end

  test "use custom application settings" do
    Mix.Project.push CustomProject

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      Mix.Tasks.Compile.App.run([])
      contents = File.read!("_build/dev/lib/custom_project/ebin/custom_project.app")
      assert contents =~ "0.2.0"
      assert contents =~ "{maxT,infinity}"
      assert contents =~ "{applications,[kernel,stdlib,elixir,example_app]}"
    end
  end

  test "application properties validation" do
    Mix.Project.push InvalidProject

    in_fixture "no_mixfile", fn ->
      for error <- [:modules, :maxT, :registered, :included_applications,
                    :applications, :env, :mod, :start_phases] do
        Process.put(:error, error)

        assert_raise Mix.Error, ~r/:#{error}/, fn ->
          Mix.Tasks.Compile.App.run([])
        end
      end
    end
  end

  test ".app contains description and registered (as required by systools)" do
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      assert Mix.Tasks.Compile.App.run([]) == :ok

      {:ok, [{_app, _, properties}]} = :file.consult("_build/dev/lib/sample/ebin/sample.app")
      assert properties[:registered] == []
      assert properties[:description] == 'sample'
      assert properties[:applications] == [:kernel, :stdlib, :elixir]

      assert Mix.Tasks.Compile.App.run([]) == :noop
    end
  end

  test "raise on invalid version" do
    Mix.Project.push InvalidVsnProject

    in_fixture "no_mixfile", fn ->
      message = "Expected :version to be a SemVer version, got: \"0.3\""
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end
    end
  end

  test "raise on non-ASCII description" do
    Mix.Project.push NonASCIIDescriptionProject

    in_fixture "no_mixfile", fn ->
      message = "Application description (:description) may contain only ASCII codepoints, got: [83, 227, 111, 32, 80, 97, 117, 108, 111]"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end
    end
  end
end
