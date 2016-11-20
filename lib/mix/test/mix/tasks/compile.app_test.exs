Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.AppTest do
  use MixTest.Case

  defmodule CustomProject do
    def project do
      [app: :custom_project, version: "0.2.0",
       description: "Some UTF-8 description (uma descrição em UTF-8)"]
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
      Process.get(:application)
    end
  end

  defmodule InvalidVsnProject do
    def project do
      [app: :invalid_vsn_project, version: "0.3"]
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
      assert contents =~ "Some UTF-8 description (uma descrição em UTF-8)"
    end
  end

  test "application properties validation" do
    Mix.Project.push InvalidProject

    in_fixture "no_mixfile", fn ->
      Process.put(:application, [:not_a_keyword, applications: []])
      message = "Application configuration returned from application/0 should be a keyword list"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, [modules: :invalid])
      message = "Application modules (:modules) should be a list of atoms, got: :invalid"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, [maxT: :invalid])
      message = "Application maximum time (:maxT) is not an integer or :infinity, got: :invalid"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, [registered: ["invalid"]])
      message = "Application registered processes (:registered) should be a list of atoms, got: [\"invalid\"]"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, [included_applications: ["invalid"]])
      message = "Application included applications (:included_applications) should be a list of atoms, got: [\"invalid\"]"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, [applications: ["invalid"]])
      message = "Application dependencies (:applications) should be a list of atoms, got: [\"invalid\"]"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, [applications: nil])
      message = "Application dependencies (:applications) should be a list of atoms, got: nil"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, [env: [:invalid]])
      message = "Application dependencies (:env) should be a keyword list, got: [:invalid]"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, [mod: {Mod}])
      message = "Application callback module (:mod) should be either [] or {module, start_args}, got: {Mod}"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, [start_phases: [:invalid]])
      message = "Application start phases (:start_phases) should be a keyword list, got: [:invalid]"
      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
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
end
