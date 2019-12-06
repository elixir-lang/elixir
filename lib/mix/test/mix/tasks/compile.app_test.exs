Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Compile.AppTest do
  use MixTest.Case

  defmodule CustomProject do
    def project do
      [
        app: :custom_project,
        version: "0.2.0",
        description: "Some UTF-8 dëscriptión"
      ]
    end

    def application do
      [maxT: :infinity, applications: [:example_app], extra_applications: [:logger]]
    end
  end

  defmodule CustomDeps do
    def project do
      [app: :custom_deps, version: "0.2.0", deps: deps()]
    end

    def application do
      [extra_applications: [:logger], included_applications: [:ok9]]
    end

    def deps do
      [
        {:ok1, path: "../ok"},
        {:ok2, path: "../ok", only: :prod},
        {:ok3, path: "../ok", only: :dev},
        {:ok4, path: "../ok", runtime: true},
        {:ok5, path: "../ok", runtime: false},
        {:ok6, path: "../ok", optional: true},
        {:ok7, path: "../ok", optional: false},
        {:ok8, path: "../ok", app: false},
        {:ok9, path: "../ok"}
      ]
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
    Mix.Project.push(MixTest.Case.Sample)

    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      assert Mix.Tasks.Compile.App.run([]) == {:ok, []}

      properties = parse_resource_file(:sample)
      assert properties[:vsn] == '0.1.0'
      assert properties[:modules] == [A, B]
      assert properties[:applications] == [:kernel, :stdlib, :elixir]
      refute Keyword.has_key?(properties, :compile_env)

      assert Mix.Tasks.Compile.App.run([]) == {:noop, []}
    end)
  end

  test "generates .app file with compile_env" do
    Mix.Project.push(MixTest.Case.Sample)

    in_fixture("no_mixfile", fn ->
      Mix.ProjectStack.compile_env([{:app, :key, :error}])
      assert Mix.Tasks.Compile.App.run([]) == {:ok, []}
      assert parse_resource_file(:sample)[:compile_env] == [{:app, :key, :error}]

      # No-op with untouched unset compile_env
      assert Mix.Tasks.Compile.App.run([]) == {:noop, []}

      # No-op with same compile_env
      Mix.ProjectStack.compile_env([{:app, :key, :error}])
      assert Mix.Tasks.Compile.App.run([]) == {:noop, []}

      # Recompiles with new compile_env
      Mix.ProjectStack.compile_env([{:app, :another, :error}])
      assert Mix.Tasks.Compile.App.run([]) == {:ok, []}
      assert parse_resource_file(:sample)[:compile_env] == [{:app, :another, :error}]

      # Keeps compile_env if forcing
      assert Mix.Tasks.Compile.App.run(["--force"]) == {:ok, []}
      assert parse_resource_file(:sample)[:compile_env] == [{:app, :another, :error}]
    end)
  end

  test "uses custom application settings" do
    Mix.Project.push(CustomProject)

    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      Mix.Tasks.Compile.App.run([])

      properties = parse_resource_file(:custom_project)
      assert properties[:vsn] == '0.2.0'
      assert properties[:maxT] == :infinity
      assert properties[:applications] == [:kernel, :stdlib, :elixir, :logger, :example_app]
      assert properties[:description] == 'Some UTF-8 dëscriptión'
      refute Keyword.has_key?(properties, :extra_applications)
    end)
  end

  test "infers applications" do
    Mix.Project.push(CustomDeps)

    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      Mix.Tasks.Compile.App.run([])

      properties = parse_resource_file(:custom_deps)

      assert properties[:applications] ==
               [:kernel, :stdlib, :elixir, :logger, :ok1, :ok3, :ok4, :ok7]
    end)
  end

  test "validates properties" do
    Mix.Project.push(InvalidProject)

    in_fixture("no_mixfile", fn ->
      Process.put(:application, [:not_a_keyword, applications: []])
      message = "Application configuration returned from application/0 should be a keyword list"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, modules: :invalid)
      message = "Application modules (:modules) should be a list of atoms, got: :invalid"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, maxT: :invalid)
      message = "Application maximum time (:maxT) is not an integer or :infinity, got: :invalid"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, registered: ["invalid"])

      message =
        "Application registered processes (:registered) should be a list of atoms, got: [\"invalid\"]"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, extra_applications: ["invalid"])

      message =
        "Application extra applications (:extra_applications) should be a list of atoms, got: [\"invalid\"]"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, included_applications: ["invalid"])

      message =
        "Application included applications (:included_applications) should be a list of atoms, got: [\"invalid\"]"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, applications: ["invalid"])

      message =
        "Application applications (:applications) should be a list of atoms, got: [\"invalid\"]"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, applications: nil)
      message = "Application applications (:applications) should be a list of atoms, got: nil"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, env: [:invalid])
      message = "Application environment (:env) should be a keyword list, got: [:invalid]"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, mod: {Mod})

      message =
        "Application callback module (:mod) should be either [] or {module, start_args}, got: {Mod}"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end

      Process.put(:application, start_phases: [:invalid])

      message =
        "Application start phases (:start_phases) should be a keyword list, got: [:invalid]"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end
    end)
  end

  test ".app contains description and registered (as required by systools)" do
    Mix.Project.push(MixTest.Case.Sample)

    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      assert Mix.Tasks.Compile.App.run([]) == {:ok, []}

      properties = parse_resource_file(:sample)
      assert properties[:registered] == []
      assert properties[:description] == 'sample'
      assert properties[:applications] == [:kernel, :stdlib, :elixir]

      assert Mix.Tasks.Compile.App.run([]) == {:noop, []}
    end)
  end

  test "raises on invalid version" do
    Mix.Project.push(InvalidVsnProject)

    in_fixture("no_mixfile", fn ->
      message = "Expected :version to be a SemVer version, got: \"0.3\""

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Compile.App.run([])
      end
    end)
  end

  defp parse_resource_file(app) do
    {:ok, [term]} = :file.consult("_build/dev/lib/#{app}/ebin/#{app}.app")
    {:application, ^app, properties} = term
    properties
  end
end
