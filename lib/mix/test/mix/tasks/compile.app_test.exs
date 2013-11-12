Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.AppTest do
  use MixTest.Case

  defmodule CustomProject do
    def project do
      [app: :custom_project, version: "0.2.0"]
    end

    def application do
      [maxT: :infinity]
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

  test "generates .app file when changes happen" do
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      assert Mix.Tasks.Compile.App.run([]) == :ok

      contents = File.read!("_build/shared/lib/sample/ebin/sample.app")
      assert contents =~ "{application,sample"
      assert contents =~ "0.1.0"
      assert contents =~ "'Elixir.A'"
      assert contents =~ "{applications,[kernel,stdlib,elixir]}"

      assert Mix.Tasks.Compile.App.run([]) == :noop
    end
  after
    Mix.Project.pop
  end

  test "use custom application settings" do
    Mix.Project.push CustomProject

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      Mix.Tasks.Compile.App.run([])
      contents = File.read!("_build/shared/lib/custom_project/ebin/custom_project.app")
      assert contents =~ "0.2.0"
      assert contents =~ "{maxT,infinity}"
    end
  after
    Mix.Project.pop
  end

  test "application properties validation" do
    Mix.Project.push InvalidProject

    in_fixture "no_mixfile", fn ->
      lc error inlist [:modules, :maxT, :registered, :included_applications,
                       :applications, :env, :mod, :start_phases] do
        Process.put(:error, error)
        e = catch_error(Mix.Tasks.Compile.App.run([]))
        assert Mix.Error[] = e
        assert e.message =~ ":#{error}"

        err_token = InvalidProject.application[error]
        cond do
          is_list(err_token) ->
            [tok] = err_token
            assert e.message =~ inspect(tok)
          true ->
            assert e.message =~ inspect(err_token)
        end
      end
      Process.delete(:error)
    end
  after
    Mix.Project.pop
  end

  test ".app contains description and registered (as required by systools)" do
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      assert Mix.Tasks.Compile.App.run([]) == :ok

      {:ok, [{_app, _, properties}]} = :file.consult("_build/shared/lib/sample/ebin/sample.app")
      properties = Keyword.from_enum(properties)
      assert properties[:registered] == []
      assert properties[:description] == 'sample'

      assert Mix.Tasks.Compile.App.run([]) == :noop
    end
  after
    Mix.Project.pop
  end
end
