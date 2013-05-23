Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.AppTest do
  use MixTest.Case

  defmodule SimpleProject do
    def project do
      [app: :simple_project, version: "0.1.0"]
    end
  end

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
    Mix.Project.push SimpleProject

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      assert Mix.Tasks.Compile.App.run([]) == :ok

      contents = File.read!("ebin/simple_project.app")
      assert contents =~ %r/\{application,simple_project/
      assert contents =~ %r/0.1.0/
      assert contents =~ %r/'Elixir.A'/
      assert contents =~ %r/\{applications,\[kernel,stdlib,elixir\]\}/

      assert Mix.Tasks.Compile.App.run([]) == :noop
    end
  after
    purge [A, B, C]
    Mix.Project.pop
  end

  test "use custom application settings" do
    Mix.Project.push CustomProject

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      Mix.Tasks.Compile.App.run([])
      contents = File.read!("ebin/custom_project.app")
      assert contents =~ %r/0.2.0/
      assert contents =~ %r/{maxT,infinity}/
    end
  after
    purge [A, B, C]
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
        assert e.message =~ %r/:#{error}/
        assert e.message =~ %r/#{inspect InvalidProject.application[error]}/
      end
      Process.delete(:error)
    end
  after
    purge [A, B, C]  
    Mix.Project.pop
  end

  test ".app contains description and registered (as required by systools)" do
    Mix.Project.push SimpleProject

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      assert Mix.Tasks.Compile.App.run([]) == :ok

      {:ok, [{_app, _, properties}]} = :file.consult("ebin/simple_project.app")
      properties = Keyword.from_enum(properties)
      assert properties[:registered] == []
      assert properties[:description] == 'simple_project'   

      assert Mix.Tasks.Compile.App.run([]) == :noop
    end
  after
    purge [A, B, C]
    Mix.Project.pop
  end  
end