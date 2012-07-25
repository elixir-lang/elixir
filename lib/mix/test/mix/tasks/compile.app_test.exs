Code.require_file "../../../test_helper", __FILE__

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
      [hello: 'beautiful']
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
      assert contents =~ %r/'Elixir-A'/
      assert contents =~ %r/\{applications,\[kernel,stdlib,elixir\]\}/

      assert Mix.Tasks.Compile.App.run([]) == :noop
    end
  after
    purge [A, B, C]
    Mix.Project.pop
  end

  test "use custom applicaiton settings" do
    Mix.Project.push CustomProject

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      Mix.Tasks.Compile.App.run([])
      contents = File.read!("ebin/custom_project.app")
      assert contents =~ %r/0.2.0/
      assert contents =~ %r/{hello,"beautiful"}/
    end
  after
    purge [A, B, C]
    Mix.Project.pop
  end
end