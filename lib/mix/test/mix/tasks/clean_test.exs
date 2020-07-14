Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.CleanTest do
  use MixTest.Case

  defmodule Sample do
    def project do
      [
        app: :sample,
        version: "0.1.0",
        deps: [
          {:ok, "0.1.0", path: "deps/ok"},
          {:unknown, "0.1.0", git: "deps/unknown"}
        ]
      ]
    end
  end

  test "cleans the application build" do
    Mix.Project.push(Sample)

    in_fixture("deps_status", fn ->
      File.mkdir_p!("_build/dev/lib/sample/consolidated")
      File.mkdir_p!("_build/dev/lib/sample")
      File.mkdir_p!("_build/test/lib/sample")
      File.mkdir_p!("_build/dev/lib/ok")

      Mix.Tasks.Clean.run([])
      refute File.exists?("_build/dev/lib/sample/consolidated")
      refute File.exists?("_build/dev/lib/sample")
      refute File.exists?("_build/test/lib/sample")
      assert File.exists?("_build/dev/lib/ok")
    end)
  end

  test "cleans dependencies build" do
    Mix.Project.push(Sample)

    in_fixture("deps_status", fn ->
      File.mkdir_p!("_build/dev/lib/ok")
      File.mkdir_p!("_build/test/lib/ok")

      Mix.Tasks.Clean.run(["--deps", "--only", "dev"])
      refute File.exists?("_build/dev")
      assert File.exists?("_build/test")

      Mix.Tasks.Clean.run(["--deps"])
      refute File.exists?("_build/test")
    end)
  end

  test "invokes compiler hook defined in project" do
    Mix.ProjectStack.post_config(compilers: Mix.compilers() ++ [:testc])
    Mix.Project.push(MixTest.Case.Sample)

    in_fixture("no_mixfile", fn ->
      File.write!("lib/testc.ex", """
      defmodule Mix.Tasks.Compile.Testc do
        use Mix.Task.Compiler

        @impl true
        def run(_args) do
          Mix.shell().info("Compiling...")
          :ok
        end

        @impl true
        def clean do
          Mix.shell().info("Cleaning...")
          :ok
        end
      end
      """)

      Mix.Task.run("compile")
      assert_received {:mix_shell, :info, ["Compiling..."]}
      purge([Mix.Tasks.Compile.Testc])

      Mix.Task.run("clean")
      assert_received {:mix_shell, :info, ["Cleaning..."]}
    end)
  end
end
