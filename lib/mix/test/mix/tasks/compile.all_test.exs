Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.CompileAllTest do
  use MixTest.Case

  test "returns diagnostics from compilers" do
    in_fixture "no_mixfile", fn ->
      Mix.Project.push MixTest.Case.Sample

      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(), do: $$$
      end
      """)

      file = Path.absname("lib/a.ex")
      ExUnit.CaptureIO.capture_io(fn ->
        assert {:error, [%Mix.Task.Compiler.Diagnostic{
          file: ^file,
          severity: :error,
          position: 2,
          message: "** (SyntaxError) lib/a.ex:2:" <> _,
          compiler_name: "Elixir"
        }]} = Mix.Tasks.Compile.All.run([])
      end)
    end
  end
end
