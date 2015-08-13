Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.AllTest do
  use MixTest.Case
  import ExUnit.CaptureLog

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "add logger application metadata" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
      require Logger
      def info, do: Logger.info("hello")
      end
      """)

      assert Mix.Tasks.Compile.All.run([]) == :ok
      try do
        assert capture_log([metadata: [:application]], &A.info/0) =~ "application=sample"
      after
        purge [A]
      end
    end
  end
end
