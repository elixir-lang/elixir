Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.WarningTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  test :invalid_token do
    assert capture_io(fn ->
      defmodule Sample do
        defp hello(0), do: hello(1)
        defp hello(1), do: :ok
      end
    end) =~ %r"function hello/1 is unused"
  after
    purge Sample
  end

  defp purge(module) do
    :code.delete module
    :code.purge module
  end
end
