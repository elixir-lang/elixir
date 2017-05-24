ExUnit.start [trace: "--trace" in System.argv]

defmodule EExTest.Case do
  def os_newline do
    case :os.type do
      {:win32, _} -> "\r\n"
      _ -> "\n"
    end
  end
end