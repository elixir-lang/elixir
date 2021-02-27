defmodule EscriptTest do
  def start do
    :ok = Application.start(:escript_test)
  end

  def main(["--protocol", protocol]) do
    IO.puts(Protocol.consolidated?(Module.concat([protocol])))
  end

  def main(["--nesting"]) do
    IO.inspect(Application.get_env(:foobar, :nesting, "TEST"))
  end

  def main(_argv) do
    IO.puts(Application.get_env(:foobar, :value, "TEST"))
  end
end
