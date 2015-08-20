defmodule Escripttest do
  def start do
    :ok = Application.start(:escripttest)
  end
  
  def main([]) do
    IO.puts Application.get_env(:foobar, :value, "TEST")
  end

  def main([protocol]) do
    IO.puts Protocol.consolidated?(Module.concat([protocol]))
  end
end
