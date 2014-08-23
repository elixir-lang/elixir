defmodule Escripttest do
  def main([protocol]) do
    IO.puts Protocol.consolidated?(Module.concat([protocol]))
  end
end
