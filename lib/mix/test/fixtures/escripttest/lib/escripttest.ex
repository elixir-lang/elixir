defmodule Escripttest do
  def start do
    :ok = Application.start(:escripttest)
  end
  
  def main(_args) do
    IO.puts "TEST"
  end
end
