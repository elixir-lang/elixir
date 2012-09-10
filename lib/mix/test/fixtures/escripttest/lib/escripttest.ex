defmodule Escripttest do
  def start do
    :ok = :application.start(:escripttest)
  end
  
  def main(_args) do
    IO.puts "TEST"
  end
end
