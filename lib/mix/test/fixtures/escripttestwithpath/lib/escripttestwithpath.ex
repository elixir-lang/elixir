defmodule Escripttestwithpath do
  def start do
    :ok = :application.start(:escripttestwithpath)
  end
  
  def main(_args) do
    IO.puts "TEST_WITH_PATH"
  end
end
