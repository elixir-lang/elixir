defmodule Escripttest do
  def start do
    :ok = Application.start(:escripttest)
  end
  
  def main(_args) do
    IO.puts Application.get_env(:foobar, :value, "TEST")
  end
end
