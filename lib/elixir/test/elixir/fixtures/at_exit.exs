defmodule AtExit do
  @spec at_exit(String.t) :: :ok
  def at_exit(str) do
    System.at_exit fn(_) -> IO.write(str) end
  end
end
System.at_exit fn(status) -> IO.puts "cruel world with status #{status}" end
AtExit.at_exit("goodbye ")
exit({:shutdown, 1})
