defmodule AtExit do
  def at_exit(str) do
    System.at_exit fn(_) -> IO.print(str) end
  end
end
System.at_exit fn(status) -> IO.puts "cruel world with status #{status}" end
AtExit.at_exit("goodbye ")
exit(0)