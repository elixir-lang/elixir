defmodule AtExit do
  def at_exit(str) do
    System.at_exit fn(_, do: IO.print str)
  end
end
System.at_exit fn(status, do: IO.puts "cruel world with status #{status}")
AtExit.at_exit("goodbye ")
exit(0)