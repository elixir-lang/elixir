defmodule AtExit do
  def at_exit(str) do
    Code.at_exit fn(_) { IO.print str }
  end
end
Code.at_exit fn(status){ IO.puts "cruel world with status #{status}" }
AtExit.at_exit("goodbye ")
exit(0)