ExUnit.start()

defmodule MaxFailuresHelper do
  def sleep(time \\ 500, return_term) when is_integer(time) do
    Process.sleep(time)
    return_term
  end
end
