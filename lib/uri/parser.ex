defmodule URI.Parser do
  def behaviour_info(:callbacks) do
    [parse: 1,
     default_port: 0]
  end
end
