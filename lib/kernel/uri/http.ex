defmodule URI.HTTP do
  @behavior URI.Parser
  def default_port(), do: 80
  def parse(info), do: info
end
