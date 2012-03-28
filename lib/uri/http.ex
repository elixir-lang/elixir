defmodule URI.HTTP do
  @behavior URI.Parser
  def default_port(), do: 80
  def parse(parsed_uri), do: parsed_uri
end
