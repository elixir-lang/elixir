defmodule URI.FTP do
  @behavior URI.Parser
  def default_port(), do: 21
  def parse(parsed_uri), do: parsed_uri
end
