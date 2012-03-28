defmodule URI.SFTP do
  @behavior URI.Parser
  def default_port(), do: 22
  def parse(parsed_uri), do: parsed_uri
end
