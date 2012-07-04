defmodule URI.FTP do
  @behavior URI.Parser
  def default_port(), do: 21
  def parse(info), do: info
end
