defmodule URI.TFTP do
  @behavior URI.Parser
  def default_port(), do: 69
  def parse(info), do: info
end
