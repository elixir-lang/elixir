defmodule URI.WSS do
  @behavior URI.Parser
  def default_port(), do: 443
  def parse(info), do: info
end
