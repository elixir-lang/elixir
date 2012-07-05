defmodule URI.LDAP do
  @behavior URI.Parser
  def default_port(), do: 389

  # TODO: LDAP specific parsing.
  def parse(info), do: info
end
