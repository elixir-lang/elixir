defmodule Elixir::Functions do
  # Raises an `Error` with the given message.
  #
  # ## Example
  #
  #     raise "Given values do not match"
  #
  def raise(msg) when is_binary(msg) do
    error Error.new(message: msg)
  end

  # Receives a reference for an exception
  # and instantiates a new exception record
  #
  # ## Example
  #
  #     raise ArgumentError
  #
  def raise(atom) when is_atom(atom) do
    error atom.new
  end

  # Receives an already existing exception and re-raises it.
  #
  # ## Example
  #
  #     try do
  #       1 + :foo
  #     rescue: x in [BadargError]
  #       IO.puts "that was expected"
  #       raise x
  #     end
  #
  def raise(exception) when is_exception(exception) do
    error exception
  end

  # Receives a reference for an exception
  # and instantiates a new exception record
  # with the given message.
  #
  # ## Example
  #
  #     raise ArgumentError, "Expected a protocol"
  #
  def raise(atom, msg) when is_atom(atom) & is_binary(msg) do
    error atom.new(message: msg)
  end
end