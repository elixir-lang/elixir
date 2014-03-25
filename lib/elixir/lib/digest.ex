defmodule Digest do
  @moduledoc """
  This module provides the framework for message digest libraries.
  """
  use Bitwise

  @doc """
  Hash a string using the MD5 hash algorithm.

  ## Examples

    iex> Digest.md5("hello")
    "5d41402abc4b2a76b9719d911017c592"

  """
  @spec md5(String.t) :: String.t | no_return
  def md5(string) when is_binary(string) do
    <<x :: [unsigned, big, integer, size(128)]>> = string |> :erlang.md5
    :io_lib.format('~32.16.0b', [x]) |> iolist_to_binary
  end

  @doc """
  Converts a string to it's hexadecimal representation.

  ## Examples

    iex> Digest.to_hex("test")
    "74657374"
    iex> Digest.to_hex("æß")
    "C3A6C39F"

  """
  @spec to_hex(String.t) :: String.t | no_return
  def to_hex(""), do: ""
  def to_hex(string) when is_binary(string) do
    bc <<x :: size(4), y :: size(4)>> inbits string do <<hex_char(x), hex_char(y)>> end |> String.upcase
  end

  # Get the hex character for a given character within the range 0-16
  defp hex_char(n) when n < 10, do: ?0 + n
  defp hex_char(n) when n < 16, do: ?a + (n - 10)

end