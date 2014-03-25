defmodule Digest do
  @moduledoc """
  This module provides the framework for message digest libraries.
  """

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
    "c3a6c39f"

  """
  @spec to_hex(String.t) :: String.t | no_return
  def to_hex(""), do: ""
  def to_hex(string) when is_binary(string) do
    bc <<x :: size(4), y :: size(4)>> inbits string do <<hex_char(x), hex_char(y)>> end
  end

  @doc """
  Converts a hexadecimal-encoded string to it's unencoded representation.

  ## Examples

    iex> Digest.from_hex("74657374")
    "test"
    iex> Digest.from_hex("c3a6c39f")
    "æß"
  """
  @spec from_hex(String.t) :: String.t | no_return
  def from_hex(""), do: ""
  def from_hex(string) when is_binary(string) do
    unhex_binary(string, <<>>)
  end


  # Get the hex character for a given character within the range 0-16
  defp hex_char(n) when n < 10, do: ?0 + n
  defp hex_char(n) when n < 16, do: ?a + (n - 10)
  # Get the integer value of a hex character in the range 0-F
  defp unhex_char(c) when c in ?0..?9, do: c - ?0
  defp unhex_char(c) when c in ?A..?F, do: (c - ?A) + 10
  defp unhex_char(c) when c in ?a..?f, do: (c - ?a) + 10
  # Recursively convert a hexadecimal string to it's unencoded representation
  defp unhex_binary(<<>>, acc), do: acc
  defp unhex_binary(<<c1 :: utf8, c2 :: utf8, rest :: binary>>, acc) do
    unhex_binary(rest, <<acc :: binary, unhex_char(c1) :: 4, unhex_char(c2) :: 4>>)
  end

end