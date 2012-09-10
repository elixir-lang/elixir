import Kernel, except: [inspect: 1]

defprotocol String.Inspect do
  @moduledoc """
  The `String.Inspect` protocol is responsible for
  converting any structure to a string (i.e. an utf-8
  binary) for textual representation. All basic data
  structures (tuple, list, function, pid, etc) implement
  the inspect protocol. Other structures are adviced to
  implement the protocol in order to provide pretty
  printing.
  """

  @only [BitString, List, Record, Tuple, Atom, Number, Any]

  def inspect(thing)
end

defmodule String.Inspect.Utils do
  @moduledoc false

  ## container_join

  def container_join([h], acc, last) do
    acc <> String.Inspect.inspect(h) <> last
  end

  def container_join([h|t], acc, last) when is_list(t) do
    acc = acc <> String.Inspect.inspect(h) <> ","
    container_join(t, acc, last)
  end

  def container_join([h|t], acc, last) do
    acc <> String.Inspect.inspect(h) <> "|" <> String.Inspect.inspect(t) <> last
  end

  def container_join([], acc, last) do
    acc <> last
  end

  ## escape

  def escape(other, char) do
    <<char>> <> do_escape(other, char)
  end

  defp do_escape(<<char, t :: binary>>, char) do
    <<?\\, char, do_escape(t, char) :: binary>>
  end

  defp do_escape(<<h, t :: binary>>, char) when
    h == ?#  or h == ?\b or
    h == ?\d or h == ?\e or
    h == ?\f or h == ?\n or
    h == ?\r or h == ?\\ or
    h == ?\t or h == ?\v do
    <<?\\, escape_map(h), do_escape(t, char) :: binary>>
  end

  defp do_escape(<<h, t :: binary>>, char) do
    <<h, do_escape(t,char) :: binary>>
  end

  defp do_escape(<<>>, char) do
    <<char>>
  end

  defp escape_map(?#),  do: ?#
  defp escape_map(?\b), do: ?b
  defp escape_map(?\d), do: ?d
  defp escape_map(?\e), do: ?e
  defp escape_map(?\f), do: ?f
  defp escape_map(?\n), do: ?n
  defp escape_map(?\r), do: ?r
  defp escape_map(?\\), do: ?\\
  defp escape_map(?\t), do: ?t
  defp escape_map(?\v), do: ?v
end

defimpl String.Inspect, for: Atom do
  require Macro
  import String.Inspect.Utils

  @doc """
  Represents the atom as an Elixir term. The atoms false, true
  and nil are simply quoted. Modules are properly represented
  as modules using the dot notation.

  Notice that in Elixir, all operators can be represented using
  literal atoms (`:+`, `:-`, etc).

  ## Examples

      inspect(:foo)    #=> ":foo"
      inspect(nil)     #=> "nil"
      inspect(Foo.Bar) #=> "Foo.Bar"

  """
  def inspect(false),  do: "false"
  def inspect(true),   do: "true"
  def inspect(nil),    do: "nil"
  def inspect(:""),    do: ":\"\""
  def inspect(Elixir), do: "Elixir"

  def inspect(atom) do
    binary = atom_to_binary(atom)

    cond do
      valid_atom_identifier?(binary) ->
        ":" <> binary
      valid_ref_identifier?(binary) ->
        "Elixir-" <> rest = binary
        bc <<r>> inbits rest, do: <<to_dot(r)>>
      atom in Macro.binary_ops or atom in Macro.unary_ops ->
        ":" <> binary
      true ->
        ":" <> escape(binary, ?")
    end
  end

  # Detect if atom is an atom alias (Elixir-Foo-Bar-Baz)

  defp to_dot(?-), do: ?.
  defp to_dot(l),  do: l

  defp valid_ref_identifier?("Elixir" <> rest) do
    valid_ref_piece?(rest)
  end

  defp valid_ref_identifier?(_), do: false

  defp valid_ref_piece?(<<?-, h, t :: binary>>) when h in ?A..?Z do
    valid_ref_piece? valid_identifier?(t)
  end

  defp valid_ref_piece?(<<>>), do: true
  defp valid_ref_piece?(_),    do: false

  # Detect if atom

  defp valid_atom_identifier?(<<h, t :: binary>>) when h in ?a..?z or h in ?A..?Z or h == ?_ do
    case valid_identifier?(t) do
      <<>>   -> true
      <<??>> -> true
      <<?!>> -> true
      _      -> false
    end
  end

  defp valid_atom_identifier?(_), do: false

  defp valid_identifier?(<<h, t :: binary>>)
      when h in ?a..?z
      when h in ?A..?Z
      when h in ?0..?9
      when h == ?_ do
    valid_identifier? t
  end

  defp valid_identifier?(other), do: other
end

defimpl String.Inspect, for: BitString do
  import String.Inspect.Utils

  @doc %B"""
  Represents the string as itself escaping
  all necessary characters.

  ## Examples

      inspect("bar")   #=> "bar"
      inspect("f\"oo") #=> "f\"oo"

  """
  def inspect(thing) when is_binary(thing) do
    if String.printable?(thing) do
      escape(thing, ?")
    else
      as_bitstring(thing)
    end
  end

  def inspect(thing) do
    as_bitstring(thing)
  end

  ## Bitstrings

  defp as_bitstring(bitstring) do
    "<<" <> each_bit(bitstring) <> ">>"
  end

  defp each_bit(<<h, t :: bitstring>>) when t != <<>> do
    integer_to_binary(h) <> "," <> each_bit(t)
  end

  defp each_bit(<<h :: size(8)>>) do
    integer_to_binary(h)
  end

  defp each_bit(<<>>) do
    <<>>
  end

  defp each_bit(bitstring) do
    size = bit_size(bitstring)
    <<h :: size(size)>> = bitstring
    integer_to_binary(h) <> "|" <> integer_to_binary(size)
  end

  defp integer_to_binary(integer) do
    integer /> integer_to_list /> list_to_binary
  end
end

defimpl String.Inspect, for: List do
  import String.Inspect.Utils

  @doc %B"""
  Represents a list checking if it can be printed or not.
  If so, a single-quoted representation is returned,
  otherwise the brackets syntax is used.

  Inspecting a list is conservative as it does not try
  to guess how the list is encoded. That said, `'josé'`
  will likely be inspected as `[106,111,115,195,169]`
  because we can't know if it is encoded in utf-8
  or iso-5569-1, which is common in Erlang libraries.

  ## Examples

      inspect('bar')       #=> 'bar'
      inspect([0|'bar'])   #=> "[0,98,97,114]"
      inspect([:foo,:bar]) #=> "[:foo, :bar]"

  """

  def inspect([]), do: "[]"

  def inspect(thing) do
    if printable?(thing) do
      escape(list_to_binary(thing), ?')
    else
      container_join(thing, "[", "]")
    end
  end

  ## printable?

  defp printable?([c|cs]) when is_integer(c) and c in 32..126 do
    printable?(cs)
  end

  defp printable?([c|cs]) when c in [?\n, ?\r, ?\t, ?\v, ?\b, ?\f, ?\e] do
    printable?(cs)
  end

  defp printable?([]), do: true
  defp printable?(_),  do: false
end

defimpl String.Inspect, for: Tuple do
  import String.Inspect.Utils

  @doc """
  Inspect tuples. If the tuple represents a record,
  it shows it nicely formatted using the access syntax.

  ## Examples

      inspect({1,2,3})            #=> "{1,2,3}"
      inspect(ArgumentError.new)  #=> ArgumentError[message: "argument error"]

  """
  def inspect({}), do: "{}"

  def inspect(exception) when is_exception(exception) do
    [name,_|tail] = tuple_to_list(exception)
    [_|fields]    = lc { field, _ } inlist name.__record__(:fields), do: field
    String.Inspect.Atom.inspect(name) <> records_join(fields, tail, "[", "]")
  end

  def inspect(thing) do
    list = tuple_to_list(thing)
    [name|tail] = list

    if is_record?(name) do
      fields = lc { field, _ } inlist name.__record__(:fields), do: field
      if length(fields) != size(thing) - 1 do
        container_join(list, "{", "}")
      else
        String.Inspect.Atom.inspect(name) <> records_join(fields, tail, "[", "]")
      end
    else
      container_join(list, "{", "}")
    end
  end

  ## Helpers

  defp is_record?(name) do
    is_atom(name) and match?("Elixir-" <> _, atom_to_binary(name, :utf8)) and
      function_exported?(name, :__record__, 1)
  end

  defp records_join([f], [v], acc, last) do
    acc <> atom_to_binary(f, :utf8) <> ": " <> String.Inspect.inspect(v) <> last
  end

  defp records_join([fh|ft], [vh|vt], acc, last) do
    acc = acc <> atom_to_binary(fh, :utf8) <> ": " <> String.Inspect.inspect(vh) <> ", "
    records_join(ft, vt, acc, last)
  end

  defp records_join([], [], acc, last) do
    acc <> last
  end
end

defimpl String.Inspect, for: Number do
  @doc """
  Represents the number as a binary.

  ## Examples

      inspect(1) #=> "1"

  """
  def inspect(thing) when is_integer(thing) do
    list_to_binary integer_to_list(thing)
  end

  def inspect(thing) do
    list_to_binary float_to_list(thing)
  end
end

defimpl String.Inspect, for: Regex do
  @doc %B"""
  Represents the Regex using the `%r""` syntax.

  ## Examples

      inspect(%r/foo/m) #=> "%r\"foo\"m"

  """
  def inspect(thing) do
    "%r" <> String.Inspect.inspect(Regex.source(thing)) <> Regex.opts(thing)
  end
end

defimpl String.Inspect, for: Any do
  @doc """
  For all other terms not implemented, we use the default
  Erlang representation.

  ## Examples

      inspect Process.self #=> "<0.35.0>"

  """
  def inspect(thing) do
    iolist_to_binary Erlang.io_lib.format('~p', [thing])
  end
end