import Kernel, except: [inspect: 1]

defprotocol Binary.Inspect do
  @moduledoc """
  The `Binary.Inspect` protocol is responsible for
  converting any structure to a binary for textual
  representation. All basic data structures
  (tuple, list, function, pid, etc) implement the
  inspect protocol. Other structures are advised to
  implement the protocol in order to provide pretty
  printing.
  """

  @only [BitString, List, Tuple, Atom, Number, Any]

  def inspect(thing, opts)
end

defmodule Binary.Inspect.Utils do
  @moduledoc false

  ## container_join

  def container_join(tuple, first, last, opts) when is_tuple(tuple) do
    container_join(tuple_to_list(tuple), first, last, opts)
  end

  def container_join(list, first, last, opts) do
    first <> do_container_join(list, opts, Keyword.get(opts, :limit, :infinity)) <> last
  end

  defp do_container_join(_, _opts, 0) do
    "..."
  end

  defp do_container_join([h], opts, _counter) do
    Binary.Inspect.inspect(h, opts)
  end

  defp do_container_join([h|t], opts, counter) when is_list(t) do
    Binary.Inspect.inspect(h, opts) <> "," <> do_container_join(t, opts, decrement(counter))
  end

  defp do_container_join([h|t], opts, _counter) do
    Binary.Inspect.inspect(h, opts) <> "|" <> Binary.Inspect.inspect(t, opts)
  end

  defp do_container_join([], _opts, _counter) do
    ""
  end

  defp decrement(:infinity), do: :infinity
  defp decrement(counter),   do: counter - 1

  ## escape

  # It is considerably faster to loop the binary
  # and convert it to a list as we go compared
  # to looping the binary and creating a binary
  # as we go.
  def escape(other, char) do
    list_to_binary [char|do_escape(other, char)]
  end

  defp do_escape(<<char, t :: binary>>, char) do
    [?\\, char | do_escape(t, char)]
  end

  defp do_escape(<<h, t :: binary>>, char) when
    h == ?#  or h == ?\b or
    h == ?\d or h == ?\e or
    h == ?\f or h == ?\n or
    h == ?\r or h == ?\\ or
    h == ?\t or h == ?\v do
    [?\\, escape_map(h) | do_escape(t, char)]
  end

  defp do_escape(<<h, t :: binary>>, char) do
    [h | do_escape(t,char)]
  end

  defp do_escape(<<>>, char) do
    [char]
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

defimpl Binary.Inspect, for: Atom do
  require Macro
  import Binary.Inspect.Utils

  @moduledoc """
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

  def inspect(false, _),  do: "false"
  def inspect(true, _),   do: "true"
  def inspect(nil, _),    do: "nil"
  def inspect(:"", _),    do: ":\"\""
  def inspect(Elixir, _), do: "Elixir"

  def inspect(atom, _) do
    binary = atom_to_binary(atom)

    cond do
      valid_atom_identifier?(binary) ->
        ":" <> binary
      valid_ref_identifier?(binary) ->
        Module.to_binary(atom)
      atom in Macro.binary_ops or atom in Macro.unary_ops ->
        ":" <> binary
      true ->
        ":" <> escape(binary, ?")
    end
  end

  # Detect if atom is an atom alias (Elixir-Foo-Bar-Baz)

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

defimpl Binary.Inspect, for: BitString do
  import Binary.Inspect.Utils

  @moduledoc %B"""
  Represents the string as itself escaping
  all necessary characters.

  ## Examples

      inspect("bar")   #=> "bar"
      inspect("f\"oo") #=> "f\"oo"

  """

  def inspect(thing, opts) when is_binary(thing) do
    if String.printable?(thing) do
      escape(thing, ?")
    else
      as_bitstring(thing, opts)
    end
  end

  def inspect(thing, opts) do
    as_bitstring(thing, opts)
  end

  ## Bitstrings

  defp as_bitstring(bitstring, opts) do
    "<<" <> each_bit(bitstring, Keyword.get(opts, :limit, :infinity)) <> ">>"
  end

  defp each_bit(_, 0) do
    "..."
  end

  defp each_bit(<<h, t :: bitstring>>, counter) when t != <<>> do
    integer_to_binary(h) <> "," <> each_bit(t, decrement(counter))
  end

  defp each_bit(<<h :: size(8)>>, _counter) do
    integer_to_binary(h)
  end

  defp each_bit(<<>>, _counter) do
    <<>>
  end

  defp each_bit(bitstring, _counter) do
    size = bit_size(bitstring)
    <<h :: size(size)>> = bitstring
    integer_to_binary(h) <> "::size(" <> integer_to_binary(size) <> ")"
  end

  defp decrement(:infinity), do: :infinity
  defp decrement(counter),   do: counter - 1
end

defimpl Binary.Inspect, for: List do
  import Binary.Inspect.Utils

  @moduledoc %B"""
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

  def inspect([], _), do: "[]"

  def inspect(thing, opts) do
    cond do
      printable?(thing) ->
        escape(list_to_binary(thing), ?')
      Keyword.keyword?(thing) ->
        "[" <> join_keywords(thing, opts) <> "]"
      true ->
        container_join(thing, "[", "]", opts)
    end
  end

  defp join_keywords(thing, opts) do
    Enum.join(lc {key, value} inlist thing do
      key_to_binary(key, opts) <> ": " <> Binary.Inspect.inspect(value, opts)
    end, ", ")
  end

  defp key_to_binary(key, opts) do
    case Binary.Inspect.Atom.inspect(key, opts) do
      ":" <> right -> right
      other -> other
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

defimpl Binary.Inspect, for: Tuple do
  import Binary.Inspect.Utils

  @moduledoc """
  Inspect tuples. If the tuple represents a record,
  it shows it nicely formatted using the access syntax.

  ## Examples

      inspect({1,2,3})            #=> "{1,2,3}"
      inspect(ArgumentError.new)  #=> ArgumentError[message: "argument error"]

  """

  def inspect({}, _), do: "{}"

  def inspect(tuple, opts) do
    unless opts[:raw] do
      record_protocol(tuple, opts)
    end || container_join(tuple, "{", "}", opts)
  end

  ## Helpers

  defp record_protocol(tuple, opts) do
    name = elem(tuple, 0)

    if is_atom(name) and match?("Elixir-" <> _, atom_to_binary(name)) do
      unless name in [BitString, List, Tuple, Atom, Number, Any] do
        try do
          target = Module.concat(Binary.Inspect, name)
          target.inspect(tuple, opts)
        rescue
          UndefinedFunctionError ->
            record_inspect(tuple, opts)
        end
      end
    end
  end

  defp record_inspect(record, opts) do
    list = tuple_to_list(record)
    [name|tail] = list

    if (fields = record_fields(name)) && (length(fields) == size(record) - 1) do
      if Enum.first(tail) == :__exception__ do
        record_join(name, tl(fields), tl(tail), opts)
      else
        record_join(name, fields, tail, opts)
      end
    end
  end

  defp record_fields(name) do
    try do
      name.__record__(:fields)
    rescue
      _ -> nil
    end
  end

  defp record_join(name, fields, tail, opts) do
    fields = lc { field, _ } inlist fields, do: field
    Binary.Inspect.Atom.inspect(name, opts) <> "[" <>
      record_join(fields, tail, opts) <> "]"
  end

  defp record_join([f], [v], opts) do
    atom_to_binary(f, :utf8) <> ": " <> Binary.Inspect.inspect(v, opts)
  end

  defp record_join([fh|ft], [vh|vt], opts) do
    atom_to_binary(fh, :utf8) <> ": " <>
      Binary.Inspect.inspect(vh, opts) <> ", " <>
      record_join(ft, vt, opts)
  end

  defp record_join([], [], _opts) do
    ""
  end
end

defimpl Binary.Inspect, for: Number do
  @moduledoc """
  Represents the number as a binary.

  ## Examples

      inspect(1) #=> "1"

  """

  def inspect(thing, _) when is_integer(thing) do
    list_to_binary integer_to_list(thing)
  end

  def inspect(thing, _) do
    list_to_binary :io_lib.format("~p", [thing])
  end
end

defimpl Binary.Inspect, for: Regex do
  @moduledoc %B"""
  Represents the Regex using the `%r""` syntax.

  ## Examples

      inspect(%r/foo/m) #=> "%r\"foo\"m"

  """

  def inspect(regex, _opts) when size(regex) == 5 do
    "%r" <> Binary.Inspect.inspect(Regex.source(regex), []) <> Regex.opts(regex)
  end

  def inspect(other, opts) do
    Binary.Inspect.inspect other, Keyword.put(opts, :raw, true)
  end
end

defimpl Binary.Inspect, for: Any do
  @moduledoc """
  For all other terms not implemented, we use the default
  Erlang representation.

  ## Examples

      inspect Process.self #=> "<0.35.0>"

  """

  def inspect(thing, _) do
    iolist_to_binary :io_lib.format('~p', [thing])
  end
end