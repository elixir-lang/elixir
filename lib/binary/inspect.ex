import Elixir.Builtin, except: [inspect: 1]

defprotocol Binary.Inspect do
  @moduledoc """
  The `Binary.Inspect` protocol is responsible for
  converting any structure to a Binary for textual
  representation. All basic data structures (tuple,
  list, function, pid, etc) implement the inspect
  protocol. Other structures are adviced to implement
  the protocol in order to provide pretty printing.
  """

  @only [BitString, List, Record, Tuple, Atom, Number, Any]

  def inspect(thing)
end

defimpl Binary.Inspect, for: Atom do
  @doc """
  Represents the atom as an Elixir term.
  The atoms false, true and nil are simply
  quoted. Modules are properly represented
  as modules using the dot notation.

  ## Examples

      inspect(:foo)    #=> ":foo"
      inspect(nil)     #=> "nil"
      inspect(Foo.Bar) #=> "Foo.Bar"

  """
  def inspect(false), do: "false"
  def inspect(true),  do: "true"
  def inspect(nil),   do: "nil"
  def inspect(:""),   do: ":\"\""

  def inspect(atom) do
    binary = atom_to_binary(atom)

    cond do
      valid_identifier?(binary) == <<>> ->
          ":" <> binary
      valid_ref_identifier?(binary) == <<>> ->
        "__MAIN__." <> rest = binary
        rest
      true ->
        ":" <> Binary.escape(binary, ?")
    end
  end

  # Detect if atom is a module reference (__MAIN__.Foo.Bar.Baz)

  defp valid_ref_identifier?("__MAIN__" <> rest) do
    valid_ref_piece?(rest)
  end

  defp valid_ref_identifier?(rest) do
    rest
  end

  defp valid_ref_piece?(<<?., h, t|:binary>>) when h >= ?A and h <= ?Z do
    valid_ref_piece? valid_identifier?(t)
  end

  defp valid_ref_piece?(other), do: other

  # Detect if atom is :letter_or_underscore

  defp valid_identifier?(<<h, t|:binary>>) \
    when h >= ?a and h <= ?z \
    when h >= ?A and h <= ?Z \
    when h == ?_ do
    valid_identifier? t
  end

  defp valid_identifier?(other), do: other
end

defimpl Binary.Inspect, for: BitString do
  @doc %B"""
  Represents the string as itself escaping
  all necessary characters.

  ## Examples

      inspect("bar")   #=> "bar"
      inspect("f\"oo") #=> "f\"oo"

  """
  def inspect(thing) when is_binary(thing) do
    if Binary.printable?(thing) do
      Binary.escape(thing, ?")
    else
      as_bitstring(thing)
    end
  end

  def inspect(thing) do
    as_bitstring(thing)
  end

  ## Helpers

  defp as_bitstring(thing) do
    erlang = Erlang.io_lib.format('~p', [thing])
    list_to_binary List.reverse(replace(erlang, []))
  end

  defp replace([?:|t], acc),                do: replace(t, [?||acc])
  defp replace([h|t], acc) when is_list(h), do: replace(t, replace(h, acc))
  defp replace([h|t], acc),                 do: replace(t, [h|acc])
  defp replace([], acc),                    do: acc
end

defimpl Binary.Inspect, for: List do

  @doc %B"""
  Represents a list checking if it can be printed or not.
  If so, a single-quoted representation is returned,
  otherwise the brackets syntax is used.

  ## Examples

      inspect('bar')       #=> 'bar'
      inspect([0|'bar'])   #=> "[0,98,97,114]"
      inspect([:foo,:bar]) #=> "[:foo, :bar]"

  """

  def inspect([]), do: "[]"

  def inspect(thing) do
    if Erlang.io_lib.printable_list(thing) do
      Binary.escape(list_to_binary(thing), ?')
    else
      container_join(thing, "[", "]")
    end
  end

  ## Helpers

  def container_join([h], acc, last) do
    acc <> Binary.Inspect.inspect(h) <> last
  end

  def container_join([h|t], acc, last) when is_list(t) do
    acc = acc <> Binary.Inspect.inspect(h) <> ","
    container_join(t, acc, last)
  end

  def container_join([h|t], acc, last) do
    acc <> Binary.Inspect.inspect(h) <> "|" <> Binary.Inspect.inspect(t) <> last
  end

  def container_join([], acc, last) do
    acc <> last
  end
end

defimpl Binary.Inspect, for: Tuple do
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
    [_|fields]    = lc { field, _ } in name.__record__(:fields), do: field
    Binary.Inspect.Atom.inspect(name) <> records_join(fields, tail, "[", "]")
  end

  def inspect(thing) do
    list = tuple_to_list(thing)
    [name|tail] = list

    if is_record?(name) do
      fields = lc { field, _ } in name.__record__(:fields), do: field
      Binary.Inspect.Atom.inspect(name) <> records_join(fields, tail, "[", "]")
    else
      Binary.Inspect.List.container_join(list, "{", "}")
    end
  end

  ## Helpers

  defp is_record?(name) do
    is_atom(name) and match?("__MAIN__." <> _, atom_to_binary(name, :utf8)) and
      :erlang.function_exported(name, :__record__, 1)
  end

  defp records_join([f], [v], acc, last) do
    acc <> atom_to_binary(f, :utf8) <> ": " <> Binary.Inspect.inspect(v) <> last
  end

  defp records_join([fh|ft], [vh|vt], acc, last) do
    acc = acc <> atom_to_binary(fh, :utf8) <> ": " <> Binary.Inspect.inspect(vh) <> ", "
    records_join(ft, vt, acc, last)
  end

  defp records_join([], [], acc, last) do
    acc <> last
  end
end

defimpl Binary.Inspect, for: Number do
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

defimpl Binary.Inspect, for: Regex do
  @doc %B"""
  Represents the Regex using the `%r""` syntax.

  ## Examples

      inspect(%r/foo/m) #=> "%r\"foo\"m"

  """
  def inspect(thing) do
    "%r" <> Binary.Inspect.inspect(Regex.source(thing)) <> Regex.opts(thing)
  end
end

defimpl Binary.Inspect, for: Any do
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