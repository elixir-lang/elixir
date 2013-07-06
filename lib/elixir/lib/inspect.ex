import Kernel, except: [inspect: 1]
import Inspect.Algebra

defrecord Inspect.Opts, raw: false, limit: :infinity, pretty: false, width: 80

defprotocol Inspect do
  @moduledoc """
  The `Inspect` protocol is responsible for
  converting any structure to a binary for textual
  representation. All basic data structures
  (tuple, list, function, pid, etc) implement the
  inspect protocol. Other structures are advised to
  implement the protocol in order to provide pretty
  printing.
  """

  def inspect(thing, opts)
end

defimpl Inspect, for: Atom do
  require Macro

  @doc """
  Represents the atom as an Elixir term. The atoms false, true
  and nil are simply quoted. Modules are properly represented
  as modules using the dot notation.

  Notice that in Elixir, all operators can be represented using
  literal atoms (`:+`, `:-`, etc).

  ## Examples

      iex> inspect(:foo)
      ":foo"
      iex> inspect(nil)
      "nil"
      iex> inspect(Foo.Bar)
      "Foo.Bar"

  """
  def inspect(atom, _opts) do
    inspect(atom)
  end

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
        Module.to_string(atom)
      atom in Macro.binary_ops or atom in Macro.unary_ops ->
        ":" <> binary
      true ->
        << ?:, ?", String.escape(binary, ?") :: binary, ?" >>
    end
  end

  # Detect if atom is an atom alias (Elixir.Foo.Bar.Baz)

  defp valid_ref_identifier?("Elixir" <> rest) do
    valid_ref_piece?(rest)
  end

  defp valid_ref_identifier?(_), do: false

  defp valid_ref_piece?(<<?., h, t :: binary>>) when h in ?A..?Z do
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

defimpl Inspect, for: BitString do
  @doc %B"""
  Represents the string as itself escaping
  all necessary characters.

  ## Examples

      iex> inspect("bar")
      "\"bar\""
      iex> inspect("f\"oo")
      "\"f\\\"oo\""

  """

  def inspect(thing, opts) when is_binary(thing) do
    if String.printable?(thing) do
      << ?", String.escape(thing, ?") :: binary, ?" >>
    else
      as_bitstring(thing, opts)
    end
  end

  def inspect(thing, opts) do
    as_bitstring(thing, opts)
  end

  ## Bitstrings

  defp as_bitstring(bitstring, Inspect.Opts[] = opts) do
    "<<" <> each_bit(bitstring, opts.limit) <> ">>"
  end

  defp each_bit(_, 0) do
    "..."
  end

  defp each_bit(<<h, t :: bitstring>>, counter) when t != <<>> do
    integer_to_binary(h) <> ", " <> each_bit(t, decrement(counter))
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

defimpl Inspect, for: List do
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

      iex> inspect('bar')
      "'bar'"
      iex> inspect([0|'bar'])
      "[0, 98, 97, 114]"
      iex> inspect([:foo,:bar])
      "[:foo, :bar]"

  """

  def inspect([], _opts), do: "[]"

  def inspect(thing, Inspect.Opts[] = opts) do
    cond do
      :io_lib.printable_list(thing) ->
        << ?', String.escape(:unicode.characters_to_binary(thing), ?') :: binary, ?' >>
      keyword?(thing) ->
        surround_many("[", thing, "]", opts.limit, keyword(&1, opts))
      true ->
        surround_many("[", thing, "]", opts.limit, Kernel.inspect(&1, opts))
    end
  end

  defp keyword({key, value}, opts) do
    concat(
      key_to_binary(key) <> ": ",
      Kernel.inspect(value, opts)
    )
  end

  defp key_to_binary(key) do
    case Inspect.Atom.inspect(key) do
      ":" <> right -> right
      other -> other
    end
  end

  defp keyword?([{ key, _value } | rest]) when is_atom(key) do
    case atom_to_list(key) do
      'Elixir.' ++ _ -> false
      _ -> keyword?(rest)
    end
  end

  defp keyword?([]),     do: true
  defp keyword?(_other), do: false
end

defimpl Inspect, for: Tuple do
  @doc """
  Inspect tuples. If the tuple represents a record,
  it shows it nicely formatted using the access syntax.

  ## Examples

      iex> inspect({1, 2, 3})
      "{1, 2, 3}"
      iex> inspect(ArgumentError.new)
      "ArgumentError[message: \\\"argument error\\\"]"

  """

  def inspect({}, _opts), do: "{}"

  def inspect(tuple, opts) do
    unless opts.raw do
      record_inspect(tuple, opts)
    end || surround_many("{", tuple_to_list(tuple), "}", opts.limit, Kernel.inspect(&1, opts))
  end

  ## Helpers

  defp record_inspect(record, opts) do
    [name|tail] = tuple_to_list(record)

    if is_atom(name) && (fields = record_fields(name)) && (length(fields) == size(record) - 1) do
      if Enum.first(tail) == :__exception__ do
        surround_record(name, tl(fields), tl(tail), opts)
      else
        surround_record(name, fields, tail, opts)
      end
    end || surround_many("{", [name|tail], "}", opts.limit, Kernel.inspect(&1, opts))
  end

  defp record_fields(name) do
    try do
      name.__record__(:fields)
    rescue
      _ -> nil
    end
  end

  defp surround_record(name, fields, tail, opts) do
    fields = lc { field, _ } inlist fields, do: field

    concat(
      Inspect.Atom.inspect(name, opts),
      surround_many("[", Enum.zip(fields, tail), "]", opts.limit, keyword(&1, opts))
    )
  end

  defp keyword({ k, v }, opts) do
    concat(
      atom_to_binary(k, :utf8) <> ": ",
      Kernel.inspect(v, opts)
    )
  end
end

defimpl Inspect, for: Number do
  @doc """
  Represents the number as a binary.

  ## Examples

      iex> inspect(1)
      "1"

  """
  def inspect(thing, _opts) when is_integer(thing) do
    integer_to_binary(thing)
  end

  def inspect(thing, _opts) do
    list_to_binary(:io_lib_format.fwrite_g(thing))
  end
end

defimpl Inspect, for: Regex do
  @moduledoc %B"""
  Represents the Regex using the `%r""` syntax.

  ## Examples

      iex> inspect(%r/foo/m)
      "%r\"foo\"m"

  """
  def inspect(regex, opts) when size(regex) == 5 do
    concat ["%r", Kernel.inspect(Regex.source(regex), opts), Regex.opts(regex)]
  end

  def inspect(other, opts) do
    Kernel.inspect(other, opts.raw(true))
  end
end

defimpl Inspect, for: Function do
  @moduledoc """
  Inspect functions, when possible, in a literal form.
  """
  def inspect(function, _opts) do
    fun_info = :erlang.fun_info(function)
    if fun_info[:type] == :external and fun_info[:env] == [] do
      "function(#{Inspect.Atom.inspect(fun_info[:module])}.#{fun_info[:name]}/#{fun_info[:arity]})"
    else
      '#Fun' ++ rest = :erlang.fun_to_list(function)
      "#Function" <> list_to_binary(rest)
    end
  end
end

defimpl Inspect, for: PID do
  def inspect(pid, _opts) do
    "#PID" <> list_to_binary(pid_to_list(pid))
  end
end

defimpl Inspect, for: Port do
  def inspect(port, _opts) do
    list_to_binary :erlang.port_to_list(port)
  end
end

defimpl Inspect, for: Reference do
  def inspect(ref, _opts) do
    '#Ref' ++ rest = :erlang.ref_to_list(ref)
    "#Reference" <> list_to_binary(rest)
  end
end

defimpl Inspect, for: HashDict do
  def inspect(dict, opts) do
    concat ["#HashDict<", Inspect.List.inspect(HashDict.to_list(dict), opts), ">"]
  end
end

defimpl Inspect, for: HashSet do
  def inspect(set, opts) do
    concat ["#HashSet<", Inspect.List.inspect(HashSet.to_list(set), opts), ">"]
  end
end
