import Kernel, except: [inspect: 1]
import Inspect.Algebra

defrecord Inspect.Opts, raw: false, limit: :infinity, pretty: false, width: 80

defprotocol Inspect do
  @moduledoc """
  The `Inspect` protocol is responsible for converting any Elixir
  data structure into an algebra document. This document is then
  formatted, either in pretty printing format or a regular one.

  The `inspect/2` function receives the entity to be inspected
  followed by the inspecting options, represented by the record
  `Inspect.Opts`.

  Inspection is done using the functions available in
  `Inspect.Algebra` and by calling `Kernel.inspect/2` recursively
  passing the `Inspect.Opts` as an argument. When `Kernel.inspect/2`
  receives an `Inspect.Opts` record as the second argument, it returns
  the underlying algebra document instead of the formatted string.

  Many times, inspecting a structure can be implemented using functions
  of the existing entities. For example, here is `HashSet`'s `inspect`
  implementation:

      defimpl Inspect, for: HashSet do
        import Inspect.Algebra

        def inspect(dict, opts) do
          concat ["#HashSet<", Kernel.inspect(HashSet.to_list(dict), opts), ">"]
        end
      end

  The `concat` function comes from `Inspect.Algebra` and it
  concatenates algebra documents together. In the example above,
  it is concatenating the string `"HashSet<"` (all strings are
  valid algebra documents that keep their formatting when pretty
  printed), the document returned by `Kernel.inspect/2` and the
  other string `">"`.

  Since regular strings are valid entities in an algebra document,
  an implementation of inspect may simply return a binary,
  although that will devoid it of any pretty-printing.
  """

  def inspect(thing, opts)
end

defimpl Inspect, for: Atom do
  require Macro

  @doc """
  Represents the atom as an Elixir term. The atoms `false`, `true`
  and `nil` are simply quoted. Modules are properly represented
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
        "Elixir." <> rest = binary
        rest
      atom in [:{}, :[], :<<>>] ->
        ":" <> binary
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
  Represents a string as itself escaping all necessary
  characters. Bitstrings and strings that contain non-
  printable characters are printed using the bitstring
  syntax.

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
  Represents a list, checking if it can be printed or not.
  If so, a single-quoted representation is returned,
  otherwise the brackets syntax is used. Keywords are
  printed in keywords syntax.

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
      keyword?(thing) && not opts.raw ->
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
  Represents tuples. If the tuple represents a record,
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
    case atom_to_binary(name) do
      "Elixir." <> _ ->
        try do
          name.__record__(:fields)
        rescue
          _ -> nil
        end
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
  Represents the number as a string.

  Floats are represented using the shortened, correctly rounded string
  that converts to float when read back with `binary_to_float/1`. This
  is done via the Erlang implementation of _Printing Floating-Point
  Numbers Quickly and Accurately_ in Proceedings of the SIGPLAN '96
  Conference on Programming Language Design and Implementation.

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
  @doc %B"""
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
  def inspect(function, _opts) do
    fun_info = :erlang.fun_info(function)
    mod = fun_info[:module]

    if fun_info[:type] == :external and fun_info[:env] == [] do
      "&#{Inspect.Atom.inspect(mod)}.#{fun_info[:name]}/#{fun_info[:arity]}"
    else
      case atom_to_list(mod) do
        'elixir_compiler_' ++ _ ->
          if function_exported?(mod, :__RELATIVE__, 0) do
            "#Function<#{uniq(fun_info)} in file:#{mod.__RELATIVE__}>"
          else
            default_inspect(mod, fun_info)
          end
        _ ->
          default_inspect(mod, fun_info)
      end
    end
  end

  defp default_inspect(mod, fun_info) do
    "#Function<#{uniq(fun_info)} in #{Inspect.Atom.inspect(mod)}.#{extract_name(fun_info[:name])}>"
  end

  defp extract_name(name) do
    case :binary.split(atom_to_binary(name), "-", [:global]) do
      ["", name | _] -> name
      _ -> name
    end
  end

  defp uniq(fun_info) do
    integer_to_binary(fun_info[:new_index]) <> "." <> integer_to_binary(fun_info[:uniq])
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
