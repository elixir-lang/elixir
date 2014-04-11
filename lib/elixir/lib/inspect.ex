import Kernel, except: [inspect: 1]
import Inspect.Algebra

defrecord Inspect.Opts,
  records: true,
  structs: true,
  binaries: :infer,
  char_lists: :infer,
  limit: 50,
  pretty: false,
  width: 80

defprotocol Inspect do
  @moduledoc """
  The `Inspect` protocol is responsible for converting any Elixir
  data structure into an algebra document. This document is then
  formatted, either in pretty printing format or a regular one.

  The `inspect/2` function receives the entity to be inspected
  followed by the inspecting options, represented by the record
  `Inspect.Opts`.

  Inspection is done using the functions available in `Inspect.Algebra`.

  ## Examples

  Many times, inspecting a structure can be implemented in function
  of existing entities. For example, here is `HashSet`'s `inspect`
  implementation:

      defimpl Inspect, for: HashSet do
        import Inspect.Algebra

        def inspect(dict, opts) do
          concat ["#HashSet<", to_doc(HashSet.to_list(dict), opts), ">"]
        end
      end

  The `concat` function comes from `Inspect.Algebra` and it
  concatenates algebra documents together. In the example above,
  it is concatenating the string `"HashSet<"` (all strings are
  valid algebra documents that keep their formatting when pretty
  printed), the document returned by `Inspect.Algebra.to_doc/2` and the
  other string `">"`.

  Since regular strings are valid entities in an algebra document,
  an implementation of inspect may simply return a string,
  although that will devoid it of any pretty-printing.

  ## Error handling

  In case there is an error while your structure is being inspected,
  Elixir will automatically fall back to tuple inspection for records.
  You can however access the underlying error by invoking the Inspect
  implementation directly. For example, to test Inspect.HashSet above,
  you just need to do:

      Inspect.HashSet.inspect(HashSet.new, Inspect.Opts.new)

  """

  # Handle structs in Any
  @fallback_to_any true

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
      atom in [:%{}, :{}, :<<>>, :..., :[]] ->
        ":" <> binary
      atom in Macro.binary_ops or atom in Macro.unary_ops ->
        ":" <> binary
      true ->
        << ?:, ?", Inspect.BitString.escape(binary, ?") :: binary, ?" >>
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
    valid_atom_piece?(t)
  end

  defp valid_atom_identifier?(_), do: false

  defp valid_atom_piece?(t) do
    case valid_identifier?(t) do
      <<>>              -> true
      <<??>>            -> true
      <<?!>>            -> true
      <<?@, t::binary>> -> valid_atom_piece?(t)
      _                 -> false
    end
  end

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
  @doc ~S"""
  Represents a string as itself escaping all necessary
  characters. Binaries that contain non-printable characters
  are printed using the bitstring syntax.

  ## Examples

      iex> inspect("bar")
      "\"bar\""

      iex> inspect("f\"oo")
      "\"f\\\"oo\""

      iex> inspect(<<0,1,2>>)
      "<<0, 1, 2>>"

  """
  def inspect(thing, Inspect.Opts[binaries: bins] = opts) when is_binary(thing) do
    if bins == :as_strings or (bins == :infer and String.printable?(thing)) do
      << ?", escape(thing, ?") :: binary, ?" >>
    else
      inspect_bitstring(thing, opts)
    end
  end

  def inspect(thing, opts) do
    inspect_bitstring(thing, opts)
  end

  ## Escaping

  @doc false
  def escape(other, char) do
    escape(other, char, <<>>)
  end

  defp escape(<< char, t :: binary >>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, char >>)
  end
  defp escape(<<?#, ?{, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?#, ?{ >>)
  end
  defp escape(<<?\a, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?a >>)
  end
  defp escape(<<?\b, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?b >>)
  end
  defp escape(<<?\d, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?d >>)
  end
  defp escape(<<?\e, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?e >>)
  end
  defp escape(<<?\f, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?f >>)
  end
  defp escape(<<?\n, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?n >>)
  end
  defp escape(<<?\r, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?r >>)
  end
  defp escape(<<?\\, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?\\ >>)
  end
  defp escape(<<?\t, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?t >>)
  end
  defp escape(<<?\v, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?v >>)
  end
  defp escape(<<h :: utf8, t :: binary>>, char, binary) do
    head = << h :: utf8 >>
    if String.printable?(head) do
      escape(t, char, append(head, binary))
    else
      << byte :: size(8), h :: binary >> = head
      t = << h :: binary, t :: binary >>
      escape(t, char, << binary :: binary, octify(byte) :: binary >>)
    end
  end
  defp escape(<<h, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, octify(h) :: binary >>)
  end
  defp escape(<<>>, _char, binary), do: binary

  defp octify(byte) do
    << hi :: size(2), mi :: size(3), lo :: size(3) >> = << byte >>
    << ?\\, ?0 + hi, ?0 + mi, ?0 + lo >>
  end

  defp append(<<h, t :: binary>>, binary), do: append(t, << binary :: binary, h >>)
  defp append(<<>>, binary), do: binary

  ## Bitstrings

  defp inspect_bitstring(bitstring, Inspect.Opts[] = opts) do
    each_bit(bitstring, opts.limit, "<<") <> ">>"
  end

  defp each_bit(_, 0, acc) do
    acc <> "..."
  end

  defp each_bit(<<h, t :: bitstring>>, counter, acc) when t != <<>> do
    each_bit(t, decrement(counter), acc <> integer_to_binary(h) <> ", ")
  end

  defp each_bit(<<h :: size(8)>>, _counter, acc) do
    acc <> integer_to_binary(h)
  end

  defp each_bit(<<>>, _counter, acc) do
    acc
  end

  defp each_bit(bitstring, _counter, acc) do
    size = bit_size(bitstring)
    <<h :: size(size)>> = bitstring
    acc <> integer_to_binary(h) <> "::size(" <> integer_to_binary(size) <> ")"
  end

  defp decrement(:infinity), do: :infinity
  defp decrement(counter),   do: counter - 1
end

defimpl Inspect, for: List do
  @doc ~S"""
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

  def inspect(thing, Inspect.Opts[char_lists: lists] = opts) do
    cond do
      lists == :as_char_lists or (lists == :infer and :io_lib.printable_list(thing)) ->
        << ?', Inspect.BitString.escape(String.from_char_list!(thing), ?') :: binary, ?' >>
      keyword?(thing) ->
        surround_many("[", thing, "]", opts.limit, &keyword(&1, opts))
      true ->
        surround_many("[", thing, "]", opts.limit, &to_doc(&1, opts))
    end
  end

  def keyword({key, value}, opts) do
    concat(
      key_to_binary(key) <> ": ",
      to_doc(value, opts)
    )
  end

  defp key_to_binary(key) do
    case Inspect.Atom.inspect(key) do
      ":" <> right -> right
      other -> other
    end
  end

  def keyword?([{ key, _value } | rest]) when is_atom(key) do
    case atom_to_list(key) do
      'Elixir.' ++ _ -> false
      _ -> keyword?(rest)
    end
  end

  def keyword?([]),     do: true
  def keyword?(_other), do: false
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

  def inspect(tuple, Inspect.Opts[] = opts) do
    if opts.records do
      record_inspect(tuple, opts)
    else
      surround_many("{", tuple_to_list(tuple), "}", opts.limit, &to_doc(&1, opts))
    end
  end

  ## Helpers

  defp record_inspect(record, Inspect.Opts[] = opts) do
    [name|tail] = tuple_to_list(record)

    if is_atom(name) && (fields = record_fields(name)) && (length(fields) == size(record) - 1) do
      surround_record(name, fields, tail, opts)
    else
      surround_many("{", [name|tail], "}", opts.limit, &to_doc(&1, opts))
    end
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

  defp surround_record(name, fields, tail, Inspect.Opts[] = opts) do
    concat(
      Inspect.Atom.inspect(name, opts),
      surround_many("[", zip_fields(fields, tail), "]", opts.limit, &keyword(&1, opts))
    )
  end

  defp zip_fields([{ key, _ }|tk], [value|tv]) do
    case atom_to_binary(key) do
      "_" <> _ -> zip_fields(tk, tv)
      key -> [{ key, value }|zip_fields(tk, tv)]
    end
  end

  defp zip_fields([], []) do
    []
  end

  defp keyword({ k, v }, opts) do
    concat(k <> ": ", to_doc(v, opts))
  end
end

defimpl Inspect, for: Map do
  def inspect(map, opts) do
    inspect(map, "", opts)
  end

  def inspect(map, name, opts) do
    map = :maps.to_list(map)
    surround_many("%" <> name <> "{", map, "}", opts.limit, traverse_fun(map, opts))
  end

  defp traverse_fun(list, opts) do
    if Inspect.List.keyword?(list) do
      &Inspect.List.keyword(&1, opts)
    else
      &to_map(&1, opts)
    end
  end

  defp to_map({key, value}, opts) do
    concat(
      concat(to_doc(key, opts), " => "),
      to_doc(value, opts)
    )
  end
end

defimpl Inspect, for: Integer do
  @doc """
  Represents the integer as a string.

  ## Examples

      iex> inspect(1)
      "1"

  """
  def inspect(thing, _opts) do
    integer_to_binary(thing)
  end
end

defimpl Inspect, for: Float do
  @doc """
  Floats are represented using the shortened, correctly rounded string
  that converts to float when read back with `binary_to_float/1`. This
  is done via the Erlang implementation of _Printing Floating-Point
  Numbers Quickly and Accurately_ in Proceedings of the SIGPLAN '96
  Conference on Programming Language Design and Implementation.

  ## Examples

      iex> inspect(1.0)
      "1.0"

  """
  def inspect(thing, _opts) do
    iolist_to_binary(:io_lib_format.fwrite_g(thing))
  end
end

defimpl Inspect, for: Regex do
  @doc ~S"""
  Represents the Regex using the `~r""` syntax.

  ## Examples

      iex> inspect(~r/foo/m)
      "~r\"foo\"m"

  """
  def inspect(regex, opts) when size(regex) == 4 do
    concat ["~r", to_doc(Regex.source(regex), opts), Regex.opts(regex)]
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
    "#Function<#{uniq(fun_info)}/#{fun_info[:arity]} in " <>
      "#{Inspect.Atom.inspect(mod)}#{extract_name(fun_info[:name])}>"
  end

  defp extract_name([]) do
    ""
  end

  defp extract_name(name) do
    case :binary.split(atom_to_binary(name), "-", [:global]) do
      ["", name | _] -> "." <> name
      _ -> "." <> name
    end
  end

  defp uniq(fun_info) do
    integer_to_binary(fun_info[:new_index]) <> "." <>
      integer_to_binary(fun_info[:uniq])
  end
end

defimpl Inspect, for: PID do
  def inspect(pid, _opts) do
    "#PID" <> iolist_to_binary(:erlang.pid_to_list(pid))
  end
end

defimpl Inspect, for: Port do
  def inspect(port, _opts) do
    iolist_to_binary :erlang.port_to_list(port)
  end
end

defimpl Inspect, for: Reference do
  def inspect(ref, _opts) do
    '#Ref' ++ rest = :erlang.ref_to_list(ref)
    "#Reference" <> iolist_to_binary(rest)
  end
end

defimpl Inspect, for: Any do
  def inspect(%{__struct__: struct} = map, opts) do
    try do
      struct.__struct__
    rescue
      _ -> Inspect.Map.inspect(map, opts)
    else
      dunder ->
        if :maps.keys(dunder) == :maps.keys(map) do
          Inspect.Map.inspect(:maps.remove(:__struct__, map),
            Inspect.Atom.inspect(struct, opts), opts)
        else
          Inspect.Map.inspect(map, opts)
        end
    end
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
