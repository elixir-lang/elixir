import Kernel, except: [inspect: 1]
import Inspect.Algebra

defprotocol Inspect do
  @moduledoc """
  The `Inspect` protocol is responsible for converting any Elixir
  data structure into an algebra document. This document is then
  formatted, either in pretty printing format or a regular one.

  The `inspect/2` function receives the entity to be inspected
  followed by the inspecting options, represented by the struct
  `Inspect.Opts`.

  Inspection is done using the functions available in `Inspect.Algebra`.

  ## Examples

  Many times, inspecting a structure can be implemented in function
  of existing entities. For example, here is `MapSet`'s `inspect`
  implementation:

      defimpl Inspect, for: MapSet do
        import Inspect.Algebra

        def inspect(dict, opts) do
          concat ["#MapSet<", to_doc(MapSet.to_list(dict), opts), ">"]
        end
      end

  The `concat` function comes from `Inspect.Algebra` and it
  concatenates algebra documents together. In the example above,
  it is concatenating the string `"MapSet<"` (all strings are
  valid algebra documents that keep their formatting when pretty
  printed), the document returned by `Inspect.Algebra.to_doc/2` and the
  other string `">"`.

  Since regular strings are valid entities in an algebra document,
  an implementation of inspect may simply return a string,
  although that will devoid it of any pretty-printing.

  ## Error handling

  In case there is an error while your structure is being inspected,
  Elixir will raise an `ArgumentError` error and will automatically fall back
  to a raw representation for printing the structure.

  You can however access the underlying error by invoking the Inspect
  implementation directly. For example, to test Inspect.MapSet above,
  you can invoke it as:

      Inspect.MapSet.inspect(MapSet.new, %Inspect.Opts{})

  """

  # Handle structs in Any
  @fallback_to_any true

  def inspect(thing, opts)
end

defimpl Inspect, for: Atom do
  require Macro

  def inspect(atom, _opts) do
    inspect(atom)
  end

  def inspect(false),  do: "false"
  def inspect(true),   do: "true"
  def inspect(nil),    do: "nil"
  def inspect(:""),    do: ":\"\""

  def inspect(atom) do
    binary = Atom.to_string(atom)

    cond do
      valid_ref_identifier?(binary) ->
        if only_elixir?(binary) do
          binary
        else
          "Elixir." <> rest = binary
          rest
        end
      valid_atom_identifier?(binary) ->
        ":" <> binary
      atom in [:%{}, :{}, :<<>>, :..., :%] ->
        ":" <> binary
      atom in Macro.binary_ops or atom in Macro.unary_ops ->
        ":" <> binary
      true ->
        <<?:, ?", Inspect.BitString.escape(binary, ?")::binary, ?">>
    end
  end

  defp only_elixir?("Elixir." <> rest), do: only_elixir?(rest)
  defp only_elixir?("Elixir"), do: true
  defp only_elixir?(_), do: false

  # Detect if atom is an atom alias (Elixir.Foo.Bar.Baz)

  defp valid_ref_identifier?("Elixir" <> rest) do
    valid_ref_piece?(rest)
  end

  defp valid_ref_identifier?(_), do: false

  defp valid_ref_piece?(<<?., h, t::binary>>) when h in ?A..?Z do
    valid_ref_piece? valid_identifier?(t)
  end

  defp valid_ref_piece?(<<>>), do: true
  defp valid_ref_piece?(_),    do: false

  # Detect if atom

  defp valid_atom_identifier?(<<h, t::binary>>) when h in ?a..?z or h in ?A..?Z or h == ?_ do
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

  defp valid_identifier?(<<h, t::binary>>)
      when h in ?a..?z
      when h in ?A..?Z
      when h in ?0..?9
      when h == ?_ do
    valid_identifier? t
  end

  defp valid_identifier?(other), do: other
end

defimpl Inspect, for: BitString do
  def inspect(thing, %Inspect.Opts{binaries: bins, base: base} = opts) when is_binary(thing) do
    if base == :decimal and (bins == :as_strings or (bins == :infer and String.printable?(thing))) do
      <<?", escape(thing, ?")::binary, ?">>
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

  defp escape(<<char, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, char>>)
  end
  defp escape(<<?#, ?{, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?#, ?{>>)
  end
  defp escape(<<?\a, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?a>>)
  end
  defp escape(<<?\b, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?b>>)
  end
  defp escape(<<?\d, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?d>>)
  end
  defp escape(<<?\e, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?e>>)
  end
  defp escape(<<?\f, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?f>>)
  end
  defp escape(<<?\n, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?n>>)
  end
  defp escape(<<?\r, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?r>>)
  end
  defp escape(<<?\\, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?\\>>)
  end
  defp escape(<<?\t, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?t>>)
  end
  defp escape(<<?\v, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, ?\\, ?v>>)
  end
  defp escape(<<h::utf8, t::binary>>, char, binary) do
    head = <<h::utf8>>
    if String.printable?(head) do
      escape(t, char, append(head, binary))
    else
      <<byte::8, h::binary>> = head
      t = <<h::binary, t::binary>>
      escape(t, char, <<binary::binary, escape_char(byte)::binary>>)
    end
  end
  defp escape(<<h, t::binary>>, char, binary) do
    escape(t, char, <<binary::binary, escape_char(h)::binary>>)
  end
  defp escape(<<>>, _char, binary), do: binary

  @doc false
  # Also used by Regex
  def escape_char(0) do
    <<?\\, ?0>>
  end

  def escape_char(char) when char < 0x100 do
    <<a::4, b::4>> = <<char::8>>
    <<?\\, ?x, to_hex(a), to_hex(b)>>
  end

  def escape_char(char) when char < 0x10000 do
    <<a::4, b::4, c::4, d::4>> = <<char::16>>
    <<?\\, ?x, ?{, to_hex(a), to_hex(b), to_hex(c), to_hex(d), ?}>>
  end

  def escape_char(char) when char < 0x1000000 do
    <<a::4, b::4, c::4, d::4, e::4, f::4>> = <<char::24>>
    <<?\\, ?x, ?{, to_hex(a), to_hex(b), to_hex(c),
                   to_hex(d), to_hex(e), to_hex(f), ?}>>
  end

  defp to_hex(c) when c in 0..9, do: ?0+c
  defp to_hex(c) when c in 10..15, do: ?A+c-10

  defp append(<<h, t::binary>>, binary), do: append(t, <<binary::binary, h>>)
  defp append(<<>>, binary), do: binary

  ## Bitstrings

  defp inspect_bitstring("", _opts) do
    "<<>>"
  end

  defp inspect_bitstring(bitstring, opts) do
    nest surround("<<", each_bit(bitstring, opts.limit, opts), ">>"), 1
  end

  defp each_bit(_, 0, _) do
    "..."
  end

  defp each_bit(<<>>, _counter, _opts) do
    :doc_nil
  end

  defp each_bit(<<h::8>>, _counter, opts) do
    Inspect.Integer.inspect(h, opts)
  end

  defp each_bit(<<h, t::bitstring>>, counter, opts) do
    glue(concat(Inspect.Integer.inspect(h, opts), ","),
         each_bit(t, decrement(counter), opts))
  end

  defp each_bit(bitstring, _counter, opts) do
    size = bit_size(bitstring)
    <<h::size(size)>> = bitstring
    Inspect.Integer.inspect(h, opts) <> "::size(" <> Integer.to_string(size) <> ")"
  end

  defp decrement(:infinity), do: :infinity
  defp decrement(counter),   do: counter - 1
end

defimpl Inspect, for: List do
  def inspect([], _opts), do: "[]"

  def inspect(thing, %Inspect.Opts{char_lists: lists} = opts) do
    cond do
      lists == :as_char_lists or (lists == :infer and printable?(thing)) ->
        <<?', Inspect.BitString.escape(IO.chardata_to_string(thing), ?')::binary, ?'>>
      keyword?(thing) ->
        surround_many("[", thing, "]", opts, &keyword/2)
      true ->
        surround_many("[", thing, "]", opts, &to_doc/2)
    end
  end

  @doc false
  def keyword({key, value}, opts) do
    concat(
      key_to_binary(key) <> ": ",
      to_doc(value, opts)
    )
  end

  @doc false
  def keyword?([{key, _value} | rest]) when is_atom(key) do
    case Atom.to_char_list(key) do
      'Elixir.' ++ _ -> false
      _ -> keyword?(rest)
    end
  end

  def keyword?([]),     do: true
  def keyword?(_other), do: false

  @doc false
  def printable?([c|cs]) when is_integer(c) and c in 32..126, do: printable?(cs)
  def printable?([?\n|cs]), do: printable?(cs)
  def printable?([?\r|cs]), do: printable?(cs)
  def printable?([?\t|cs]), do: printable?(cs)
  def printable?([?\v|cs]), do: printable?(cs)
  def printable?([?\b|cs]), do: printable?(cs)
  def printable?([?\f|cs]), do: printable?(cs)
  def printable?([?\e|cs]), do: printable?(cs)
  def printable?([?\a|cs]), do: printable?(cs)
  def printable?([]), do: true
  def printable?(_), do: false

  ## Private

  defp key_to_binary(key) do
    case Inspect.Atom.inspect(key) do
      ":" <> right -> right
      other -> other
    end
  end
end

defimpl Inspect, for: Tuple do
  def inspect({}, _opts), do: "{}"

  def inspect(tuple, opts) do
    surround_many("{", Tuple.to_list(tuple), "}", opts, &to_doc/2)
  end
end

defimpl Inspect, for: Map do
  def inspect(map, opts) do
    nest inspect(map, "", opts), 1
  end

  def inspect(map, name, opts) do
    map = :maps.to_list(map)
    surround_many("%" <> name <> "{", map, "}", opts, traverse_fun(map))
  end

  defp traverse_fun(list) do
    if Inspect.List.keyword?(list) do
      &Inspect.List.keyword/2
    else
      &to_map/2
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
  def inspect(thing, %Inspect.Opts{base: base}) do
    Integer.to_string(thing, base_to_value(base))
    |> prepend_prefix(base)
  end

  defp base_to_value(base) do
    case base do
      :binary  -> 2
      :decimal -> 10
      :octal   -> 8
      :hex     -> 16
    end
  end

  defp prepend_prefix(value, :decimal), do: value
  defp prepend_prefix(value, base) do
    prefix = case base do
      :binary -> "0b"
      :octal  -> "0o"
      :hex    -> "0x"
    end
    prefix <> value
  end
end

defimpl Inspect, for: Float do
  def inspect(thing, _opts) do
    IO.iodata_to_binary(:io_lib_format.fwrite_g(thing))
  end
end

defimpl Inspect, for: Regex do
  def inspect(regex, _opts) do
    delim = ?/
    concat ["~r",
            <<delim, escape(regex.source, delim)::binary, delim>>,
            regex.opts]
  end

  defp escape(bin, term),
    do: escape(bin, <<>>, term)

  defp escape(<<?\\, term>> <> rest, buf, term),
    do: escape(rest, buf <> <<?\\, term>>, term)

  defp escape(<<term>> <> rest, buf, term),
    do: escape(rest, buf <> <<?\\, term>>, term)

  # the list of characters is from "String.printable?" impl
  # minus characters treated specially by regex: \s, \d, \b, \e

  defp escape(<<?\n>> <> rest, buf, term),
    do: escape(rest, <<buf::binary, ?\\, ?n>>, term)

  defp escape(<<?\r>> <> rest, buf, term),
    do: escape(rest, <<buf::binary, ?\\, ?r>>, term)

  defp escape(<<?\t>> <> rest, buf, term),
    do: escape(rest, <<buf::binary, ?\\, ?t>>, term)

  defp escape(<<?\v>> <> rest, buf, term),
    do: escape(rest, <<buf::binary, ?\\, ?v>>, term)

  defp escape(<<?\f>> <> rest, buf, term),
    do: escape(rest, <<buf::binary, ?\\, ?f>>, term)

  defp escape(<<?\a>> <> rest, buf, term),
    do: escape(rest, <<buf::binary, ?\\, ?a>>, term)

  defp escape(<<c::utf8>> <> rest, buf, term) do
    charstr = <<c::utf8>>
    if String.printable?(charstr) and not c in [?\d, ?\b, ?\e] do
      escape(rest, buf <> charstr, term)
    else
      escape(rest, buf <> Inspect.BitString.escape_char(c), term)
    end
  end

  defp escape(<<c>> <> rest, buf, term),
    do: escape(rest, <<buf::binary, Inspect.BitString.escape_char(c)>>, term)

  defp escape(<<>>, buf, _), do: buf
end

defimpl Inspect, for: Function do
  def inspect(function, _opts) do
    fun_info = :erlang.fun_info(function)
    mod = fun_info[:module]

    if fun_info[:type] == :external and fun_info[:env] == [] do
      "&#{Inspect.Atom.inspect(mod)}.#{fun_info[:name]}/#{fun_info[:arity]}"
    else
      case Atom.to_char_list(mod) do
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
    name = Atom.to_string(name)
    case :binary.split(name, "-", [:global]) do
      ["", name | _] -> "." <> name
      _ -> "." <> name
    end
  end

  defp uniq(fun_info) do
    Integer.to_string(fun_info[:new_index]) <> "." <>
      Integer.to_string(fun_info[:uniq])
  end
end

defimpl Inspect, for: PID do
  def inspect(pid, _opts) do
    "#PID" <> IO.iodata_to_binary(:erlang.pid_to_list(pid))
  end
end

defimpl Inspect, for: Port do
  def inspect(port, _opts) do
    IO.iodata_to_binary :erlang.port_to_list(port)
  end
end

defimpl Inspect, for: Reference do
  def inspect(ref, _opts) do
    '#Ref' ++ rest = :erlang.ref_to_list(ref)
    "#Reference" <> IO.iodata_to_binary(rest)
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
          pruned = :maps.remove(:__exception__, :maps.remove(:__struct__, map))
          Inspect.Map.inspect(pruned, Inspect.Atom.inspect(struct, opts), opts)
        else
          Inspect.Map.inspect(map, opts)
        end
    end
  end
end
