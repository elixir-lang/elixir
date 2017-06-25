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

  The `concat/1` function comes from `Inspect.Algebra` and it
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

  def inspect(term, opts)
end

defimpl Inspect, for: Atom do
  require Macro

  def inspect(atom, opts) do
    color(inspect(atom), color_key(atom), opts)
  end

  defp color_key(atom) when is_boolean(atom), do: :boolean
  defp color_key(nil), do: :nil
  defp color_key(_), do: :atom

  def inspect(atom) when is_nil(atom) or is_boolean(atom) do
    Atom.to_string(atom)
  end

  def inspect(atom) when is_atom(atom) do
    binary = Atom.to_string(atom)

    case Macro.classify_identifier(atom) do
      :alias ->
        case binary do
          binary when binary in ["Elixir", "Elixir.Elixir"] ->
            binary
          "Elixir.Elixir." <> _rest ->
            binary
          "Elixir." <> rest ->
            rest
        end
      type when type in [:callable, :not_callable] ->
        ":" <> binary
      :other ->
        {escaped, _} = Inspect.BitString.escape(binary, ?")
        IO.iodata_to_binary [?:, ?", escaped, ?"]
    end
  end
end

defimpl Inspect, for: BitString do
  def inspect(term, opts) when is_binary(term) do
    %Inspect.Opts{binaries: bins, base: base, printable_limit: printable_limit} = opts

    if base == :decimal and
       (bins == :as_strings or (bins == :infer and String.printable?(term, printable_limit))) do
      inspected =
        case escape(term, ?", printable_limit) do
          {escaped, ""} -> [?", escaped, ?"]
          {escaped, _} -> [?", escaped, ?", " <> ..."]
        end
      color(IO.iodata_to_binary(inspected), :string, opts)
    else
      inspect_bitstring(term, opts)
    end
  end

  def inspect(term, opts) do
    inspect_bitstring(term, opts)
  end

  ## Escaping

  @doc false
  def escape(other, char) do
    escape(other, char, :infinity, [])
  end

  @doc false
  def escape(other, char, count) do
    escape(other, char, count, [])
  end

  defp escape(<<_, _::binary>> = binary, _char, 0, acc) do
    {acc, binary}
  end
  defp escape(<<char, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | [?\\, char]])
  end
  defp escape(<<?#, ?{, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\\#{'])
  end
  defp escape(<<?\a, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\a'])
  end
  defp escape(<<?\b, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\b'])
  end
  defp escape(<<?\d, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\d'])
  end
  defp escape(<<?\e, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\e'])
  end
  defp escape(<<?\f, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\f'])
  end
  defp escape(<<?\n, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\n'])
  end
  defp escape(<<?\r, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\r'])
  end
  defp escape(<<?\\, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\\\'])
  end
  defp escape(<<?\t, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\t'])
  end
  defp escape(<<?\v, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | '\\v'])
  end
  defp escape(<<h::utf8, t::binary>>, char, count, acc)
       when h in 0x20..0x7E
       when h in 0xA0..0xD7FF
       when h in 0xE000..0xFFFD
       when h in 0x10000..0x10FFFF do
    escape(t, char, decrement(count), [acc | <<h::utf8>>])
  end
  defp escape(<<h, t::binary>>, char, count, acc) do
    escape(t, char, decrement(count), [acc | escape_char(h)])
  end
  defp escape(<<>>, _char, _count, acc) do
    {acc, <<>>}
  end

  @doc false
  # Also used by Regex
  def escape_char(0) do
    '\\0'
  end

  def escape_char(char) when char < 0x100 do
    <<a::4, b::4>> = <<char::8>>
    ['\\x', to_hex(a), to_hex(b)]
  end

  def escape_char(char) when char < 0x10000 do
    <<a::4, b::4, c::4, d::4>> = <<char::16>>
    ['\\x{', to_hex(a), to_hex(b), to_hex(c), to_hex(d), ?}]
  end

  def escape_char(char) when char < 0x1000000 do
    <<a::4, b::4, c::4, d::4, e::4, f::4>> = <<char::24>>
    ['\\x{', to_hex(a), to_hex(b), to_hex(c),
             to_hex(d), to_hex(e), to_hex(f), ?}]
  end

  defp to_hex(c) when c in 0..9, do: ?0 + c
  defp to_hex(c) when c in 10..15, do: ?A + c - 10

  ## Bitstrings

  defp inspect_bitstring("", opts) do
    color("<<>>", :binary, opts)
  end

  defp inspect_bitstring(bitstring, opts) do
    left = color("<<", :binary, opts)
    right = color(">>", :binary, opts)
    nest surround(left, each_bit(bitstring, opts.limit, opts), right), 1
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

  @compile {:inline, decrement: 1}
  defp decrement(:infinity), do: :infinity
  defp decrement(counter),   do: counter - 1
end

defimpl Inspect, for: List do
  def inspect([], opts) do
    color("[]", :list, opts)
  end

  # TODO: Remove :char_list and :as_char_lists handling in 2.0
  def inspect(term, opts) do
    %Inspect.Opts{charlists: lists, char_lists: lists_deprecated, printable_limit: printable_limit} = opts
    lists =
      if lists == :infer and lists_deprecated != :infer do
        case lists_deprecated do
          :as_char_lists ->
            IO.warn "the :char_lists inspect option and its :as_char_lists " <>
              "value are deprecated, use the :charlists option and its " <>
              ":as_charlists value instead"
            :as_charlists
          _ ->
            IO.warn "the :char_lists inspect option is deprecated, use :charlists instead"
            lists_deprecated
        end
      else
        lists
      end

    open = color("[", :list, opts)
    sep = color(",", :list, opts)
    close = color("]", :list, opts)

    cond do
      lists == :as_charlists or (lists == :infer and printable?(term, printable_limit)) ->
        inspected =
          case Inspect.BitString.escape(IO.chardata_to_string(term), ?', printable_limit) do
            {escaped, ""} -> [?', escaped, ?']
            {escaped, _} -> [?', escaped, ?', " ++ ..."]
          end
        IO.iodata_to_binary inspected
      keyword?(term) ->
        surround_many(open, term, close, opts, &keyword/2, sep)
      true ->
        surround_many(open, term, close, opts, &to_doc/2, sep)
    end
  end

  @doc false
  def keyword({key, value}, opts) do
    key = color(key_to_binary(key) <> ": ", :atom, opts)
    concat(key, to_doc(value, opts))
  end

  @doc false
  def keyword?([{key, _value} | rest]) when is_atom(key) do
    case Atom.to_charlist(key) do
      'Elixir.' ++ _ -> false
      _ -> keyword?(rest)
    end
  end

  def keyword?([]),     do: true
  def keyword?(_other), do: false

  @doc false
  def printable?(list), do: printable?(list, :infinity)

  @doc false
  def printable?(_, 0), do: true
  def printable?([char | rest], counter) when char in 32..126, do: printable?(rest, decrement(counter))
  def printable?([?\n | rest], counter), do: printable?(rest, decrement(counter))
  def printable?([?\r | rest], counter), do: printable?(rest, decrement(counter))
  def printable?([?\t | rest], counter), do: printable?(rest, decrement(counter))
  def printable?([?\v | rest], counter), do: printable?(rest, decrement(counter))
  def printable?([?\b | rest], counter), do: printable?(rest, decrement(counter))
  def printable?([?\f | rest], counter), do: printable?(rest, decrement(counter))
  def printable?([?\e | rest], counter), do: printable?(rest, decrement(counter))
  def printable?([?\a | rest], counter), do: printable?(rest, decrement(counter))
  def printable?([], _counter), do: true
  def printable?(_, _counter), do: false

  @compile {:inline, decrement: 1}
  defp decrement(:infinity), do: :infinity
  defp decrement(counter),   do: counter - 1

  ## Private

  defp key_to_binary(key) do
    case Inspect.Atom.inspect(key) do
      ":" <> right -> right
      other -> other
    end
  end
end

defimpl Inspect, for: Tuple do
  def inspect(tuple, opts) do
    open = color("{", :tuple, opts)
    sep = color(",", :tuple, opts)
    close = color("}", :tuple, opts)
    surround_many(open, Tuple.to_list(tuple), close, opts, &to_doc/2, sep)
  end
end

defimpl Inspect, for: Map do
  def inspect(map, opts) do
    nest inspect(map, "", opts), 1
  end

  def inspect(map, name, opts) do
    map = :maps.to_list(map)
    open = color("%" <> name <> "{", :map, opts)
    sep = color(",", :map, opts)
    close = color("}", :map, opts)
    surround_many(open, map, close, opts, traverse_fun(map), sep)
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
  def inspect(term, %Inspect.Opts{base: base} = opts) do
    inspected = Integer.to_string(term, base_to_value(base)) |> prepend_prefix(base)
    color(inspected, :number, opts)
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
  def inspect(term, opts) do
    inspected = IO.iodata_to_binary(:io_lib_format.fwrite_g(term))
    color(inspected, :number, opts)
  end
end

defimpl Inspect, for: Regex do
  def inspect(regex, opts) do
    source = IO.iodata_to_binary(['~r/', escape(regex.source, ?/), ?/, regex.opts])
    color(source, :regex, opts)
  end

  defp escape(bin, term),
    do: escape(bin, [], term)

  defp escape(<<term, rest::binary>>, buf, term),
    do: escape(rest, [buf | [?\\, term]], term)

  # The list of characters is from 'String.printable?' implementation
  # minus characters treated specially by regex: \s, \d, \b, \e

  defp escape(<<?\n, rest::binary>>, buf, term),
    do: escape(rest, [buf | '\\n'], term)

  defp escape(<<?\r, rest::binary>>, buf, term),
    do: escape(rest, [buf | '\\r'], term)

  defp escape(<<?\t, rest::binary>>, buf, term),
    do: escape(rest, [buf | '\\t'], term)

  defp escape(<<?\v, rest::binary>>, buf, term),
    do: escape(rest, [buf | '\\v'], term)

  defp escape(<<?\f, rest::binary>>, buf, term),
    do: escape(rest, [buf | '\\f'], term)

  defp escape(<<?\a, rest::binary>>, buf, term),
    do: escape(rest, [buf | '\\a'], term)

  defp escape(<<char::utf8, rest::binary>>, buf, term)
       when char in 0x20..0x7E
       when char in 0xA0..0xD7FF
       when char in 0xE000..0xFFFD
       when char in 0x10000..0x10FFFF,
    do: escape(rest, [buf | <<char::utf8>>], term)

  defp escape(<<char, rest::binary>>, buf, term),
    do: escape(rest, [buf | Inspect.BitString.escape_char(char)], term)

  defp escape(<<>>, buf, _), do: buf
end

defimpl Inspect, for: Function do
  def inspect(function, _opts) do
    fun_info = :erlang.fun_info(function)
    mod = fun_info[:module]
    name = fun_info[:name]

    if fun_info[:type] == :external and fun_info[:env] == [] do
      "&#{Inspect.Atom.inspect(mod)}.#{escape_name(name)}/#{fun_info[:arity]}"
    else
      case Atom.to_charlist(mod) do
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

  def escape_name(atom) when is_atom(atom) do
    string = Atom.to_string(atom)

    case Macro.classify_identifier(atom) do
      :callable ->
        string
      type when type in [:not_callable, :alias] ->
        "\"" <> string <> "\""
      :other ->
        {escaped, _} = Inspect.BitString.escape(string, ?")
        IO.iodata_to_binary [?", escaped, ?"]
    end
  end

  # Example of this format: -NAME/ARITY-fun-COUNT-
  def extract_anonymous_fun_parent(atom) when is_atom(atom) do
    extract_anonymous_fun_parent(Atom.to_string(atom))
  end

  def extract_anonymous_fun_parent("-" <> rest) do
    [trailing | reversed] =
      rest
      |> String.split("/")
      |> Enum.reverse()

    case String.split(trailing, "-") do
      [arity, _inner, _count, ""] ->
        {reversed |> Enum.reverse |> Enum.join("/") |> String.to_atom(), arity}
      _other ->
        :error
    end
  end

  def extract_anonymous_fun_parent(other) when is_binary(other), do: :error

  defp default_inspect(mod, fun_info) do
    "#Function<#{uniq(fun_info)}/#{fun_info[:arity]} in " <>
      "#{Inspect.Atom.inspect(mod)}#{extract_name(fun_info[:name])}>"
  end

  defp extract_name([]) do
    ""
  end

  defp extract_name(name) do
    case extract_anonymous_fun_parent(name) do
      {name, arity} ->
        "." <> escape_name(name) <> "/" <> arity
      :error ->
        "." <> escape_name(name)
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
    IO.iodata_to_binary(:erlang.port_to_list(port))
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
          colorless_opts = %{opts | syntax_colors: []}
          Inspect.Map.inspect(pruned, Inspect.Atom.inspect(struct, colorless_opts), opts)
        else
          Inspect.Map.inspect(map, opts)
        end
    end
  end
end
