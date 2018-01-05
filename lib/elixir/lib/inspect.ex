import Kernel, except: [inspect: 1]
import Inspect.Algebra

alias Code.Identifier

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
          concat(["#MapSet<", to_doc(MapSet.to_list(dict), opts), ">"])
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

      Inspect.MapSet.inspect(MapSet.new(), %Inspect.Opts{})

  """

  # Handle structs in Any
  @fallback_to_any true

  def inspect(term, opts)
end

defimpl Inspect, for: Atom do
  require Macro

  def inspect(atom, opts) do
    color(Identifier.inspect_as_atom(atom), color_key(atom), opts)
  end

  defp color_key(atom) when is_boolean(atom), do: :boolean
  defp color_key(nil), do: nil
  defp color_key(_), do: :atom
end

defimpl Inspect, for: BitString do
  def inspect(term, opts) when is_binary(term) do
    %Inspect.Opts{binaries: bins, base: base, printable_limit: printable_limit} = opts

    if base == :decimal and
         (bins == :as_strings or (bins == :infer and String.printable?(term, printable_limit))) do
      inspected =
        case Identifier.escape(term, ?", printable_limit) do
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

  defp inspect_bitstring("", opts) do
    color("<<>>", :binary, opts)
  end

  defp inspect_bitstring(bitstring, opts) do
    left = color("<<", :binary, opts)
    right = color(">>", :binary, opts)
    inner = each_bit(bitstring, opts.limit, opts)
    group(concat(concat(left, nest(inner, 2)), right))
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
    flex_glue(
      concat(Inspect.Integer.inspect(h, opts), ","),
      each_bit(t, decrement(counter), opts)
    )
  end

  defp each_bit(bitstring, _counter, opts) do
    size = bit_size(bitstring)
    <<h::size(size)>> = bitstring
    Inspect.Integer.inspect(h, opts) <> "::size(" <> Integer.to_string(size) <> ")"
  end

  @compile {:inline, decrement: 1}
  defp decrement(:infinity), do: :infinity
  defp decrement(counter), do: counter - 1
end

defimpl Inspect, for: List do
  def inspect([], opts) do
    color("[]", :list, opts)
  end

  # TODO: Remove :char_list and :as_char_lists handling in 2.0
  def inspect(term, opts) do
    %Inspect.Opts{
      charlists: lists,
      char_lists: lists_deprecated,
      printable_limit: printable_limit
    } = opts

    lists =
      if lists == :infer and lists_deprecated != :infer do
        case lists_deprecated do
          :as_char_lists ->
            IO.warn(
              "the :char_lists inspect option and its :as_char_lists " <>
                "value are deprecated, use the :charlists option and its " <>
                ":as_charlists value instead"
            )

            :as_charlists

          _ ->
            IO.warn("the :char_lists inspect option is deprecated, use :charlists instead")
            lists_deprecated
        end
      else
        lists
      end

    open = color("[", :list, opts)
    sep = color(",", :list, opts)
    close = color("]", :list, opts)

    cond do
      lists == :as_charlists or (lists == :infer and List.ascii_printable?(term, printable_limit)) ->
        inspected =
          case Identifier.escape(IO.chardata_to_string(term), ?', printable_limit) do
            {escaped, ""} -> [?', escaped, ?']
            {escaped, _} -> [?', escaped, ?', " ++ ..."]
          end

        IO.iodata_to_binary(inspected)

      keyword?(term) ->
        container_doc(open, term, close, opts, &keyword/2, separator: sep, break: :strict)

      true ->
        container_doc(open, term, close, opts, &to_doc/2, separator: sep)
    end
  end

  @doc false
  def keyword({key, value}, opts) do
    key = color(Identifier.inspect_as_key(key), :atom, opts)
    concat(key, concat(" ", to_doc(value, opts)))
  end

  @doc false
  def keyword?([{key, _value} | rest]) when is_atom(key) do
    case Atom.to_charlist(key) do
      'Elixir.' ++ _ -> false
      _ -> keyword?(rest)
    end
  end

  def keyword?([]), do: true
  def keyword?(_other), do: false
end

defimpl Inspect, for: Tuple do
  def inspect(tuple, opts) do
    open = color("{", :tuple, opts)
    sep = color(",", :tuple, opts)
    close = color("}", :tuple, opts)
    container_opts = [separator: sep, break: :flex]
    container_doc(open, Tuple.to_list(tuple), close, opts, &to_doc/2, container_opts)
  end
end

defimpl Inspect, for: Map do
  def inspect(map, opts) do
    inspect(map, "", opts)
  end

  def inspect(map, name, opts) do
    map = :maps.to_list(map)
    open = color("%" <> name <> "{", :map, opts)
    sep = color(",", :map, opts)
    close = color("}", :map, opts)
    container_doc(open, map, close, opts, traverse_fun(map, opts), separator: sep, break: :strict)
  end

  defp traverse_fun(list, opts) do
    if Inspect.List.keyword?(list) do
      &Inspect.List.keyword/2
    else
      sep = color(" => ", :map, opts)
      &to_map(&1, &2, sep)
    end
  end

  defp to_map({key, value}, opts, sep) do
    concat(concat(to_doc(key, opts), sep), to_doc(value, opts))
  end
end

defimpl Inspect, for: Integer do
  def inspect(term, %Inspect.Opts{base: base} = opts) do
    inspected = Integer.to_string(term, base_to_value(base)) |> prepend_prefix(base)
    color(inspected, :number, opts)
  end

  defp base_to_value(base) do
    case base do
      :binary -> 2
      :decimal -> 10
      :octal -> 8
      :hex -> 16
    end
  end

  defp prepend_prefix(value, :decimal), do: value

  defp prepend_prefix(<<?-, value::binary>>, base) do
    "-" <> prepend_prefix(value, base)
  end

  defp prepend_prefix(value, base) do
    prefix =
      case base do
        :binary -> "0b"
        :octal -> "0o"
        :hex -> "0x"
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
    {escaped, _} = Identifier.escape(regex.source, ?/, :infinity, &escape_map/1)
    source = IO.iodata_to_binary(['~r/', escaped, ?/, regex.opts])
    color(source, :regex, opts)
  end

  defp escape_map(?\a), do: '\\a'
  defp escape_map(?\f), do: '\\f'
  defp escape_map(?\n), do: '\\n'
  defp escape_map(?\r), do: '\\r'
  defp escape_map(?\t), do: '\\t'
  defp escape_map(?\v), do: '\\v'
  defp escape_map(_), do: false
end

defimpl Inspect, for: Function do
  def inspect(function, _opts) do
    fun_info = :erlang.fun_info(function)
    mod = fun_info[:module]
    name = fun_info[:name]

    if fun_info[:type] == :external and fun_info[:env] == [] do
      inspected_as_atom = Identifier.inspect_as_atom(mod)
      inspected_as_function = Identifier.inspect_as_function(name)
      "&#{inspected_as_atom}.#{inspected_as_function}/#{fun_info[:arity]}"
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

  defp default_inspect(mod, fun_info) do
    inspected_as_atom = Identifier.inspect_as_atom(mod)
    extracted_name = extract_name(fun_info[:name])
    "#Function<#{uniq(fun_info)}/#{fun_info[:arity]} in #{inspected_as_atom}#{extracted_name}>"
  end

  defp extract_name([]) do
    ""
  end

  defp extract_name(name) do
    case Identifier.extract_anonymous_fun_parent(name) do
      {name, arity} ->
        "." <> Identifier.inspect_as_function(name) <> "/" <> arity

      :error ->
        "." <> Identifier.inspect_as_function(name)
    end
  end

  defp uniq(fun_info) do
    Integer.to_string(fun_info[:new_index]) <> "." <> Integer.to_string(fun_info[:uniq])
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
  def inspect(%module{} = struct, opts) do
    try do
      module.__struct__
    rescue
      _ -> Inspect.Map.inspect(struct, opts)
    else
      dunder ->
        if :maps.keys(dunder) == :maps.keys(struct) do
          pruned = :maps.remove(:__exception__, :maps.remove(:__struct__, struct))
          colorless_opts = %{opts | syntax_colors: []}
          Inspect.Map.inspect(pruned, Inspect.Atom.inspect(module, colorless_opts), opts)
        else
          Inspect.Map.inspect(struct, opts)
        end
    end
  end
end
