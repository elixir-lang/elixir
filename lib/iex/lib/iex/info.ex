defprotocol IEx.Info do
  @fallback_to_any true

  @spec info(term()) :: [{info_name :: String.Chars.t(), info :: String.t()}]
  def info(term)
end

defimpl IEx.Info, for: Tuple do
  def info(_tuple) do
    [
      {"Data type", "Tuple"},
      {"Reference modules", "Tuple"}
    ]
  end
end

defimpl IEx.Info, for: Atom do
  def info(atom) do
    specific_info =
      cond do
        module?(atom) ->
          info_module(atom)

        match?("Elixir." <> _, Atom.to_string(atom)) ->
          info_module_like_atom(atom)

        true ->
          info_atom(atom)
      end

    description =
      if atom == IEx.dont_display_result() do
        description = """
        This atom is returned by IEx when a function that should not print its
        return value on screen is executed.
        """

        [{"Description", description}]
      else
        []
      end

    [{"Data type", "Atom"}] ++ description ++ specific_info
  end

  defp module?(atom) do
    case :code.get_object_code(atom) do
      :error ->
        Code.ensure_loaded?(atom)

      {^atom, beam, _path} ->
        info = :beam_lib.info(beam)
        Keyword.fetch(info, :module) == {:ok, atom}
    end
  end

  defp info_module(mod) do
    extra =
      case Code.fetch_docs(mod) do
        {:docs_v1, _, _, _, %{}, _, _} -> "Use h(#{inspect(mod)}) to access its documentation.\n"
        _ -> ""
      end

    mod_info = mod.module_info()

    generic_info = [
      {"Module bytecode", module_object_file(mod)},
      {"Source", module_source_file(mod_info)},
      {"Version", module_version(mod_info)},
      {"Compile options", module_compile_options(mod_info)},
      {"Description", "#{extra}Call #{inspect(mod)}.module_info() to access metadata."}
    ]

    final_info = [
      {"Raw representation", ":" <> inspect(Atom.to_string(mod))},
      {"Reference modules", "Module, Atom"}
    ]

    generic_info ++ protocol_info(mod) ++ final_info
  end

  defp protocol_info(mod) do
    if function_exported?(mod, :__protocol__, 1) do
      impls =
        mod
        |> Protocol.extract_impls(:code.get_path())
        |> Enum.map_join(", ", &inspect/1)

      [{"Protocol", "This module is a protocol. These data structures implement it:\n  #{impls}"}]
    else
      []
    end
  end

  defp info_module_like_atom(atom) do
    [
      {"Raw representation", ":" <> inspect(Atom.to_string(atom))},
      {"Reference modules", "Atom"}
    ]
  end

  defp info_atom(_atom) do
    [{"Reference modules", "Atom"}]
  end

  defp module_object_file(mod) do
    default_or_apply(:code.which(mod), fn
      [_ | _] = path -> Path.relative_to_cwd(path)
      atom -> inspect(atom)
    end)
  end

  defp module_version(mod_info) do
    default_or_apply(mod_info[:attributes][:vsn], &inspect/1)
  end

  defp module_source_file(mod_info) do
    default_or_apply(mod_info[:compile][:source], &Path.relative_to_cwd/1)
  end

  defp module_compile_options(mod_info) do
    default_or_apply(mod_info[:compile][:options], &inspect/1)
  end

  defp default_or_apply(nil, _), do: "no value found"
  defp default_or_apply(data, fun), do: fun.(data)
end

defimpl IEx.Info, for: List do
  def info(list) do
    specific_info =
      cond do
        list == [] -> info_list(list)
        List.ascii_printable?(list) -> info_printable_charlist(list)
        Keyword.keyword?(list) -> info_kw_list(list)
        List.improper?(list) -> info_improper_list(list)
        true -> info_list(list)
      end

    [{"Data type", "List"}] ++ specific_info
  end

  defp info_printable_charlist(charlist) do
    description = """
    This is a list of integers that is printed as a sequence of characters
    delimited by single quotes because all the integers in it represent printable
    ASCII characters. Conventionally, a list of Unicode code points is known as a
    charlist and a list of ASCII characters is a subset of it.
    """

    [
      {"Description", description},
      {"Raw representation", inspect(charlist, charlists: :as_lists)},
      {"Reference modules", "List"}
    ]
  end

  defp info_kw_list(_kw_list) do
    description = """
    This is what is referred to as a "keyword list". A keyword list is a list
    of two-element tuples where the first element of each tuple is an atom.
    """

    [{"Description", description}, {"Reference modules", "Keyword, List"}]
  end

  defp info_improper_list(_improper_list) do
    description = """
    This is what is referred to as an "improper list". An improper list is a
    list which its last tail is not to an empty list. For example: [1, 2, 3]
    is a proper list, as it is equivalent to [1, 2, 3 | []], as opposed to
    [1, 2 | 3] which is an improper list since its last tail returns 3.
    """

    [
      {"Description", description},
      {"Reference modules", "List"}
    ]
  end

  defp info_list(_list) do
    [{"Reference modules", "List"}]
  end
end

defimpl IEx.Info, for: BitString do
  def info(bitstring) do
    specific_info =
      cond do
        is_binary(bitstring) and String.printable?(bitstring) -> info_string(bitstring)
        is_binary(bitstring) and String.valid?(bitstring) -> info_non_printable_string(bitstring)
        is_binary(bitstring) -> info_binary(bitstring)
        is_bitstring(bitstring) -> info_bitstring(bitstring)
      end

    [{"Data type", "BitString"}] ++ specific_info
  end

  defp info_string(bitstring) do
    description = """
    This is a string: a UTF-8 encoded binary. It's printed surrounded by
    "double quotes" because all UTF-8 encoded code points in it are printable.
    """

    [
      {"Byte size", byte_size(bitstring)},
      {"Description", description},
      {"Raw representation", inspect(bitstring, binaries: :as_binaries)},
      {"Reference modules", "String, :binary"}
    ]
  end

  defp info_non_printable_string(bitstring) do
    first_non_printable = find_first_codepoint(bitstring, &(not String.printable?(&1)))

    desc = """
    This is a string: a UTF-8 encoded binary. It's printed with the `<<>>`
    syntax (as opposed to double quotes) because it contains non-printable
    UTF-8 encoded code points (the first non-printable code point being
    `#{inspect(first_non_printable)}`).
    """

    [
      {"Byte size", byte_size(bitstring)},
      {"Description", desc},
      {"Reference modules", "String, :binary"}
    ]
  end

  defp info_binary(bitstring) do
    first_non_valid = find_first_codepoint(bitstring, &(not String.valid?(&1)))

    description = """
    This is a binary: a collection of bytes. It's printed with the `<<>>`
    syntax (as opposed to double quotes) because it is not a UTF-8 encoded
    binary (the first invalid byte being `#{inspect(first_non_valid)}`)
    """

    [
      {"Byte size", byte_size(bitstring)},
      {"Description", description},
      {"Reference modules", ":binary"}
    ]
  end

  defp info_bitstring(bitstring) do
    description = """
    This is a bitstring. It's a chunk of bits that are not divisible by 8
    (the number of bytes isn't whole).
    """

    [{"Bits size", bit_size(bitstring)}, {"Description", description}]
  end

  defp find_first_codepoint(binary, fun) do
    binary
    |> String.codepoints()
    |> Enum.find(fun)
  end
end

defimpl IEx.Info, for: Integer do
  def info(_integer) do
    [{"Data type", "Integer"}, {"Reference modules", "Integer"}]
  end
end

defimpl IEx.Info, for: Float do
  def info(_float) do
    [{"Data type", "Float"}, {"Reference modules", "Float"}]
  end
end

defimpl IEx.Info, for: Function do
  def info(fun) do
    fun_info = Function.info(fun)

    specific_info =
      if fun_info[:type] == :external and fun_info[:env] == [] do
        info_named_fun(fun_info)
      else
        info_anon_fun(fun_info)
      end

    [{"Data type", "Function"}] ++ specific_info
  end

  defp info_anon_fun(fun_info) do
    [
      {"Type", to_string(fun_info[:type])},
      {"Arity", fun_info[:arity]},
      {"Description", "This is an anonymous function."}
    ]
  end

  defp info_named_fun(fun_info) do
    [
      {"Type", to_string(fun_info[:type])},
      {"Arity", fun_info[:arity]}
    ]
  end
end

defimpl IEx.Info, for: PID do
  @keys [:registered_name, :links, :message_queue_len]

  def info(pid) do
    extra_info =
      case :rpc.pinfo(pid, @keys) do
        [_ | _] = info ->
          [
            {"Alive", true},
            {"Name", process_name(info[:registered_name])},
            {"Links", links(info[:links])},
            {"Message queue length", info[:message_queue_len]}
          ]

        _ ->
          [{"Alive", false}]
      end

    final_info = [
      {"Description", "Use Process.info/1 to get more info about this process"},
      {"Reference modules", "Process, Node"}
    ]

    [{"Data type", "PID"}] ++ extra_info ++ final_info
  end

  defp process_name([]), do: "not registered"
  defp process_name(name), do: inspect(name)

  defp links([]), do: "none"
  defp links(links), do: Enum.map_join(links, ", ", &inspect/1)
end

defimpl IEx.Info, for: Map do
  def info(_map) do
    [{"Data type", "Map"}, {"Reference modules", "Map"}]
  end
end

defimpl IEx.Info, for: Port do
  def info(port) do
    connected = :rpc.call(node(port), :erlang, :port_info, [port, :connected])

    [
      {"Data type", "Port"},
      {"Open", match?({:connected, _}, connected)},
      {"Reference modules", "Port"}
    ]
  end
end

defimpl IEx.Info, for: Reference do
  def info(_ref) do
    [{"Data type", "Reference"}]
  end
end

defimpl IEx.Info, for: [Date, Time, NaiveDateTime] do
  {sigil, repr} =
    case @for do
      Date -> {"D", "date"}
      Time -> {"T", "time"}
      NaiveDateTime -> {"N", ~S{"naive" datetime (that is, a datetime without a time zone)}}
    end

  def info(value) do
    description = """
    This is a struct representing a #{unquote(repr)}. It is commonly
    represented using the `~#{unquote(sigil)}` sigil syntax, that is
    defined in the `Kernel.sigil_#{unquote(sigil)}/2` macro.
    """

    [
      {"Data type", inspect(@for)},
      {"Description", description},
      {"Raw representation", raw_inspect(value)},
      {"Reference modules", inspect(@for) <> ", Calendar, Map"}
    ]
  end

  defp raw_inspect(value) do
    value
    |> Inspect.Any.inspect(%Inspect.Opts{})
    |> Inspect.Algebra.format(:infinity)
    |> IO.iodata_to_binary()
  end
end

defimpl IEx.Info, for: Any do
  def info(%module{}) do
    [
      {"Data type", inspect(module)},
      {"Description", "This is a struct. Structs are maps with a __struct__ key."},
      {"Reference modules", inspect(module) <> ", Map"}
    ]
  end
end
