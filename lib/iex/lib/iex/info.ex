defprotocol IEx.Info do
  @fallback_to_any true

  @spec info(term) :: [{atom, String.t}]
  def info(term)
end

defimpl IEx.Info, for: Tuple do
  def info(_tuple) do
    ["Data type": "Tuple",
     "Reference modules": "Tuple"]
  end
end

defimpl IEx.Info, for: Atom do
  def info(atom) do
    specific_info =
      cond do
        Code.ensure_loaded?(atom) ->
          info_module(atom)
        match?("Elixir." <> _, Atom.to_string(atom)) ->
          info_module_like_atom(atom)
        true ->
          info_atom(atom)
      end
    ["Data type": "Atom"] ++ specific_info
  end

  defp info_module(mod) do
    extra =
      if Code.get_docs(mod, :moduledoc) do
        "Use h(#{inspect mod}) to access its documentation.\n"
      else
        ""
      end

    mod_info = mod.module_info()
    generic_info =
      ["Module bytecode": module_object_file(mod),
       "Source": module_source_file(mod_info),
       "Version": module_version(mod_info),
       "Compile options": module_compile_options(mod_info),
       "Description": "#{extra}Call #{inspect mod}.module_info() to access metadata."]

    specific_info =
      if function_exported?(mod, :__protocol__, 1) do
        impls =
          mod
          |> Protocol.extract_impls(:code.get_path())
          |> Enum.map_join(", ", &inspect/1)
        ["Protocol": "This module is a protocol. These data structures implement it:\n  #{impls}"]
      else
        []
      end

    generic_info ++ specific_info ++
      ["Raw representation": ":" <> inspect(Atom.to_string(mod)),
       "Reference modules": "Module, Atom"]
  end

  defp info_module_like_atom(atom) do
    ["Raw representation": ":" <> inspect(Atom.to_string(atom)),
     "Reference modules": "Atom"]
  end

  defp info_atom(_atom) do
    ["Reference modules": "Atom"]
  end

  defp module_object_file(mod) do
    default_or_apply :code.which(mod), fn
      atom when is_atom(atom) -> inspect(atom)
      path                    -> Path.relative_to_cwd(path)
    end
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
        list == []                    -> info_list(list)
        Inspect.List.printable?(list) -> info_charlist(list)
        Keyword.keyword?(list)        -> info_kw_list(list)
        true                          -> info_list(list)
      end

    ["Data type": "List"] ++ specific_info
  end

  defp info_charlist(charlist) do
    desc = """
    This is a list of integers that is printed as a sequence of characters
    delimited by single quotes because all the integers in it represent valid
    ASCII characters. Conventionally, such lists of integers are referred to as
    "charlists" (more precisely, a charlist is a list of Unicode codepoints,
    and ASCII is a subset of Unicode).
    """

    ["Description": desc,
     "Raw representation": inspect(charlist, charlists: :as_lists),
     "Reference modules": "List"]
  end

  defp info_kw_list(_kw_list) do
    desc = """
    This is what is referred to as a "keyword list". A keyword list is a list
    of two-element tuples where the first element of each tuple is an atom.
    """

    ["Description": desc,
     "Reference modules": "Keyword, List"]
  end

  defp info_list(_list) do
    ["Reference modules": "List"]
  end
end

defimpl IEx.Info, for: BitString do
  def info(bitstring) do
    specific_info =
      cond do
        is_binary(bitstring) and String.printable?(bitstring) -> info_string(bitstring)
        is_binary(bitstring) and String.valid?(bitstring)     -> info_non_printable_string(bitstring)
        is_binary(bitstring)                                  -> info_binary(bitstring)
        is_bitstring(bitstring)                               -> info_bitstring(bitstring)
      end

    ["Data type": "BitString"] ++ specific_info
  end

  defp info_string(bitstring) do
    desc = """
    This is a string: a UTF-8 encoded binary. It's printed surrounded by
    "double quotes" because all UTF-8 encoded codepoints in it are printable.
    """
    ["Byte size": byte_size(bitstring),
     "Description": desc,
     "Raw representation": inspect(bitstring, binaries: :as_binaries),
     "Reference modules": "String, :binary"]
  end

  defp info_non_printable_string(bitstring) do
    first_non_printable =
      bitstring
      |> String.codepoints()
      |> Enum.find(fn cp -> not String.printable?(cp) end)

    desc = """
    This is a string: a UTF-8 encoded binary. It's printed with the `<<>>`
    syntax (as opposed to double quotes) because it contains non-printable
    UTF-8 encoded codepoints (the first non-printable codepoint being `#{inspect first_non_printable}`)
    """
    ["Byte size": byte_size(bitstring),
     "Description": desc,
     "Reference modules": "String, :binary"]
  end

  defp info_binary(bitstring) do
    first_non_valid =
      bitstring
      |> String.codepoints()
      |> Enum.find(fn cp -> not String.valid?(cp) end)

    desc = """
    This is a binary: a collection of bytes. It's printed with the `<<>>`
    syntax (as opposed to double quotes) because it is not a
    UTF-8 encoded binary (the first invalid byte being `#{inspect first_non_valid}`)
    """

    ["Byte size": byte_size(bitstring),
     "Description": desc,
     "Reference modules": ":binary"]
  end

  defp info_bitstring(bitstring) do
    desc = """
    This is a bitstring. It's a chunk of bits that are not divisible by 8
    (the number of bytes isn't whole).
    """

    ["Bits size": bit_size(bitstring),
     "Description": desc]
  end
end

defimpl IEx.Info, for: Integer do
  def info(_) do
    ["Data type": "Integer",
     "Reference modules": "Integer"]
  end
end

defimpl IEx.Info, for: Float do
  def info(_) do
    ["Data type": "Float",
     "Reference modules": "Float"]
  end
end

defimpl IEx.Info, for: Function do
  def info(fun) do
    fun_info = :erlang.fun_info(fun)

    specific_info =
      if fun_info[:type] == :external and fun_info[:env] == [] do
        info_named_fun(fun_info)
      else
        info_anon_fun(fun_info)
      end

    ["Data type": "Function"] ++ specific_info
  end

  defp info_anon_fun(fun_info) do
    ["Type": to_string(fun_info[:type]),
     "Arity": fun_info[:arity],
     "Description": "This is an anonymous function."]
  end

  defp info_named_fun(fun_info) do
    ["Type": to_string(fun_info[:type]),
     "Arity": fun_info[:arity]]
  end
end

defimpl IEx.Info, for: PID do
  @keys [:registered_name, :links, :message_queue_len]

  def info(pid) do
    extra =
      case :rpc.pinfo(pid, @keys) do
        [_ | _] = info ->
          ["Alive": true,
           "Name": process_name(info[:registered_name]),
           "Links": links(info[:links]),
           "Message queue length": info[:message_queue_len]]
        _ ->
          ["Alive": false]
      end

    ["Data type": "PID"] ++ extra ++
      ["Description": "Use Process.info/1 to get more info about this process",
       "Reference modules": "Process, Node"]
  end

  defp process_name([]), do: "not registered"
  defp process_name(name), do: inspect(name)

  defp links([]), do: "none"
  defp links(links), do: Enum.map_join(links, ", ", &inspect/1)
end

defimpl IEx.Info, for: Map do
  def info(_) do
    ["Data type": "Map",
     "Reference modules": "Map"]
  end
end

defimpl IEx.Info, for: Port do
  def info(port) do
    connected = :rpc.call(node(port), :erlang, :port_info, [port, :connected])

    ["Data type": "Port",
     "Open": match?({:connected, _}, connected),
     "Reference modules": "Port"]
  end
end

defimpl IEx.Info, for: Reference do
  def info(_) do
    ["Data type": "Reference"]
  end
end

defimpl IEx.Info, for: Any do
  def info(%{__struct__: mod}) do
    ["Data type": inspect(mod),
     "Description": "This is a struct. Structs are maps with a __struct__ key.",
     "Reference modules": inspect(mod) <> ", Map"]
  end
end
