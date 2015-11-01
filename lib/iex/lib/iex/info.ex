defprotocol IEx.Info do
  @spec info(term) :: [{atom, iodata}]
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
      case Atom.to_string(atom) do
        "Elixir." <> _ -> info_module(atom)
        atom           -> info_atom(atom)
      end

    ["Data type": "Atom"] ++ specific_info
  end

  defp info_module(_mod) do
    ["Reference modules": "Atom, Module"]
  end

  defp info_atom(_atom) do
    ["Reference modules": "Atom"]
  end
end

defimpl IEx.Info, for: List do
  def info(list) do
    specific_info =
      cond do
        :io_lib.printable_unicode_list(list) -> info_char_list(list)
        Keyword.keyword?(list)               -> info_kw_list(list)
        true                                 -> []
      end

    ["Data type": "List"] ++ specific_info
  end

  defp info_char_list(char_list) do
    desc = """
    This is a list of integers that is printed as a sequence of codepoints
    delimited by single quotes because all the integers in it represent valid
    UTF-8 characters. Conventionally, such lists of integers are referred to as
    "char lists".
    """

    ["Description": desc,
     "Raw representation": inspect(char_list, char_lists: :as_lists)]
  end

  defp info_kw_list(_kw_list) do
    desc = """
    This is what is referred to as a "keyword list". A keyword list is just a
    list of two-element tuples where the first element of each tuple is an atom.
    """
    ["Description": desc]
  end
end

defimpl IEx.Info, for: PID do
  def info(pid) do
    ["Data type": "PID",
     "Alive": Process.alive?(pid),
     "Name": process_name(pid),
     "Reference modules": "Process, Node",
     "Description": "Use Process.info/1 for more info about this process"]
  end

  defp process_name(pid) do
    if name = Process.info(pid)[:registered_name] do
      inspect(name)
    else
      "not registered"
    end
  end
end
