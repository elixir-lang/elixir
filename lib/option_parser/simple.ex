defmodule OptionParser.Simple do
  @doc """
  Parses the argv and returns one tuple with parsed options
  and the arguments.

  ## Example

      OptionParser.Simple.parse(["--debug"])
      #=> { [debug: true], [] }

      OptionParser.Simple.parse(["--source", "lib"])
      #=> { [source: "lib"], [] }

      OptionParser.Simple.parse(["--source", "lib", "test/enum_test.exs"])
      #=> { [source: "lib"], ["test/enum_test.exs"] }

  A set of aliases can be given as second argument:

      OptionParser.Simple.parse(["-d"], [d: :debug])
      #=> { [debug: true], [] }

  """
  def parse(options, aliases // []) when is_list(options) and is_list(aliases) do
    parse(options, aliases, [], [])
  end

  ## Helpers

  defp parse([<<?-, option|:binary>>, h|t], aliases, dict, args) do
    option = normalize_option(option, aliases)

    case h do
    match: <<?-, _|:binary>>
      dict = Keyword.put dict, option, true
      parse([h|t], aliases, dict, args)
    else:
      dict = key_value(option, h, dict)
      parse(t, aliases, dict, args)
    end
  end

  defp parse([<<?-, option|:binary>>], aliases, dict, args) do
    option = normalize_option(option, aliases)
    dict = Keyword.put dict, option, true
    { dict, args }
  end

  defp parse(value, _, dict, args) do
    { dict, List.concat(args, value) }
  end

  defp key_value(key, boolean, dict) when boolean in ["false", "true"] do
    Keyword.put dict, key, binary_to_atom(boolean)
  end

  defp key_value(key, value, dict) do
    Keyword.put dict, key, value
  end

  defp normalize_option(<<?-, option|:binary>>, aliases) do
    normalize_option(option, aliases)
  end

  defp normalize_option(option, aliases) do
    option = binary_to_atom(option)
    aliases[option] || option
  end
end