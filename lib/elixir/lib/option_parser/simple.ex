defmodule OptionParser.Simple do
  @doc """
  Parses the argv and returns one tuple with parsed options
  and the arguments.

  ## Example

      OptionParser.Simple.parse(["--debug"])
      #=> { [debug: true], [] }

      OptionParser.Simple.parse(["--source", "lib"])
      #=> { [source: "lib"], [] }

      OptionParser.Simple.parse(["--source", "lib", "test/enum_test.exs", "--verbose"])
      #=> { [source: "lib", verbose: true], ["test/enum_test.exs"] }

  A set of aliases can be given as second argument:

      OptionParser.Simple.parse(["-d"], aliases: [d: :debug])
      #=> { [debug: true], [] }

  """
  def parse(argv, opts // []) when is_list(argv) and is_list(opts) do
    aliases = opts[:aliases] || []
    parse(argv, aliases, [], [], true)
  end

  @doc """
  Similar to parse but only parses the head of the argv.
  I.e. as soon as it finds a non switch, it stops parsing.

  ## Example

      OptionParser.Simple.parse_head(["--source", "lib", "test/enum_test.exs", "--verbose"])
      #=> { [source: "lib"], ["test/enum_test.exs", "--verbose"] }

  """
  def parse_head(argv, opts // []) when is_list(argv) and is_list(opts) do
    aliases = opts[:aliases] || []
    parse(argv, aliases, [], [], false)
  end

  ## Helpers

  defp parse([<<?-, option|:binary>>, h|t], aliases, dict, args, all) do
    option = normalize_option(option, aliases)

    case h do
      <<?-, _|:binary>> ->
        dict = Keyword.put dict, option, true
        parse([h|t], aliases, dict, args, all)
      _ ->
        dict = key_value(option, h, dict)
        parse(t, aliases, dict, args, all)
    end
  end

  defp parse([<<?-, option|:binary>>], aliases, dict, args, _all) do
    option = normalize_option(option, aliases)
    dict = Keyword.put dict, option, true
    { dict, args }
  end

  defp parse([], _, dict, args, true) do
    { dict, List.reverse(args) }
  end

  defp parse([h|t], aliases, dict, args, true) do
    parse(t, aliases, dict, [h|args], true)
  end

  defp parse(value, _, dict, _args, false) do
    { dict, value }
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
    option = bc <<c>> inbits option, do: << if c == ?-, do: ?_, else: c >>
    option = binary_to_atom(option)
    aliases[option] || option
  end
end