defmodule OptionParser do
  @doc """
  Parses the argv and returns one tuple with parsed options
  and the arguments.

  ## Examples

      OptionParser.parse(["--debug"])
      #=> { [debug: true], [] }

      OptionParser.parse(["--source", "lib"])
      #=> { [source: "lib"], [] }

      OptionParser.parse(["--source", "lib", "test/enum_test.exs", "--verbose"])
      #=> { [source: "lib", verbose: true], ["test/enum_test.exs"] }

  ## Aliases

  A set of aliases can be given as second argument:

      OptionParser.parse(["-d"], aliases: [d: :debug])
      #=> { [debug: true], [] }

  ## Flags

  A set of flags can be given as argument too. Those are considered
  boolean and never consume the next value unless it is a boolean:

      OptionParser.parse(["--unlock path/to/file"], flags: [:unlock])
      #=> { [unlock: true], ["path/to/file"] }

      OptionParser.parse(["--unlock false path/to/file"], flags: [:unlock])
      #=> { [unlock: false], ["path/to/file"] }

  ## Negation switches

  Any switches starting with `--no-` are always considered to be
  booleans and never parse the next value.

      OptionParser.parse(["--no-op path/to/file"])
      #=> { [no_op: true], ["path/to/file"] }

  """
  def parse(argv, opts // []) when is_list(argv) and is_list(opts) do
    aliases = opts[:aliases] || []
    flags   = opts[:flags]   || []
    dict    = Keyword.new(flags, fn(k) -> { k, false } end)
    parse(argv, aliases, flags, dict, [], true)
  end

  @doc """
  Similar to parse but only parses the head of the argv.
  I.e. as soon as it finds a non switch, it stops parsing.

  Check `parse/2` for more info.

  ## Example

      OptionParser.parse_head(["--source", "lib", "test/enum_test.exs", "--verbose"])
      #=> { [source: "lib"], ["test/enum_test.exs", "--verbose"] }

  """
  def parse_head(argv, opts // []) when is_list(argv) and is_list(opts) do
    aliases = opts[:aliases] || []
    flags   = opts[:flags]   || []
    dict    = Keyword.new(flags, fn(k) -> { k, false } end)
    parse(argv, aliases, flags, dict, [], false)
  end

  ## Helpers

  defp parse(["-" <> option|t], aliases, flags, dict, args, all) do
    { option, value } = normalize_option(option, aliases)

    if value == nil do
      { value, t } = if is_flag?(flags, option) do
        flag_from_tail(t)
      else
        value_from_tail(t)
      end
    end

    dict = store_option dict, option, value
    parse(t, aliases, flags, dict, args, all)
  end

  defp parse([], _, _, dict, args, true) do
    { dict, Enum.reverse(args) }
  end

  defp parse([h|t], aliases, flags, dict, args, true) do
    parse(t, aliases, flags, dict, [h|args], true)
  end

  defp parse(value, _, _, dict, _args, false) do
    { dict, value }
  end

  defp flag_from_tail([h|t]) when h in ["false", "true"], do: { h, t }
  defp flag_from_tail(t)                                , do: { true, t }

  defp value_from_tail(["-" <> _|_] = t), do: { true, t }
  defp value_from_tail([h|t]),            do: { h, t }
  defp value_from_tail([]),               do: { true, [] }

  defp store_option(dict, option, value) when value in ["false", "true"] do
    store_option(dict, option, binary_to_atom(value))
  end

  defp store_option(dict, option, value) do
    Keyword.put dict, option, value
  end

  defp normalize_option(<<?-, option|:binary>>, aliases) do
    normalize_option(option, aliases)
  end

  defp normalize_option(option, aliases) do
    { option, value } = split_option(option)
    if is_no?(option), do: value = true
    atom = option /> to_underscore /> binary_to_atom
    { aliases[atom] || atom, value }
  end

  defp split_option(option) do
    case :binary.split(option, "=") do
      [h]   -> { h, nil }
      [h|t] -> { h, Enum.join(t, "=") }
    end
  end

  defp to_underscore(option) do
    bc <<c>> inbits option, do: << if c == ?-, do: ?_, else: c >>
  end

  defp is_no?("no-" <> _), do: true
  defp is_no?(_),          do: false

  defp is_flag?(flags, option), do: List.member?(flags, option)
end