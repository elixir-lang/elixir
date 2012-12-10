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

  ## Switches

  Extra information about switches can be given as argument too. This is useful
  in order to say a switch must behave as a boolean, list, etc. The following
  types are supported:

  * `:boolean` - They never consume the next value unless it is true/false;

  Examples:

      OptionParser.parse(["--unlock path/to/file"], switches: [unlock: :boolean])
      #=> { [unlock: true], ["path/to/file"] }

      OptionParser.parse(["--unlock false path/to/file"], switches: [unlock: :boolean])
      #=> { [unlock: false], ["path/to/file"] }

  ## Negation switches

  Any switches starting with `--no-` are always considered to be
  booleans and never parse the next value:

      OptionParser.parse(["--no-op path/to/file"])
      #=> { [no_op: true], ["path/to/file"] }

  """
  def parse(argv, opts // []) when is_list(argv) and is_list(opts) do
    parse(argv, opts, true)
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
    parse(argv, opts, false)
  end

  ## Helpers

  defp parse(argv, opts, bool) do
    if flags = opts[:flags] do
      IO.write "OptionParser option :flags is deprecated, please use :switches instead\n#{Exception.formatted_stacktrace}"
      opts = Keyword.put(opts, :switches, lc(k inlist flags, do: { k, :boolean }))
    end

    aliases  = opts[:aliases]  || []
    switches = opts[:switches] || []
    parse(argv, aliases, switches, bool)
  end

  defp parse(argv, aliases, switches, all) do
    parse(argv, aliases, switches, [], [], all)
  end

  defp parse(["-" <> option|t], aliases, switches, dict, args, all) do
    { option, value } = normalize_option(option, aliases)

    if value == nil do
      { value, t } =
        case switch_type(switches, option) do
          :boolean -> boolean_from_tail(t)
          _        -> value_from_tail(t)
        end
    end

    dict = store_option dict, option, value
    parse(t, aliases, switches, dict, args, all)
  end

  defp parse([h|t], aliases, switches, dict, args, true) do
    parse(t, aliases, switches, dict, [h|args], true)
  end

  defp parse([], _, switches, dict, args, true) do
    { reverse_dict(dict, switches), Enum.reverse(args) }
  end

  defp parse(value, _, switches, dict, _args, false) do
    { reverse_dict(dict, switches), value }
  end

  defp boolean_from_tail([h|t]) when h in ["false", "true"], do: { h, t }
  defp boolean_from_tail(t)                                , do: { true, t }

  defp value_from_tail(["-" <> _|_] = t), do: { true, t }
  defp value_from_tail([h|t]),            do: { h, t }
  defp value_from_tail([]),               do: { true, [] }

  defp store_option(dict, option, value) when value in ["false", "true"] do
    store_option(dict, option, binary_to_atom(value))
  end

  defp store_option(dict, option, value) do
    [{ option, value }|dict]
  end

  defp reverse_dict(dict, switches) do
    switches = lc { k, :boolean } inlist switches, not Keyword.has_key?(dict, k), do: { k, false }
    Enum.reverse switches ++ dict
  end

  defp normalize_option(<<?-, option :: binary>>, aliases) do
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

  defp switch_type(switches, option) do
    switches[option] || :default
  end
end