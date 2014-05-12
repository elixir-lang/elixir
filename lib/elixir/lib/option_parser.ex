defmodule OptionParser do
  @moduledoc """
  This module contains functions to parse command line arguments.
  """

  @doc """
  Parses `argv` into a keywords list.

  It returns the parsed values, remaining arguments and the
  invalid options.

  ## Examples

      iex> OptionParser.parse(["--debug"])
      {[debug: true], [], []}

      iex> OptionParser.parse(["--source", "lib"])
      {[source: "lib"], [], []}

      iex> OptionParser.parse(["--source-path", "lib", "test/enum_test.exs", "--verbose"])
      {[source_path: "lib", verbose: true], ["test/enum_test.exs"], []}

  By default, Elixir will try to automatically parse switches.
  Switches without an argument, like `--debug` will automatically
  be set to true. Switches followed by a value will be assigned
  to the value, always as strings.

  Note Elixir also converts the switches to underscore atoms, as
  `--source-path` becomes `:source_path`, to better suit Elixir
  conventions.

  ## Switches

  Many times though, it is better to explicitly list the available
  switches and their formats. The following types are supported:

  * `:boolean` - Marks the given switch as a boolean. Boolean switches
                 never consume the following value unless it is
                 `true` or `false`;
  * `:integer` - Parses the switch as an integer;
  * `:float`   - Parses the switch as a float;
  * `:string`  - Returns the switch as a string;

  If a switch can't be parsed, the option is returned in the invalid
  options list (third element of the returned tuple).

  The following extra "types" are supported:

  * `:keep` - Keeps duplicated items in the list instead of overriding;

  Examples:

      iex> OptionParser.parse(["--unlock", "path/to/file"], switches: [unlock: :boolean])
      {[unlock: true], ["path/to/file"], []}

      iex> OptionParser.parse(["--unlock", "--limit", "0", "path/to/file"],
      ...>                    switches: [unlock: :boolean, limit: :integer])
      {[unlock: true, limit: 0], ["path/to/file"], []}

      iex> OptionParser.parse(["--limit", "3"], switches: [limit: :integer])
      {[limit: 3], [], []}

      iex> OptionParser.parse(["--limit", "yyz"], switches: [limit: :integer])
      {[], [], [limit: "yyz"]}

  ## Negation switches

  All switches starting with `--no-` are considered to be booleans and never
  parse the next value:

      iex> OptionParser.parse(["--no-op", "path/to/file"])
      {[no_op: true], ["path/to/file"], []}

  However, in case the base switch exists, it sets that particular switch to
  false:

      iex> OptionParser.parse(["--no-op", "path/to/file"], switches: [op: :boolean])
      {[op: false], ["path/to/file"], []}

  ## Aliases

  A set of aliases can be given as options too:

      iex> OptionParser.parse(["-d"], aliases: [d: :debug])
      {[debug: true], [], []}

  """
  def parse(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    parse(argv, opts, true)
  end

  @doc """
  Similar to `parse/2` but only parses the head of `argv`;
  as soon as it finds a non-switch, it stops parsing.

  See `parse/2` for more information.

  ## Example

      iex> OptionParser.parse_head(["--source", "lib", "test/enum_test.exs", "--verbose"])
      {[source: "lib"], ["test/enum_test.exs", "--verbose"], []}

      iex> OptionParser.parse_head(["--verbose", "--source", "lib", "test/enum_test.exs", "--unlock"])
      {[verbose: true, source: "lib"], ["test/enum_test.exs", "--unlock"], []}

  """
  def parse_head(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    parse(argv, opts, false)
  end

  ## Helpers

  defp parse(argv, opts, bool) do
    aliases  = opts[:aliases]  || []
    switches = opts[:switches] || []
    parse(argv, aliases, switches, bool)
  end

  defp parse(argv, aliases, switches, all) do
    parse(argv, aliases, switches, [], [], [], all)
  end

  defp parse(["--"|_] = value, _aliases, _switches, dict, args, invalid, _all) do
    {Enum.reverse(dict), Enum.reverse(args, value), Enum.reverse(invalid)}
  end

  defp parse(["-" <> option|t], aliases, switches, dict, args, invalid, all) do
    {option, value} = split_option(option)
    {option, kinds, value} = normalize_option(option, value, switches, aliases)
    {value, kinds, t} = normalize_value(value, kinds, t)
    {dict, invalid} = store_option(dict, invalid, option, value, kinds)
    parse(t, aliases, switches, dict, args, invalid, all)
  end

  defp parse([h|t], aliases, switches, dict, args, invalid, true) do
    parse(t, aliases, switches, dict, [h|args], invalid, true)
  end

  defp parse([], _, _switches, dict, args, invalid, true) do
    {Enum.reverse(dict), Enum.reverse(args), Enum.reverse(invalid)}
  end

  defp parse(value, _, _switches, dict, _args, invalid, false) do
    {Enum.reverse(dict), value, Enum.reverse(invalid)}
  end

  defp store_option(dict, invalid, option, value, kinds) do
    {invalid_option, value} =
      cond do
        :invalid in kinds ->
          {option, value}
        :boolean in kinds ->
          case value do
            t when t in [true, "true"] -> {nil, true}
            f when f in [false, "false"] -> {nil, false}
            _ -> {option, value}
          end
        :integer in kinds ->
          case Integer.parse(value) do
            {value, ""} -> {nil, value}
            _ -> {option, value}
          end
        :float in kinds ->
          case Float.parse(value) do
            {value, ""} -> {nil, value}
            _ -> {option, value}
          end
        true ->
          {nil, value}
      end

    if invalid_option do
      {dict, [{option, value}|invalid]}
    else
      {do_store_option(dict, option, value, kinds), invalid}
    end
  end

  defp do_store_option(dict, option, value, kinds) do
    cond do
      :keep in kinds ->
        [{option, value}|dict]
      true ->
        [{option, value}|Keyword.delete(dict, option)]
    end
  end

  defp normalize_option(<<?-, option :: binary>>, value, switches, _aliases) do
    normalize_option(get_negated(option, switches), value, switches)
  end

  defp normalize_option(option, value, switches, aliases) do
    option = get_option(option)
    if alias = aliases[option] do
      normalize_option({:default, alias}, value, switches)
    else
      {option, [:invalid], value}
    end
  end

  defp normalize_option({:negated, option}, nil, switches) do
    kinds = List.wrap(switches[option])

    cond do
      :boolean in kinds ->
        {option, kinds, false}
      kinds == [] ->
        {option, kinds, true}
      true ->
        {option, [:invalid], false}
    end
  end

  defp normalize_option({:negated, option}, value, _switches) do
    {option, [:invalid], value}
  end

  defp normalize_option({:default, option}, value, switches) do
    {option, List.wrap(switches[option]), value}
  end

  defp normalize_value(nil, kinds, t) do
    cond do
      :boolean in kinds ->
        {true, kinds, t}
      value_in_tail?(t) ->
        [h|t] = t
        {h, kinds, t}
      kinds == [] ->
        {true, kinds, t}
      true ->
        {true, [:invalid], t}
    end
  end

  defp normalize_value(value, kinds, t) do
    {value, kinds, t}
  end

  defp value_in_tail?(["-" <> _|_]), do: false
  defp value_in_tail?([]),           do: false
  defp value_in_tail?(_),            do: true

  defp split_option(option) do
    case :binary.split(option, "=") do
      [h]    -> {h, nil}
      [h, t] -> {h, t}
    end
  end

  defp to_underscore(option) do
    for <<c <- option>>, into: "", do: << if(c == ?-, do: ?_, else: c) >>
  end

  defp get_option(option) do
    option |> to_underscore |> binary_to_atom
  end

  defp get_negated("no-" <> rest = option, switches) do
    negated = get_option(rest)
    option  = if Keyword.has_key?(switches, negated), do: negated, else: get_option(option)
    {:negated, option}
  end

  defp get_negated(rest, _switches) do
    {:default, get_option(rest)}
  end
end
