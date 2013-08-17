defmodule OptionParser do
  @moduledoc """
  This module contains functions to parse command line arguments.
  """

  @doc """
  Parses `argv` and returns a tuple with the parsed options, its
  arguments, and a list options that couldn't be parsed.

  ## Examples

      iex> OptionParser.parse(["--debug"])
      { [debug: true], [], [] }

      iex> OptionParser.parse(["--source", "lib"])
      { [source: "lib"], [], [] }

      iex> OptionParser.parse(["--source-path", "lib", "test/enum_test.exs", "--verbose"])
      { [source_path: "lib", verbose: true], ["test/enum_test.exs"], [] }

  Notice how Elixir automatically translates the "--source-path"
  switch to the underscored atom `:source_path`, which better follows
  Elixir conventions.

  ## Aliases

  A set of aliases can be given as the second argument:

      iex> OptionParser.parse(["-d"], aliases: [d: :debug])
      { [debug: true], [], [] }

  ## Switches

  Extra information about switches can be given as arguments, too.
  This is useful when a switch must behave as a boolean
  or if duplicated switches should be kept, overriden or accumulated.

  The following types are supported:

  * `:boolean` - Marks the given switch as a boolean. Boolean switches
                 never consume the following value unless it is
                 `true` or `false`;
  * `:integer` - Parses the switch as an integer;
  * `:float`   - Parses the switch as a float;

  If a switch can't be parsed, the option is returned in the invalid
  options list (third element of the returned tuple).

  The following extra options are supported:

  * `:keep` - Keeps duplicated items in the list instead of overriding;

  Examples:

      iex> OptionParser.parse(["--unlock", "path/to/file"], switches: [unlock: :boolean])
      { [unlock: true], ["path/to/file"], [] }

      iex> OptionParser.parse(["--unlock", "--limit", "0", "path/to/file"],
      ...>                    switches: [unlock: :boolean, limit: :integer])
      { [unlock: true, limit: 0], ["path/to/file"], [] }

      iex> OptionParser.parse(["-limit", "3"], switches: [limit: :integer])
      { [limit: 3], [], [] }

      iex> OptionParser.parse(["-limit", "yyz"], switches: [limit: :integer])
      { [], [], [limit: "yyz"] }

  ## Negation switches

  Any switches starting with `--no-` are always considered to be
  booleans and never parse the next value:

      iex> OptionParser.parse(["--no-op", "path/to/file"])
      { [no_op: true], ["path/to/file"], [] }

  In case the negated switch exists as a boolean, it sets the boolean to false:

      iex> OptionParser.parse(["--no-op", "path/to/file"], switches: [op: :boolean])
      { [op: false], ["path/to/file"], [] }

  """
  def parse(argv, opts // []) when is_list(argv) and is_list(opts) do
    parse(argv, opts, true)
  end

  @doc """
  Similar to `parse/2` but only parses the head of `argv`;
  as soon as it finds a non-switch, it stops parsing.

  See `parse/2` for more information.

  ## Example

      iex> OptionParser.parse_head(["--source", "lib", "test/enum_test.exs", "--verbose"])
      { [source: "lib"], ["test/enum_test.exs", "--verbose"], [] }

      iex> OptionParser.parse_head(["--verbose", "--source", "lib", "test/enum_test.exs", "--unlock"])
      { [verbose: true, source: "lib"], ["test/enum_test.exs", "--unlock"], [] }
  """
  def parse_head(argv, opts // []) when is_list(argv) and is_list(opts) do
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

  defp parse(["--"|_] = value, _aliases, _switches, dict, _args, invalid, _all) do
    { Enum.reverse(dict), value, Enum.reverse(invalid) }
  end

  defp parse(["-" <> option|t], aliases, switches, dict, args, invalid, all) do
    { option, kinds, value } = normalize_option(option, switches, aliases)

    if nil?(value) do
      { value, t } =
        if :boolean in kinds do
          { true, t }
        else
          value_from_tail(t)
        end
    end

    { dict, invalid } = store_option(dict, invalid, option, value, kinds)

    parse(t, aliases, switches, dict, args, invalid, all)
  end

  defp parse([h|t], aliases, switches, dict, args, invalid, true) do
    parse(t, aliases, switches, dict, [h|args], invalid, true)
  end

  defp parse([], _, _switches, dict, args, invalid, true) do
    { Enum.reverse(dict), Enum.reverse(args), Enum.reverse(invalid) }
  end

  defp parse(value, _, _switches, dict, _args, invalid, false) do
    { Enum.reverse(dict), value, Enum.reverse(invalid) }
  end

  defp value_from_tail(["-" <> _|_] = t), do: { true, t }
  defp value_from_tail([h|t]),            do: { h, t }
  defp value_from_tail([]),               do: { true, [] }

  defp store_option(dict, invalid, option, value, kinds) do
    { invalid_option, value } =
      cond do
        :boolean in kinds ->
          { nil, value in [true, "true"] }
        :integer in kinds ->
          case String.to_integer(value) do
            { value, "" } -> { nil, value }
            _ -> { option, value }
          end
        :float in kinds ->
          case String.to_float(value) do
            { value, "" } -> { nil, value }
            _ -> { option, value }
          end
        true ->
          { nil, value }
      end

    if invalid_option do
      { dict, [{ option, value }|invalid] }
    else
      { do_store_option(dict, option, value, kinds), invalid }
    end
  end

  defp do_store_option(dict, option, value, kinds) do
    cond do
      :keep in kinds ->
        [{ option, value }|dict]
      true ->
        [{ option, value }|Keyword.delete(dict, option)]
    end
  end

  defp normalize_option(<<?-, option :: binary>>, switches, aliases) do
    normalize_option(option, switches, aliases)
  end

  defp normalize_option(option, switches, aliases) do
    { option, value } = split_option(option)

    if non_neg = get_non_negated(option, aliases) do
      kinds = List.wrap(switches[non_neg])

      if :boolean in kinds do
        { non_neg, kinds, false }
      else
        { get_aliased(option, aliases), [:boolean], true }
      end
    else
      atom = get_aliased(option, aliases)
      { atom, List.wrap(switches[atom]), value }
    end
  end

  defp split_option(option) do
    case :binary.split(option, "=") do
      [h]    -> { h, nil }
      [h, t] -> { h, t }
    end
  end

  defp to_underscore(option) do
    bc <<c>> inbits option, do: << if(c == ?-, do: ?_, else: c) >>
  end

  defp get_aliased(option, aliases) do
    atom = option |> to_underscore |> binary_to_atom
    aliases[atom] || atom
  end

  defp get_non_negated("no-" <> rest, aliases), do: get_aliased(rest, aliases)
  defp get_non_negated(_, _),                   do: nil
end
