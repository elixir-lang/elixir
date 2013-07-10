defmodule OptionParser do
  @moduledoc """
  This module contains functions to parse command line arguments.
  """

  @doc """
  Parses `argv` and returns a tuple with parsed options
  and arguments.

  ## Examples

      iex> OptionParser.parse(["--debug"])
      { [debug: true], [] }

      iex> OptionParser.parse(["--source", "lib"])
      { [source: "lib"], [] }

      iex> OptionParser.parse(["--source", "lib", "test/enum_test.exs", "--verbose"])
      { [source: "lib", verbose: true], ["test/enum_test.exs"] }

  ## Aliases

  A set of aliases can be given as the second argument:

      iex> OptionParser.parse(["-d"], aliases: [d: :debug])
      { [debug: true], [] }

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

  The following extra options are supported:

  * `:keep` - Keeps duplicated items in the list instead of overriding;

  Examples:

      iex> OptionParser.parse(["--unlock", "path/to/file"], switches: [unlock: :boolean])
      { [unlock: true], ["path/to/file"] }

      iex> OptionParser.parse(["--unlock", "false", "path/to/file"], switches: [unlock: :boolean])
      { [unlock: false], ["path/to/file"] }

  ## Negation switches

  Any switches starting with `--no-` are always considered to be
  booleans and never parse the next value:

      iex> OptionParser.parse(["--no-op", "path/to/file"])
      { [no_op: true], ["path/to/file"] }

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
      { [source: "lib"], ["test/enum_test.exs", "--verbose"] }

      iex> OptionParser.parse_head(["--verbose", "--source", "lib", "test/enum_test.exs", "--unlock"])
      {[verbose: true, source: "lib"], ["test/enum_test.exs", "--unlock"]}
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
    parse(argv, aliases, switches, [], [], all)
  end

  defp parse(["-" <> option|t], aliases, switches, dict, args, all) do
    { option, value } = normalize_option(option, aliases)
    kind = switches[option]

    if value == nil do
      { value, t } =
        if is_switch_a? :boolean, kind do
          boolean_from_tail(t)
        else
          value_from_tail(t)
        end
    end

    dict = store_option dict, option, value, kind
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

  defp store_option(dict, option, value, switches) when value in ["true", "false"] do
    store_option dict, option, binary_to_atom(value), switches
  end

  defp store_option(dict, option, value, kind) do
    case kind do
      :keep ->
        [{ option, value }|dict]
      :integer ->
        case String.to_integer(value) do
          { value, "" } -> [{ option, value }|Keyword.delete(dict, option)]
          _ -> dict
        end
      :float ->
        case String.to_float(value) do
          { value, "" } -> [{ option, value }|Keyword.delete(dict, option)]
          _ -> dict
        end
      _ ->
        [{ option, value }|Keyword.delete(dict, option)]
    end
  end

  defp reverse_dict(dict, switches) do
    switches = lc { k, v } inlist switches,
                  is_switch_a?(:boolean, v),
                  not Keyword.has_key?(dict, k), do: { k, false }
    Enum.reverse switches ++ dict
  end

  defp normalize_option(<<?-, option :: binary>>, aliases) do
    normalize_option(option, aliases)
  end

  defp normalize_option(option, aliases) do
    { option, value } = split_option(option)
    if is_no?(option), do: value = true
    atom = option |> to_underscore |> binary_to_atom
    { aliases[atom] || atom, value }
  end

  defp split_option(option) do
    case :binary.split(option, "=") do
      [h]   -> { h, nil }
      [h|t] -> { h, Enum.join(t, "=") }
    end
  end

  defp to_underscore(option) do
    bc <<c>> inbits option, do: << if(c == ?-, do: ?_, else: c) >>
  end

  defp is_no?("no-" <> _), do: true
  defp is_no?(_),          do: false

  defp is_switch_a?(kind, list) when is_list(list), do: kind in list
  defp is_switch_a?(kind, kind), do: true
  defp is_switch_a?(_, _),       do: false
end
