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
    config =
      Keyword.merge(opts, [all: true, strict: false])
      |> compile_config()
    do_parse(argv, config, [], [], [])
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
    config =
      Keyword.merge(opts, [all: false, strict: false])
      |> compile_config()
    do_parse(argv, config, [], [], [])
  end


  defp do_parse([], _config, opts, args, invalid) do
    {Enum.reverse(opts), Enum.reverse(args), Enum.reverse(invalid)}
  end

  defp do_parse(argv, {aliases, switches, strict, all}=config, opts, args, invalid) do
    case next(argv, aliases, switches, strict) do
      {:ok, option, value, rest} ->
        # the option exist and it was successfully parsed
        kinds = List.wrap Keyword.get(switches, option)
        new_opts = do_store_option(opts, option, value, kinds)
        do_parse(rest, config, new_opts, args, invalid)

      {:error, {:value, option, value}, rest} ->
        # the option exist but it has wrong value
        do_parse(rest, config, opts, args, [{option, value}|invalid])

      {:error, {:undefined, option, value}, rest} ->
        # the option does not exist (for strict cases)
        do_parse(rest, config, opts, args, [{option, value}|invalid])

      {:error, ["--"|rest]} ->
        {Enum.reverse(opts), Enum.reverse(args, rest), Enum.reverse(invalid)}

      {:error, [arg|rest]=remaining_args} ->
        # there is no option
        if all do
          do_parse(rest, config, opts, [arg|args], invalid)
        else
          {Enum.reverse(opts), Enum.reverse(args, remaining_args), Enum.reverse(invalid)}
        end
    end
  end


  @doc """
  Low-level function that parses one option.
  """
  def next(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    {aliases, switches, strict, _} = compile_config(opts)
    next(argv, aliases, switches, strict)
  end

  defp next([], _aliases, _switches, _strict) do
    {:error, []}
  end

  defp next(["--"|_]=argv, _aliases, _switches, _strict) do
    {:error, argv}
  end

  defp next(["-" <> option|rest], aliases, switches, strict) do
    {option, value} = split_option(option)
    opt = tag_option(option, value, switches, aliases)

    if strict and not option_defined?(opt, switches) do
      {_, opt_name} = opt
      {:error, {:undefined, opt_name, value}, rest}
    else
      {opt_name, kinds, value} = normalize_option(opt, value, switches)
      {value, kinds, rest} = normalize_value(value, kinds, rest, strict)
      case validate_option(opt_name, value, kinds) do
        {:ok, new_value} -> {:ok, opt_name, new_value, rest}
        :invalid         -> {:error, {:value, opt_name, value}, rest}
      end
    end
  end

  defp next(argv, _aliases, _switches, _strict) do
    {:error, argv}
  end

  ## Helpers

  defp compile_config(opts) do
    aliases  = opts[:aliases]  || []
    switches = opts[:switches] || []
    strict   = opts[:strict]   || false
    all      = opts[:all]      || false
    {aliases, switches, strict, all}
  end

  defp validate_option(option, value, kinds) do
    {invalid_opt, value} = cond do
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

    if invalid_opt do
      :invalid
    else
      {:ok, value}
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

  defp tag_option(<<?-, option :: binary>>, value, switches, _aliases) do
    get_negated(option, value, switches)
  end

  defp tag_option(option, _value, _switches, aliases) when is_binary(option) do
    opt = get_option(option)
    if alias = aliases[opt] do
      {:default, alias}
    else
      {:unknown, opt}
    end
  end

  defp option_defined?({:unknown, _option}, _switches) do
    false
  end

  defp option_defined?({:negated, option}, switches) do
    Keyword.has_key?(switches, option)
  end

  defp option_defined?({:default, option}, switches) do
    Keyword.has_key?(switches, option)
  end

  defp normalize_option({:unknown, option}, value, _switches) do
    {option, [:invalid], value}
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

  defp normalize_value(nil, kinds, t, strict) do
    null = if strict, do: nil, else: true
    cond do
      :boolean in kinds ->
        {true, kinds, t}
      value_in_tail?(t) ->
        [h|t] = t
        {h, kinds, t}
      kinds == [] ->
        {null, kinds, t}
      true ->
        {null, [:invalid], t}
    end
  end

  defp normalize_value(value, kinds, t, _) do
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
    option |> to_underscore |> String.to_atom
  end

  defp get_negated("no-" <> rest = option, value, switches) do
    negated = get_option(rest)
    option  = if Keyword.has_key?(switches, negated) and value == nil do
      negated
    else
      get_option(option)
    end
    {:negated, option}
  end

  defp get_negated(rest, _value, _switches) do
    {:default, get_option(rest)}
  end
end
