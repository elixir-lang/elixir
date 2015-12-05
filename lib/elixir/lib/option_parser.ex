defmodule OptionParser do
  @moduledoc """
  This module contains functions to parse command line options.
  """

  @type argv    :: [String.t]
  @type parsed  :: Keyword.t
  @type errors  :: [{String.t, String.t | nil}]
  @type options :: [switches: Keyword.t, strict: Keyword.t, aliases: Keyword.t]

  @doc """
  Parses `argv` into a keywords list.

  It returns a three-element tuple as follows:

     1. parsed switches,
     2. remaining arguments,
     3. invalid options.

  ## Examples

      iex> OptionParser.parse(["--debug"])
      {[debug: true], [], []}

      iex> OptionParser.parse(["--source", "lib"])
      {[source: "lib"], [], []}

      iex> OptionParser.parse(["--source-path", "lib", "test/enum_test.exs", "--verbose"])
      {[source_path: "lib", verbose: true], ["test/enum_test.exs"], []}

  By default, Elixir will try to automatically parse all switches.
  Switches followed by a value will be assigned the value, as a string.
  Switches without an argument, like `--debug` will automatically
  be set to `true`.

  Note: Elixir also converts the switches to underscore atoms, so
  `--source-path` becomes `:source_path`, to better suit Elixir
  conventions. This means that option names on the command line cannot contain
  underscores; such options will be put in the invalid options list.

  ## Switch Definitions

  Often it is better to explicitly list the known
  switches and their formats. The switches can be specified via two
  alternative options:

    * `:switches` - defines some switches. An attempt is still made to parse
      switches that do not appear in the list.

    * `:strict` - the switches are strict. Any switch that is not specified
      in the list is returned in the invalid options list.

  Note that you should only supply the `:switches` or `:strict` option. If you
  supply both, an error will be raised.

  For each switch, the following types are supported:

    * `:boolean` - marks the given switch as a boolean. Boolean switches
      never consume the following value unless it is `true` or
      `false`.
    * `:integer` - parses the switch as an integer.
    * `:float`   - parses the switch as a float.
    * `:string`  - returns the switch as a string.

  If a switch can't be parsed, it is returned in the invalid options list.

  The following extra "types" are supported:

    * `:keep` - keeps duplicated items in the list instead of overriding them.

  Note: if you want to use `:keep` with a non-string type, use a list, e.g.
  `[foo: [:integer, :keep]]`.

  Examples:

      iex> OptionParser.parse(["--unlock", "path/to/file"], strict: [unlock: :boolean])
      {[unlock: true], ["path/to/file"], []}

      iex> OptionParser.parse(["--unlock", "--limit", "0", "path/to/file"],
      ...>                    strict: [unlock: :boolean, limit: :integer])
      {[unlock: true, limit: 0], ["path/to/file"], []}

      iex> OptionParser.parse(["--limit", "3"], strict: [limit: :integer])
      {[limit: 3], [], []}

      iex> OptionParser.parse(["--limit", "xyz"], strict: [limit: :integer])
      {[], [], [{"--limit", "xyz"}]}

      iex> OptionParser.parse(["--unknown", "xyz"], strict: [])
      {[], ["xyz"], [{"--unknown", nil}]}

      iex> OptionParser.parse(["--limit", "3", "--unknown", "xyz"],
      ...>                    switches: [limit: :integer])
      {[limit: 3, unknown: "xyz"], [], []}

      iex> OptionParser.parse(["--unlock", "path/to/file", "--unlock", "path/to/another/file"], strict: [unlock: :keep])
      {[unlock: "path/to/file", unlock: "path/to/another/file"], [], []}

  ## Negation switches

  In case a switch is declared as boolean, it may be passed as `--no-SWITCH`
  which will set the option to `false`:

      iex> OptionParser.parse(["--no-op", "path/to/file"], switches: [op: :boolean])
      {[op: false], ["path/to/file"], []}

  ## Aliases

  A set of aliases can be given as options too:

      iex> OptionParser.parse(["-d"], aliases: [d: :debug])
      {[debug: true], [], []}

  """
  @spec parse(argv, options) :: {parsed, argv, errors}
  def parse(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    do_parse(argv, compile_config(opts), [], [], [], true)
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
  @spec parse_head(argv, options) :: {parsed, argv, errors}
  def parse_head(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    do_parse(argv, compile_config(opts), [], [], [], false)
  end

  defp do_parse([], _config, opts, args, invalid, _all?) do
    {Enum.reverse(opts), Enum.reverse(args), Enum.reverse(invalid)}
  end

  defp do_parse(argv, {aliases, switches, strict}=config, opts, args, invalid, all?) do
    case next(argv, aliases, switches, strict) do
      {:ok, option, value, rest} ->
        # the option exist and it was successfully parsed
        kinds = List.wrap Keyword.get(switches, option)
        new_opts = do_store_option(opts, option, value, kinds)
        do_parse(rest, config, new_opts, args, invalid, all?)

      {:invalid, option, value, rest} ->
        # the option exist but it has wrong value
        do_parse(rest, config, opts, args, [{option, value}|invalid], all?)

      {:undefined, option, _value, rest} ->
        # the option does not exist (for strict cases)
        do_parse(rest, config, opts, args, [{option, nil}|invalid], all?)

      {:error, ["--"|rest]} ->
        {Enum.reverse(opts), Enum.reverse(args, rest), Enum.reverse(invalid)}

      {:error, [arg|rest]=remaining_args} ->
        # there is no option
        if all? do
          do_parse(rest, config, opts, [arg|args], invalid, all?)
        else
          {Enum.reverse(opts), Enum.reverse(args, remaining_args), Enum.reverse(invalid)}
        end
    end
  end

  @doc """
  Low-level function that parses one option.

  It accepts the same options as `parse/2` and `parse_head/2`
  as both functions are built on top of next. This function
  may return:

    * `{:ok, key, value, rest}` - the option `key` with `value` was
      successfully parsed

    * `{:invalid, key, value, rest}` - the option `key` is invalid with `value`
      (returned when the switch type does not match the one given via the
      command line)

    * `{:undefined, key, value, rest}` - the option `key` is undefined
      (returned in strict mode when the switch is unknown)

    * `{:error, rest}` - there are no switches at the top of the given argv
  """

  @spec next(argv, options) ::
        {:ok, key :: atom, value :: term, argv} |
        {:invalid, String.t, String.t | nil, argv} |
        {:undefined, String.t, String.t | nil, argv} |
        {:error, argv}

  def next(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    {aliases, switches, strict} = compile_config(opts)
    next(argv, aliases, switches, strict)
  end

  defp next([], _aliases, _switches, _strict) do
    {:error, []}
  end

  defp next(["--"|_]=argv, _aliases, _switches, _strict) do
    {:error, argv}
  end

  defp next(["-"|_]=argv, _aliases, _switches, _strict) do
    {:error, argv}
  end

  defp next(["- " <> _|_]=argv, _aliases, _switches, _strict) do
    {:error, argv}
  end

  defp next(["-" <> option|rest], aliases, switches, strict) do
    {option, value} = split_option(option)
    opt_name_bin = "-" <> option
    tagged = tag_option(option, switches, aliases)

    if strict and not option_defined?(tagged, switches) do
      {:undefined, opt_name_bin, value, rest}
    else
      {opt_name, kinds, value} = normalize_option(tagged, value, switches)
      {value, kinds, rest} = normalize_value(value, kinds, rest, strict)
      case validate_option(value, kinds) do
        {:ok, new_value} -> {:ok, opt_name, new_value, rest}
        :invalid         -> {:invalid, opt_name_bin, value, rest}
      end
    end
  end

  defp next(argv, _aliases, _switches, _strict) do
    {:error, argv}
  end

  @doc """
  Receives a key-value enumerable and converts it to argv.

  Keys must be atoms. Keys with nil value are discarded,
  boolean values are converted to `--key` or `--no-key`
  and all other values are converted using `to_string/1`.

  ## Examples

      iex>  OptionParser.to_argv([foo_bar: "baz"])
      ["--foo-bar", "baz"]

      iex>  OptionParser.to_argv([bool: true, bool: false, discarded: nil])
      ["--bool", "--no-bool"]

  """
  @spec to_argv(Enumerable.t) :: argv
  def to_argv(enum) do
    Enum.flat_map(enum, fn
      {_key, nil}  -> []
      {key, true}  -> [to_switch(key)]
      {key, false} -> [to_switch(key, "--no-")]
      {key, value} -> [to_switch(key), to_string(value)]
    end)
  end

  defp to_switch(key, prefix \\ "--") when is_atom(key) do
    prefix <> String.replace(Atom.to_string(key), "_", "-")
  end

  @doc ~S"""
  Splits a string into argv chunks.

  ## Examples

      iex> OptionParser.split("foo bar")
      ["foo", "bar"]

      iex> OptionParser.split("foo \"bar baz\"")
      ["foo", "bar baz"]
  """
  @spec split(String.t) :: argv
  def split(string) do
    do_split(strip_leading_spaces(string), "", [], nil)
  end

  # If we have an escaped quote, simply remove the escape
  defp do_split(<<?\\, quote, t::binary>>, buffer, acc, quote),
    do: do_split(t, <<buffer::binary, quote>>, acc, quote)

  # If we have a quote and we were not in a quote, start one
  defp do_split(<<quote, t::binary>>, buffer, acc, nil) when quote in [?", ?'],
    do: do_split(t, buffer, acc, quote)

  # If we have a quote and we were inside it, close it
  defp do_split(<<quote, t::binary>>, buffer, acc, quote),
    do: do_split(t, buffer, acc, nil)

  # If we have an escaped quote/space, simply remove the escape as long as we are not inside a quote
  defp do_split(<<?\\, h, t::binary>>, buffer, acc, nil) when h in [?\s, ?', ?"],
    do: do_split(t, <<buffer::binary, h>>, acc, nil)

  # If we have space and we are outside of a quote, start new segment
  defp do_split(<<?\s, t::binary>>, buffer, acc, nil),
    do: do_split(strip_leading_spaces(t), "", [buffer|acc], nil)

  # All other characters are moved to buffer
  defp do_split(<<h, t::binary>>, buffer, acc, quote) do
    do_split(t, <<buffer::binary, h>>, acc, quote)
  end

  # Finish the string expecting a nil marker
  defp do_split(<<>>, "", acc, nil),
    do: Enum.reverse(acc)

  defp do_split(<<>>, buffer, acc, nil),
    do: Enum.reverse([buffer|acc])

  # Otherwise raise
  defp do_split(<<>>, _, _acc, marker) do
    raise "argv string did not terminate properly, a #{<<marker>>} was opened but never closed"
  end

  defp strip_leading_spaces(" " <> t), do: strip_leading_spaces(t)
  defp strip_leading_spaces(t), do: t

  ## Helpers

  defp compile_config(opts) do
    aliases = opts[:aliases] || []

    {switches, strict} = cond do
      opts[:switches] && opts[:strict] ->
        raise ArgumentError, ":switches and :strict cannot be given together"
      s = opts[:switches] ->
        {s, false}
      s = opts[:strict] ->
        {s, true}
      true ->
        {[], false}
    end

    {aliases, switches, strict}
  end

  defp validate_option(value, kinds) do
    {is_invalid, value} = cond do
      :invalid in kinds ->
        {true, value}
      :boolean in kinds ->
        case value do
          t when t in [true, "true"] -> {nil, true}
          f when f in [false, "false"] -> {nil, false}
          _ -> {true, value}
        end
      :integer in kinds ->
        case Integer.parse(value) do
          {value, ""} -> {nil, value}
          _ -> {true, value}
        end
      :float in kinds ->
        case Float.parse(value) do
          {value, ""} -> {nil, value}
          _ -> {true, value}
        end
      true ->
        {nil, value}
    end

    if is_invalid do
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

  defp tag_option(<<?-, option::binary>>, switches, _aliases) do
    get_negated(option, switches)
  end

  defp tag_option(option, _switches, aliases) when is_binary(option) do
    opt = get_option(option)
    if alias = aliases[opt] do
      {:default, alias}
    else
      :unknown
    end
  end

  defp option_defined?(:unknown, _switches) do
    false
  end

  defp option_defined?({:negated, option}, switches) do
    Keyword.has_key?(switches, option)
  end

  defp option_defined?({:default, option}, switches) do
    Keyword.has_key?(switches, option)
  end

  defp normalize_option(:unknown, value, _switches) do
    {nil, [:invalid], value}
  end

  defp normalize_option({:negated, option}, value, switches) do
    if value do
      {option, [:invalid], value}
    else
      {option, List.wrap(switches[option]), false}
    end
  end

  defp normalize_option({:default, option}, value, switches) do
    {option, List.wrap(switches[option]), value}
  end

  defp normalize_value(nil, kinds, t, strict) do
    nil_or_true = if strict, do: nil, else: true
    cond do
      :boolean in kinds ->
        {true, kinds, t}
      value_in_tail?(t) ->
        [h|t] = t
        {h, kinds, t}
      kinds == [] ->
        {nil_or_true, kinds, t}
      true ->
        {nil, [:invalid], t}
    end
  end

  defp normalize_value(value, kinds, t, _) do
    {value, kinds, t}
  end

  defp value_in_tail?(["-"|_]),       do: true
  defp value_in_tail?(["- " <> _|_]), do: true
  defp value_in_tail?(["-" <> _|_]),  do: false
  defp value_in_tail?([]),            do: false
  defp value_in_tail?(_),             do: true

  defp split_option(option) do
    case :binary.split(option, "=") do
      [h]    -> {h, nil}
      [h, t] -> {h, t}
    end
  end

  defp to_underscore(option), do: to_underscore(option, <<>>)

  defp to_underscore("_" <> _rest, _acc), do: nil

  defp to_underscore("-" <> rest, acc),
    do: to_underscore(rest, acc <> "_")

  defp to_underscore(<<c>> <> rest, acc),
    do: to_underscore(rest, <<acc::binary, c>>)

  defp to_underscore(<<>>, acc), do: acc

  defp get_option(option) do
    if str = to_underscore(option) do
      String.to_atom(str)
    end
  end

  defp get_negated("no-" <> rest = original, switches) do
    cond do
      (negated = get_option(rest)) && :boolean in List.wrap(switches[negated]) ->
        {:negated, negated}
      option = get_option(original) ->
        {:default, option}
      true ->
        :unknown
    end
  end

  defp get_negated(rest, _switches) do
    if option = get_option(rest) do
      {:default, option}
    else
      :unknown
    end
  end
end
