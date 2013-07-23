defmodule IEx.Options do
  @moduledoc """
  Provides an interface for adjusting options of the running IEx session.

  Changing options is usually done inside an IEx session or in your .iex file.
  See `h(IEx)` for more info on the latter.

  If the value of an option is a keyword list, only those keys that are
  mentioned will be changed. The rest of the sub-options will keep their
  current values. Any extraneous keys are filtered out, i.e. not used.

  To get the list of all supported options, use `list/0`. You can also get an
  option's description using `print_help/1`.

  ## Examples

      iex(1)> ArgumentError[]
      ArgumentError[message: "argument error"]

      iex(2)> IEx.Options.set :inspect, raw: true
      [limit: 50, raw: false]

      iex(3)> ArgumentError[]
      {ArgumentError,:__exception__,"argument error"}

      iex(4)> IEx.Options.list
      [:colors,:inspect]

      iex(5)> IEx.Options.print_help :colors
      This is an aggregate option that encapsulates all color settings used
      by the shell.
      ... # omitted content

  """

  @supported_options %w(colors inspect history_size)a

  @doc """
  Returns all supported IEx options with their respective values as a keyword
  list.
  """
  def get do
    Enum.map list(), fn name ->
      {name, get(name)}
    end
  end

  @doc """
  Get current value of the option `name`. Raises `ArgumentError` if `name` is not a
  known option.
  """
  def get(name)

  keys = [ colors: :colors,
           inspect: :inspect_opts,
           history_size: :history_size ]

  Enum.each keys, fn { key, env } ->
    def get(unquote(key)) do
      { :ok, value } = :application.get_env(:iex, unquote(env))
      value
    end
  end

  def get(name) do
    raise_option(name)
  end

  @doc """
  Set all options at once by providing a keyword list with option names and
  their corresponding values. This is generally obtained from `get/0`.

  Returns a keyword list of old option values.
  """
  def set(opts) do
    Enum.map opts, fn {name, val} ->
      {name, set(name, val)}
    end
  end

  @doc """
  Sets the value for the option `name` to `value`.

  Returns the option's previous value in the case of success.

  If `name` is not a known option or if `value` is invalid, raises
  `ArgumentError`.
  """
  def set(name, value)

  def set(:colors, colors) when is_list(colors) do
    filter_and_merge(:colors, colors)
  end

  def set(:colors, _) do
    raise_value("a keyword list")
  end

  def set(:inspect, opts) when is_list(opts) do
    filter_and_merge(:inspect, :inspect_opts, opts)
  end

  def set(:inspect, _) do
    raise_value("a keyword list")
  end

  def set(:history_size, size) when is_integer(size) do
    old_size = get(:history_size)
    :application.set_env(:iex, :history_size, size)
    old_size
  end

  def set(:history_size, _) do
    raise_value("an integer")
  end

  def set(name, _) do
    raise_option(name)
  end

  def help(:colors), do: """
  This is an aggregate option that encapsulates all color settings used by the
  shell. See documentation for the `IO.ANSI` module for the list of supported
  colors and attributes.

  The value is a keyword list. List of supported keys:

    * enabled     -- boolean value that allows for switching the coloring
                     on and off
    * eval_result -- color for an expression's resulting value
    * error       -- color for error messages
    * info        -- color for various informational messages
    * directory   -- color for directory entries (ls helper)
    * device      -- color for device entries (ls helper)
  """

  def help(:inspect), do: """
  Inspect options used by the shell when printing results of expression
  evaluation.

  The value is a keyword list.

  See `Kernel.inspect/2` for the full list of options.
  """

  def help(:history_size), do: """
  Number of expressions and their results to keep in the history.

  The value is an integer. When it is negative, the history is unlimited.
  """

  @doc """
  Returns a string with the option's description. Raises `ArgumentError`
  if `name` is not a known option.
  """
  def help(name) do
    raise_option(name)
  end

  @doc """
  Same as `help/1` but instead of returning a string, prints it.
  """
  def print_help(name) do
    IO.write help(name)
  end

  @doc """
  Returns all supported options as a list of names.
  """
  def list() do
    @supported_options
  end

  defp raise_option(name) do
    raise ArgumentError, message: "Unknown IEx option #{inspect name}"
  end

  defp raise_value(type) do
    raise ArgumentError, message: "Expected the value to be #{type}"
  end

  defp raise_key(option_name, name) do
    raise ArgumentError, message: "Unsupported key '#{name}' for option '#{option_name}'"
  end

  defp filter_and_merge(option_name, env_var_name // nil, values) when is_list(values) do
    env_var_name = env_var_name || option_name

    old_values = get(option_name)
    filtered_values = filtered_kw(option_name, old_values, values)
    :application.set_env(:iex, env_var_name, Keyword.merge(old_values, filtered_values))
    old_values
  end

  defp filtered_kw(option_name, reference_kw, user_kw) do
    Enum.filter user_kw, fn {name, _} ->
      if not Keyword.has_key?(reference_kw, name) do
        raise_key(option_name, name)
      end
      true
    end
  end
end
