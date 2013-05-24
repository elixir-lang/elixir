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

  @supported_options [
    colors: [],
    inspect: [],
  ]

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
  Get current value of the option `name`. Raises ArgumentError if name is not a
  known option.
  """
  def get(name)

  def get(:colors) do
    { :ok, colors } = :application.get_env(:iex, :colors)
    colors
  end

  def get(:inspect) do
    { :ok, opts } = :application.get_env(:iex, :inspect_opts)
    opts
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

  Returns option's previous value in the case of success.

  Raises if `name` is not a known option or if the value is invalid.
  """
  def set(name, value)

  def set(:colors, colors) when is_list(colors) do
    old_colors = get(:colors)
    filtered_colors = filtered_kw(old_colors, colors)
    :application.set_env(:iex, :colors, Keyword.merge(old_colors, filtered_colors))
    old_colors
  end

  def set(:colors, _) do
    raise_value
  end

  def set(:inspect, opts) when is_list(opts) do
    old_opts = get(:inspect)
    filtered_opts = filtered_kw(old_opts, opts)
    :application.set_env(:iex, :inspect_opts, Keyword.merge(old_opts, filtered_opts))
    old_opts
  end

  def set(:inspect, _) do
    raise_value
  end

  def set(name, _) do
    raise_option(name)
  end

  @doc """
  Returns a string with the option's description. Raises if `name` is not a
  known option.
  """
  def help(name) do
    case @supported_options[name] do
      kv when is_list(kv) ->
        docs = __MODULE__.__info__(:docs)
        { {_, _}, _, _, _, doc } = Enum.find docs, fn { {f, _}, _, _, _, _ } ->
          f == name
        end

        # Strip the first paragraph
        String.lstrip(Regex.replace %r/\A.+?^$/ms, doc, "")

      nil -> raise_option(name)
    end
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
    Enum.map @supported_options, fn {k, _} -> k end
  end

  @doc """
  **NOTE**: This is just a stub for documentation purposes. Use
  `IEx.Options.get` and `IEx.Options.set` to query and change the option's
  value.

  This is an aggregate option that encapsulates all color settings used
  by the shell.

    * enabled     -- boolean value that allows for switching the coloring
                     on and off

    * eval_result -- color for an expression's resulting value
    * error       -- color for error messages
    * info        -- color for various informational messages
    * directory   -- color for directory entries (ls helper)
    * device      -- color for device entries (ls helper)

  """
  def colors

  @doc """
  **NOTE**: This is just a stub for documentation purposes. Use
  `IEx.Options.get` and `IEx.Options.set` to query and change the option's
  value.

  Control the behavior of the shell's inspecting algorithm. Inspect is
  used for printing results of expression evaluation. It is also used by
  IO.inspect.

  See the doc for `Kernel.inspect/2` for the full list of options.

  """
  def inspect

  defp raise_option(name) do
    raise ArgumentError, message: "Unknown IEx option #{inspect name}"
  end

  defp raise_value do
    raise ArgumentError, message: "Expected the value to be a keyword list"
  end

  defp filtered_kw(reference_kw, user_kw) do
    Enum.filter user_kw, fn {name, _} ->
      Keyword.has_key? reference_kw, name
    end
  end
end
