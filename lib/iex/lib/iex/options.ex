defmodule IEx.Options do
  @moduledoc """
  Provides an interface for changing options of the running IEx session.
  """

  @supported_options [
    colors: [
      doc: """
        This is an aggregate option that encapsulates all color settings used
        by the shell.

        The value is a keyword list that should have any of the following keys
        specified. If any of the keys is omitted, that color is not changed.

          * enabled     -- boolean value that allows to switch the colors
                           on and off
          * eval_result -- color for an expression's resulting value
          * error       -- color for error messages
          * info        -- color for various informational messages

        """
    ],
    inspect: [
      doc: ""
    ],
  ]

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
    IEx.inspect_opts
  end

  def get(name) do
    raise ArgumentError, message: "Unknown IEx option #{inspect name}"
  end

  @doc """
  Sets the value of the option `name` to `value`.

  Returns option's previous value if in the case of success.

  Raises if name is not a known option or if the value is invalid.
  """
  def set(name, value)

  def set(:colors, colors) when is_list(colors) do
    { :ok, old_colors } = :application.get_env(:iex, :colors)
    # FIXME: validate keys before setting
    :application.set_env(:iex, :colors, Keyword.merge(old_colors, colors))
    old_colors
  end

  def set(:colors, _) do
    raise ArgumentError, message: "Expected colors to be a keyword list"
  end

  def set(:inspect, opts) when is_list(opts) do
    old_opts = IEx.inspect_opts
    IEx.inspect_opts(opts)
    old_opts
  end

  def set(:inspect, _) do
    raise ArgumentError, message: "Expected opts to be a keyword list"
  end

  def set(name, _) do
    raise ArgumentError, message: "Unknown IEx option #{inspect name}"
  end

  @doc """
  Returns a string with the option's description.
  """
  def help(name) do
    case @supported_options[name] do
      kv when is_list(kv) -> kv[:doc]
      nil -> :bad_option
    end
  end

  @doc """
  Same as `help/1` but instead of returning a string, prints it to the console.
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
end
