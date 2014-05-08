defmodule IEx.Options do
  @moduledoc false

  @supported_options ~w(colors inspect history_size prompt)a

  @doc """
  Returns all supported IEx options with their respective values as a keyword
  list.
  """
  def get do
    IO.write :stderr, "IEx.Options.get/0 is deprecated, please use IEx.configuration/0\n#{Exception.format_stacktrace}"
    Enum.map list(), fn name ->
      {name, get(name)}
    end
  end

  @doc """
  Get current value of the option `name`. Raises `ArgumentError` if `name` is not a
  known option.
  """
  def get(name)

  for key <- @supported_options do
    def get(unquote(key)) do
      IO.write :stderr, "IEx.Options.get/1 is deprecated, please use IEx.configuration/0\n#{Exception.format_stacktrace}"
      {:ok, value} = Application.fetch_env(:iex, unquote(key))
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
    IO.write :stderr, "IEx.Options.set/1 is deprecated, please use IEx.configure/1\n#{Exception.format_stacktrace}"
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
    IO.write :stderr, "IEx.Options.set/2 is deprecated, please use IEx.configure/1\n#{Exception.format_stacktrace}"
    filter_and_merge(:colors, colors)
  end

  def set(:colors, _) do
    raise_value("a keyword list")
  end

  def set(:inspect, opts) when is_list(opts) do
    IO.write :stderr, "IEx.Options.set/2 is deprecated, please use IEx.configure/1\n#{Exception.format_stacktrace}"
    filter_and_merge(:inspect, opts)
  end

  def set(:inspect, _) do
    raise_value("a keyword list")
  end

  def set(:history_size, size) when is_integer(size) do
    IO.write :stderr, "IEx.Options.set/2 is deprecated, please use IEx.configure/1\n#{Exception.format_stacktrace}"
    old_size = get(:history_size)
    Application.put_env(:iex, :history_size, size)
    old_size
  end

  def set(:history_size, _) do
    raise_value("an integer")
  end

  def set(:prompt, prompts) when is_list(prompts) do
    IO.write :stderr, "IEx.Options.set/2 is deprecated, please use IEx.configure/1\n#{Exception.format_stacktrace}"
    filter_and_merge(:prompt, prompts)
  end

  def set(:prompt, _) do
    raise_value("a keyword list")
  end

  def set(name, _) do
    raise_option(name)
  end

  def help(:colors), do: """
  This is an aggregate option that encapsulates all color settings used by the
  shell. See documentation for the `IO.ANSI` module for the list of supported
  colors and attributes.

  The value is a keyword list. List of supported keys:

    * `:enabled`      - boolean value that allows for switching the coloring on and off
    * `:eval_result`  - color for an expression's resulting value
    * `:eval_info`    - … various informational messages
    * `:eval_error`   - … error messages
    * `:stack_app`    - … the app in stack traces
    * `:stack_info`   - … the remaining info in stacktraces
    * `:ls_directory` - … for directory entries (ls helper)
    * `:ls_device`    - … device entries (ls helper)

  When printing documentation, IEx will convert the markdown
  documentation to ANSI as well. Those can be configured via:

    * `:doc_code`        — the attributes for code blocks (cyan, bright)
    * `:doc_inline_code` - inline code (cyan)
    * `:doc_headings`    - h1 and h2 (yellow, bright)
    * `:doc_title`       — the overall heading for the output (reverse,yellow,bright)
    * `:doc_bold`        - (bright)
    * `:doc_underline`   - (underline)

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

  def help(:prompt), do: """
  This is an option determining the prompt displayed to the user
  when awaiting input.

  The value is a keyword list. Two prompt types:

  * `:default` - used when `Node.alive?` returns false
  * `:alive`   - used when `Node.alive?` returns true

  The part of the listed in the following of the prompt string is replaced.

  * `%counter` - the index of the history
  * `%prefix`  - a prefix given by `IEx.Server`
  * `%node`    - the name of the local node
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
    IO.write :stderr, "IEx.Options is deprecated\n#{Exception.format_stacktrace}"
    IO.ANSI.Docs.print help(name)
  end

  @doc """
  Returns all supported options as a list of names.
  """
  def list() do
    IO.write :stderr, "IEx.Options is deprecated\n#{Exception.format_stacktrace}"
    @supported_options
  end

  defp raise_option(name) do
    raise ArgumentError, message: "Unknown IEx option #{inspect name}"
  end

  defp raise_value(type) do
    raise ArgumentError, message: "Expected the value to be #{type}"
  end

  defp filter_and_merge(opt, values) when is_list(values) do
    old_values = get(opt)
    :application.set_env(:iex, opt, Keyword.merge(old_values, values))
    old_values
  end
end
