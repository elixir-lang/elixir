defmodule Mix.Task.Compiler do
  @moduledoc """
  This module defines the behaviour for a Mix task that does compilation.

  A Mix compiler task can be defined by simply using `Mix.Task.Compiler`
  in a module whose name starts with `Mix.Tasks.Compile.` and defining
  the `run/1` function:

      defmodule Mix.Tasks.Compile.MyLanguage do
        use Mix.Task.Compiler

        def run(_args) do
          :ok
        end
      end

  The `run/1` function returns an atom indicating the status of the
  compilation, and optionally can also return a list of "diagnostics"
  such as warnings or compilation errors. Doing this enables code
  editors to display issues inline without having to analyze the
  command-line output.

  If the compiler uses manifest files to track stale sources, it should
  define `manifests/0`, and if it writes any output to disk it should
  also define `clean/0`.

  A compiler supports the same attributes for configuration and
  documentation as a regular Mix task. See `Mix.Task` for more information.
  """

  defmodule Diagnostic do
    @moduledoc """
    Diagnostic information such as a warning or compilation error.
    """

    @type t :: %__MODULE__{
            file: Path.t(),
            severity: severity,
            message: String.t(),
            position: position,
            compiler_name: String.t(),
            details: any
          }

    @typedoc """
    Severity of a diagnostic:

      * `:error` - An issue that caused compilation to fail

      * `:warning` - An issue that did not cause failure but suggests the
        programmer may have made a mistake

      * `:hint` - A suggestion for style or good practices that is not as
        severe as a warning

      * `:information` - Any other information relevant to compilation that
        does not fit into the above categories

    """
    @type severity :: :error | :warning | :information | :hint

    @typedoc """
    Where in a file the diagnostic applies. Can be either a line number,
    a range specified as `{start_line, start_col, end_line, end_col}`,
    or `nil` if unknown.

    Line numbers are one-based, and column numbers in a range are zero-based and refer
    to the cursor position at the start of the character at that index. For example,
    to indicate that a diagnostic applies to the first `n` characters of the
    first line, the range would be `{1, 0, 1, n}`.
    """
    @type position ::
            nil
            | pos_integer
            | {pos_integer, non_neg_integer, pos_integer, non_neg_integer}

    @enforce_keys [:file, :severity, :message, :position, :compiler_name]
    defstruct [:file, :severity, :message, :position, :compiler_name, :details]
  end

  @doc """
  Receives command-line arguments and performs compilation. If it
  produces errors, warnings, or any other diagnostic information,
  it should return a tuple with the status and a list of diagnostics.
  """
  @callback run([binary]) :: {:ok | :noop | :error, [Diagnostic.t()]}

  @doc """
  Lists manifest files for the compiler.
  """
  @callback manifests() :: [Path.t()]

  @doc """
  Removes build artifacts and manifests.
  """
  @callback clean() :: any

  @optional_callbacks clean: 0, manifests: 0

  @doc false
  defmacro __using__(_opts) do
    quote do
      Enum.each(
        Mix.Task.supported_attributes(),
        &Module.register_attribute(__MODULE__, &1, persist: true)
      )

      @behaviour Mix.Task.Compiler
    end
  end

  # Normalize the compiler result to a diagnostic tuple.
  # TODO: Deprecate :ok and :noop on v1.9
  @doc false
  def normalize(result, name) do
    case result do
      {status, diagnostics} when status in [:ok, :noop, :error] and is_list(diagnostics) ->
        {status, diagnostics}

      _ when result in [:ok, :noop] ->
        {result, []}

      _ ->
        Mix.shell().error(
          "[warning] Mix compiler #{inspect(name)} was supposed to return " <>
            "{:ok | :noop | :error, [diagnostic]} but it returned #{inspect(result)}"
        )

        {:noop, []}
    end
  end
end
