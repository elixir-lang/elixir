defmodule Mix.Task.Compiler do
  @moduledoc """
  This module defines the behaviour for a Mix task that does compilation.

  A Mix compiler task can be defined by simply using `Mix.Task.Compiler`
  in a module whose name starts with `Mix.Tasks.Compile.` and defining
  the [`run/1`](`c:run/1`) function:

      defmodule Mix.Tasks.Compile.MyLanguage do
        use Mix.Task.Compiler

        def run(_args) do
          :ok
        end
      end

  The [`run/1`](`c:run/1`) function returns an atom indicating the status of the
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

    The file and position relate to where the diagnostic should be shown.
    If there is a file and position, then the diagnostic is precise
    and you can use the given file and position for generating snippets,
    IDEs annotations, and so on. An optional span is available with
    the line and column the diagnostic ends.

    Otherwise, a stacktrace may be given, which you can place your own
    heuristics to provide better reporting.

    The source field points to the source file the compiler tracked
    the error to. For example, a file `lib/foo.ex` may embed `.eex`
    templates from `lib/foo/bar.eex`. A syntax error on the EEx template
    will point to file `lib/foo/bar.eex` but the source is `lib/foo.ex`.
    """

    @type t :: %__MODULE__{
            file: Path.t() | nil,
            source: Path.t() | nil,
            severity: severity,
            message: IO.chardata(),
            position: Code.position(),
            compiler_name: String.t(),
            details: term(),
            stacktrace: Exception.stacktrace(),
            span: {line :: pos_integer(), column :: pos_integer()} | nil
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

    @enforce_keys [:file, :severity, :message, :position, :compiler_name]
    defstruct [
      :file,
      :source,
      :severity,
      :message,
      :position,
      :compiler_name,
      span: nil,
      details: nil,
      stacktrace: []
    ]
  end

  @type status :: :ok | :noop | :error

  @doc """
  Receives command-line arguments and performs compilation. If it
  produces errors, warnings, or any other diagnostic information,
  it should return a tuple with the status and a list of diagnostics.
  """
  @callback run([binary]) :: status | {status, [Diagnostic.t()]}

  @doc """
  Lists manifest files for the compiler.
  """
  @callback manifests() :: [Path.t()]

  @doc """
  Removes build artifacts and manifests.
  """
  @callback clean() :: any

  @optional_callbacks clean: 0, manifests: 0

  @doc """
  Adds a callback that runs after a given compiler.

  The callback is invoked after the compiler runs and
  it receives a tuple with current status and the list
  of diagnostic. It must return the updated status and
  diagnostics.

  If the given compiler does not run (for instance,
  because an earlier compiler in the stack has aborted),
  the callback will not be executed.
  """
  @doc since: "1.10.0"
  @spec after_compiler(atom, ({status, [Diagnostic.t()]} -> {status, [Diagnostic.t()]})) :: :ok
  def after_compiler(name, fun) when is_atom(name) and is_function(fun, 1) do
    Mix.ProjectStack.prepend_after_compiler(name, fun)
  end

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
  @doc false
  def normalize(result, name) do
    case result do
      {status, diagnostics} when status in [:ok, :noop, :error] and is_list(diagnostics) ->
        {status, diagnostics}

      # ok/noop can come from tasks that have already run
      _ when result in [:ok, :noop] ->
        {result, []}

      _ ->
        # TODO: Convert this to an error on v2.0
        Mix.shell().error(
          "warning: Mix compiler #{inspect(name)} was supposed to return " <>
            "{:ok | :noop | :error, [diagnostic]} but it returned #{inspect(result)}"
        )

        {:noop, []}
    end
  end
end
