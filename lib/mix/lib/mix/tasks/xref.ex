defmodule Mix.Tasks.Xref do
  use Mix.Task

  import Mix.Tasks.Compile.Elixir, only: [manifests: 0]
  import Mix.Compilers.Elixir, only: [parse_manifest: 1, source: 1]

  @shortdoc "Performs remote dispatch checking"
  @recursive true

  @moduledoc """
  Performs remote dispatch checking.

  When this task runs, it looks for all runtime remote dispatches in the
  application. It will then check that all of the modules/functions referred to
  by the dispatches are available. If any are not, a warning will be printed.

  ## Command line options

    * `--no-compile` - do not compile even if files require compilation

  """

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :error
  def run(args) do
    {opts, _, _} =
      OptionParser.parse(args, switches: [no_compile: :boolean])

    unless opts[:no_compile], do: Mix.Task.run("compile")

    Enum.reduce manifests(), :ok, fn manifest, result ->
      {_, sources} = parse_manifest(manifest)

      Enum.reduce sources, result, fn source, result ->
        source(runtime_dispatches: runtime_dispatches, source: source) = source
        warn_for_missing_remote_functions(runtime_dispatches, source, result)
      end
    end
  end

  defp warn_for_missing_remote_functions(runtime_dispatches, source, result) do
    warnings =
      Enum.flat_map runtime_dispatches, fn {module, func_arity_lines} ->
        Enum.flat_map func_arity_lines, fn {{func, arity}, lines} ->
          for line <- lines, warning = warning(module, func, arity, line),
            do: warning
        end
      end

    if warnings == [] do
      result
    else
      print_warnings(source, warnings)

      :error
    end
  end

  defp warning(module, func, arity, line) do
    if Code.ensure_loaded?(module) do
      unless function_exported?(module, func, arity) or is_erlang_op?(module, func, arity) do
        {line, :unknown_function, module, func, arity}
      end
    else
      unless builtin_protocol_impl?(module, func, arity) do
        {line, :unknown_module, module, func, arity}
      end
    end
  end

  defp print_warnings(source, warnings) do
    Enum.sort(warnings)
    |> Enum.each(&print_warning(source, &1))
  end

  defp print_warning(source, {line, :unknown_function, module, func, arity}) do
    message =
      """
      Remote function #{inspect module}.#{func}/#{arity} cannot be found
        #{source}:#{line}
      """

    :elixir_errors.warn([message])
  end

  defp print_warning(source, {line, :unknown_module, module, func, arity}) do
    message =
      """
      Module #{inspect module} cannot be found
        In remote call to #{inspect module}.#{func}/#{arity} at:
          #{source}:#{line}
      """

    :elixir_errors.warn([message])
  end

  defp is_erlang_op?(:erlang, func, 2) when func in [:andalso, :orelse],
    do: true
  defp is_erlang_op?(_, _, _),
    do: false

  @protocol_builtins for {_, type} <- Protocol.__builtin__(), do: type

  defp builtin_protocol_impl?(module, :__impl__, 1) do
    {maybe_protocol, maybe_builtin} = module |> Module.split() |> Enum.split(-1)
    maybe_protocol = Module.concat(maybe_protocol)
    maybe_builtin = Module.concat(maybe_builtin)

    Code.ensure_loaded?(maybe_protocol) and
    maybe_builtin in @protocol_builtins and
    function_exported?(maybe_protocol, :__protocol__, 1)
  end
  defp builtin_protocol_impl?(_, _, _),
    do: false
end
