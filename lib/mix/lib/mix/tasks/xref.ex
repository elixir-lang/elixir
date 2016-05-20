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
  """

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :error
  def run(_args) do
    Enum.reduce manifests(), :ok, fn manifest, result ->
      {_, sources} = parse_manifest(manifest)

      Enum.reduce sources, result, fn source, result ->
        source(runtime_dispatches: runtime_dispatches, source: source) = source
        warn_for_missing_remote_functions(runtime_dispatches, source, result)
      end
    end
  end

  defp warn_for_missing_remote_functions(runtime_dispatches, source, result) do
    Enum.sort_by(runtime_dispatches, fn {_, _, line} -> line end)
    |> Enum.reduce(result, &reduce_dispatch(source, &1, &2))
  end

  defp reduce_dispatch(source, {module, {func, arity}, line}, result) do
    if Code.ensure_loaded?(module) do
      if function_exported?(module, func, arity) or is_erlang_op?(module, func, arity) do
        result
      else
        IO.warn(
          "Remote function #{inspect module}.#{func}/#{arity} cannot be found\n" <>
          "  #{source}:#{line}",
          []
        )

        :error
      end
    else
      if builtin_protocol_impl?(module, func, arity) do
        result
      else
        IO.warn(
          "Module #{inspect module} cannot be found\n" <>
          "  In remote call to #{inspect module}.#{func}/#{arity} at:\n" <>
          "    #{source}:#{line}",
          []
        )

        :error
      end
    end
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

    maybe_builtin in @protocol_builtins and function_exported?(maybe_protocol, :__protocol__, 1)
  end
  defp builtin_protocol_impl?(_, _, _),
    do: false
end
