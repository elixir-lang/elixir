defmodule Mix.Tasks.Xref do
  use Mix.Task

  alias Mix.Tasks.Compile.Elixir, as: E
  import Mix.Compilers.Elixir, only: [parse_manifest: 1, source: 1]

  @shortdoc "Performs cross reference checks"
  @recursive true

  @moduledoc """
  Performs cross reference checks between modules.

  ## Xref modes

  The following options control the information xref can emit.

    * `--warnings` - prints warnings for violated cross reference checks
    * `--unreachable` - prints all unreachable "file:line: module.function/arity" entries

  ## Command line options

    * `--no-compile` - do not compile even if files require compilation
    * `--no-deps-check` - do not check dependencies
    * `--no-archives-check` - do not check archives
    * `--no-elixir-version-check` - do not check the Elixir version from mix.exs

  """

  @switches [compile: :boolean, deps_check: :boolean, archives_check: :boolean,
             warnings: :boolean, unreachable: :boolean, elixir_version_check: :boolean]

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :error
  def run(args) do
    {opts, _} =
      OptionParser.parse!(args, strict: @switches)

    if Keyword.get(opts, :compile, true) do
      Mix.Task.run("compile")
    end

    cond do
      opts[:warnings] ->
        warnings()
      opts[:unreachable] ->
        unreachable()
      true ->
        Mix.raise "xref expects one of the following flags: --warnings, --unreachable"
    end
  end

  ## Modes

  defp warnings() do
    if unreachable(&print_warnings/2) == [] do
      :ok
    else
      :error
    end
  end

  defp unreachable() do
    if unreachable(&print_entry/2) == [] do
      :ok
    else
      :error
    end
  end

  ## Unreachable

  defp unreachable(pair) do
    for manifest <- E.manifests(),
        {_, sources} = parse_manifest(manifest),
        source(source: file) = source <- sources,
        entries = unreachable_source(source),
        entries != [],
        do: pair.(file, entries)
  end

  defp unreachable_source(source) do
    source(runtime_dispatches: runtime_dispatches) = source

    for {module, func_arity_lines} <- runtime_dispatches,
        {{func, arity}, lines} <- func_arity_lines,
        warning = unreachable_mfa(module, func, arity, lines),
        do: warning
  end

  defp unreachable_mfa(module, func, arity, lines) do
    cond do
      skip?(module, func, arity) ->
        nil
      not Code.ensure_loaded?(module) ->
        {Enum.sort(lines), :unknown_module, module, func, arity}
      not function_exported?(module, func, arity) ->
        {Enum.sort(lines), :unknown_function, module, func, arity}
      true ->
        nil
    end
  end

  ## Print entries

  defp print_entry(file, entries) do
    entries
    |> Enum.sort()
    |> Enum.each(&IO.write(format_entry(file, &1)))
  end

  defp format_entry(file, {lines, _, module, function, arity}) do
    for line <- lines do
      [Exception.format_file_line(file, line), ?\s, Exception.format_mfa(module, function, arity), ?\n]
    end
  end

  ## Print warnings

  defp print_warnings(file, entries) do
    prefix = IO.ANSI.format([:yellow, "warning: "])
    entries
    |> Enum.sort
    |> Enum.each(&IO.write(:stderr, [prefix, format_warning(file, &1), ?\n]))
  end

  defp format_warning(file, {lines, :unknown_function, module, function, arity}) do
    ["function ", Exception.format_mfa(module, function, arity),
      " is undefined or private\n" | format_file_lines(file, lines)]
  end

  defp format_warning(file, {lines, :unknown_module, module, function, arity}) do
    ["function ", Exception.format_mfa(module, function, arity),
     " is undefined (module #{inspect module} is not available)\n" | format_file_lines(file, lines)]
  end

  defp format_file_lines(file, [line]) do
    format_file_line(file, line)
  end

  defp format_file_lines(file, lines) do
    ["\nFound at #{length(lines)} locations:\n" |
     Enum.map(lines, &format_file_line(file, &1))]
  end

  defp format_file_line(file, line) do
    ["  ", file, ?:, Integer.to_string(line), ?\n]
  end

  ## Helpers

  @protocol_builtins for {_, type} <- Protocol.__builtin__(), do: type

  defp skip?(:erlang, func, 2) when func in [:andalso, :orelse] do
    true
  end

  defp skip?(module, :__impl__, 1) do
    {maybe_protocol, maybe_builtin} = module |> Module.split() |> Enum.split(-1)
    maybe_protocol = Module.concat(maybe_protocol)
    maybe_builtin = Module.concat(maybe_builtin)

    maybe_builtin in @protocol_builtins
      and Code.ensure_loaded?(maybe_protocol)
      and function_exported?(maybe_protocol, :__protocol__, 1)
  end

  defp skip?(_, _, _) do
    false
  end
end
