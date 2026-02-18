# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Compile.Erlang do
  use Mix.Task.Compiler
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest "compile.erlang"
  @switches [force: :boolean, verbose: :boolean, all_warnings: :boolean]

  @moduledoc """
  Compiles Erlang source files.

  When this task runs, it will first check the modification times of
  all files to be compiled and if they haven't been changed since the
  last compilation, it will not compile them. If any of them have changed,
  it compiles everything.

  ## Command line options

    * `--all-warnings` (`--no-all-warnings`) - prints all warnings, including previous compilations
      (default is true except on errors)
    * `--force` - forces compilation regardless of modification times
    * `--verbose` - prints verbose output

  ## Configuration

    * `ERL_COMPILER_OPTIONS` - can be used to give default compile options.
      The value must be a valid Erlang term. If the value is a list, it will
      be used as is. If it is not a list, it will be put into a list.

    * `:erlc_paths` - directories to find source files.
      Defaults to `["src"]`.

    * `:erlc_include_path` - directory for adding include files.
      Defaults to `"include"`.

    * `:erlc_options` - compilation options that apply to Erlang's
      compiler. Defaults to `[]`.

      For a complete list of options, see `:compile.file/2`.
      The option `:debug_info` is always added to the end of it.
      You can disable that using:

          erlc_options: [debug_info: false]

  """

  @impl true
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    project = Mix.Project.config()

    Mix.Project.with_build_lock(project, fn ->
      source_paths = project[:erlc_paths]
      Mix.Compilers.Erlang.assert_valid_erlc_paths(source_paths)
      files = Mix.Utils.extract_files(source_paths, [:erl])
      do_run(files, opts, project, source_paths)
    end)
  end

  defp do_run([], _, _, _), do: {:noop, []}

  defp do_run(files, opts, project, source_paths) do
    include_path = Erlang.to_erl_file(project[:erlc_include_path])
    compile_path = Erlang.to_erl_file(Mix.Project.compile_path(project))
    erlc_options = project[:erlc_options] || []

    if not is_list(erlc_options) do
      Mix.raise(":erlc_options should be a list of options, got: #{inspect(erlc_options)}")
    end

    erlc_options =
      erlc_options ++ [:debug_info, :return, :report, outdir: compile_path, i: include_path]

    erlc_options =
      Enum.map(erlc_options, fn
        {kind, dir} when kind in [:i, :outdir] -> {kind, Erlang.to_erl_file(dir)}
        opt -> opt
      end)

    compile_path = Path.relative_to(compile_path, File.cwd!())
    erls = scan_sources(files, include_path, source_paths, compile_path, opts)

    {sorted, parallel} = topsort_and_parallelize(erls, compile_path)
    opts = [parallel: MapSet.new(parallel)] ++ opts

    Erlang.compile_entries(manifest(), sorted, :erl, :beam, opts, fn input, _output ->
      # We're purging the module because a previous compiler (for example, Phoenix)
      # might have already loaded the previous version of it.
      module = input |> Path.basename(".erl") |> String.to_atom()
      :code.purge(module)
      :code.delete(module)

      path = Path.rootname(input, ".erl")
      file = Erlang.to_erl_file(path)

      case :compile.file(file, erlc_options) do
        :error ->
          message =
            "Compiling Erlang file #{inspect(path)} failed, probably because of invalid :erlc_options"

          Mix.raise(message)

        result ->
          result
      end
    end)
  end

  @impl true
  def manifests, do: [manifest()]
  defp manifest, do: Path.join(Mix.Project.manifest_path(), @manifest)

  @impl true
  def diagnostics do
    Mix.Compilers.Erlang.diagnostics(manifest())
  end

  @impl true
  def clean do
    Mix.Compilers.Erlang.clean(manifest())
  end

  @doc false
  def modules do
    for output <- Mix.Compilers.Erlang.outputs(manifest()) do
      output
      |> Path.basename()
      |> Path.rootname()
      |> String.to_atom()
    end
  end

  ## Internal helpers

  defp scan_sources(files, include_path, source_paths, compile_path, opts) do
    include_paths = [include_path | source_paths]

    files
    |> Task.async_stream(&scan_source(&1, include_paths, compile_path, opts),
      timeout: :infinity,
      ordered: false
    )
    |> Enum.flat_map(fn
      {:ok, {:ok, erl_file}} -> [erl_file]
      {:ok, :error} -> []
    end)
  end

  defp scan_source(file, include_paths, compile_path, opts) do
    erl_file = %{
      file: file,
      module: module_from_artifact(file),
      deps: [],
      includes: [],
      status: :ok
    }

    case :epp.parse_file(Erlang.to_erl_file(file), include_paths, []) do
      {:ok, forms} ->
        erl_file = List.foldl(tl(forms), erl_file, &do_form(file, &1, &2))
        {:ok, maybe_stale(erl_file, compile_path, opts[:force])}

      {:error, _error} ->
        :error
    end
  end

  defp do_form(file, form, erl) do
    case form do
      {:attribute, _, :file, {include_file, _}} when file != include_file ->
        if File.regular?(include_file) do
          %{erl | includes: [include_file | erl.includes]}
        else
          erl
        end

      {:attribute, _, :behaviour, behaviour} ->
        %{erl | deps: [behaviour | erl.deps]}

      {:attribute, _, :behavior, behaviour} ->
        %{erl | deps: [behaviour | erl.deps]}

      {:attribute, _, :compile, value} when is_list(value) ->
        %{erl | deps: Enum.reduce(value, erl.deps, &add_parse_transforms/2)}

      {:attribute, _, :compile, value} ->
        %{erl | deps: add_parse_transforms(value, erl.deps)}

      _ ->
        erl
    end
  end

  defp topsort_and_parallelize(erls, compile_path) do
    graph = :digraph.new()

    for %{module: module, status: status, file: file} <- erls do
      :digraph.add_vertex(graph, module, {file, status})
    end

    for %{module: module, deps: deps} <- erls, dep <- deps do
      # It may error if the behaviour/parse transform is not in the project,
      # which is expected
      :digraph.add_edge(graph, module, dep)
    end

    if vertices = :digraph_utils.topsort(graph) do
      sorted =
        Enum.reduce(vertices, [], fn module, acc ->
          {_, {file, status}} = :digraph.vertex(graph, module)
          [{status, file, Path.join(compile_path, "#{module}.beam")} | acc]
        end)

      parallel =
        for %{file: file, module: module} <- erls,
            :digraph.in_neighbours(graph, module) == [],
            do: file

      {sorted, parallel}
    else
      Mix.raise(
        "Could not compile Erlang. " <>
          "The following modules form a cycle: " <>
          Enum.join(Mix.Utils.find_cycle!(graph), ", ")
      )
    end
  end

  defp add_parse_transforms(compile, deps) do
    Enum.reduce(compile, deps, fn
      {:parse_transform, transform}, deps -> [transform | deps]
      _, deps -> deps
    end)
  end

  defp maybe_stale(erl, compile_path, force) do
    beam = Path.join(compile_path, "#{erl.module}.beam")

    if force || Mix.Utils.stale?([erl.file | erl.includes], [beam]) do
      %{erl | status: :stale}
    else
      erl
    end
  end

  defp module_from_artifact(artifact) do
    artifact |> Path.basename() |> Path.rootname() |> String.to_atom()
  end
end
