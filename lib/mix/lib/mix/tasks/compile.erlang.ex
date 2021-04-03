defmodule Mix.Tasks.Compile.Erlang do
  use Mix.Task.Compiler
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest "compile.erlang"
  @switches [force: :boolean, all_warnings: :boolean]

  @moduledoc """
  Compiles Erlang source files.

  When this task runs, it will first check the modification times of
  all files to be compiled and if they haven't been
  changed since the last compilation, it will not compile
  them. If any of them have changed, it compiles
  everything.

  For this reason, the task touches your `:compile_path`
  directory and sets the modification time to the current
  time and date at the end of each compilation. You can
  force compilation regardless of modification times by passing
  the `--force` option.

  ## Command line options

    * `--force` - forces compilation regardless of modification times

    * `--all-warnings` - prints warnings even from files that do not need to be
      recompiled

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

      The option `:debug_info` is always added to the end of it. You can
      disable that using:

          erlc_options: [debug_info: false]

  """

  @impl true
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)
    project = Mix.Project.config()
    source_paths = project[:erlc_paths]
    Mix.Compilers.Erlang.assert_valid_erlc_paths(source_paths)
    files = Mix.Utils.extract_files(source_paths, [:erl])
    do_run(files, opts, project, source_paths)
  end

  defp do_run([], _, _, _), do: {:noop, []}

  defp do_run(files, opts, project, source_paths) do
    include_path = Erlang.to_erl_file(project[:erlc_include_path])
    compile_path = Erlang.to_erl_file(Mix.Project.compile_path(project))
    erlc_options = project[:erlc_options] || []

    unless is_list(erlc_options) do
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

    {erls, tuples} =
      Enum.unzip(scan_sources(files, include_path, source_paths, compile_path, opts))

    opts = [parallel: MapSet.new(find_parallel(erls))] ++ opts

    Erlang.compile(manifest(), tuples, opts, fn input, _output ->
      # We're purging the module because a previous compiler (for example, Phoenix)
      # might have already loaded the previous version of it.
      module = input |> Path.basename(".erl") |> String.to_atom()
      :code.purge(module)
      :code.delete(module)

      file = Erlang.to_erl_file(Path.rootname(input, ".erl"))

      case :compile.file(file, erlc_options) do
        # TODO: Don't handle {:error, :badarg} when we require Erlang/OTP 24
        error when error == :error or error == {:error, :badarg} ->
          message =
            "Compiling Erlang file #{inspect(file)} failed, probably because of invalid :erlc_options"

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
  def clean do
    Mix.Compilers.Erlang.clean(manifest())
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
      {:ok, {:ok, erl_file, target_tuple}} -> [{erl_file, target_tuple}]
      {:ok, :error} -> []
    end)
  end

  defp scan_source(file, include_paths, compile_path, opts) do
    erl_file = %{
      file: file,
      module: module_from_artifact(file),
      behaviours: [],
      compile: [],
      includes: [],
      invalid: false
    }

    case :epp.parse_file(Erlang.to_erl_file(file), include_paths, []) do
      {:ok, forms} ->
        erl_file = List.foldl(tl(forms), erl_file, &do_form(file, &1, &2))
        target_tuple = annotate_target(erl_file, compile_path, opts[:force])
        {:ok, erl_file, target_tuple}

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
        %{erl | behaviours: [behaviour | erl.behaviours]}

      {:attribute, _, :behavior, behaviour} ->
        %{erl | behaviours: [behaviour | erl.behaviours]}

      {:attribute, _, :compile, value} when is_list(value) ->
        %{erl | compile: value ++ erl.compile}

      {:attribute, _, :compile, value} ->
        %{erl | compile: [value | erl.compile]}

      _ ->
        erl
    end
  end

  defp find_parallel(erls) do
    serial = MapSet.new(find_dependencies(erls))

    erls
    |> Enum.reject(&(&1.module in serial))
    |> Enum.map(& &1.file)
  end

  defp find_dependencies(erls) do
    Enum.flat_map(erls, fn erl ->
      transforms =
        Enum.flat_map(erl.compile, fn
          {:parse_transform, transform} -> [transform]
          _ -> []
        end)

      transforms ++ erl.behaviours
    end)
  end

  defp annotate_target(erl, compile_path, force) do
    beam = Path.join(compile_path, "#{erl.module}.beam")

    if force || Mix.Utils.stale?([erl.file | erl.includes], [beam]) do
      {:stale, erl.file, beam}
    else
      {:ok, erl.file, beam}
    end
  end

  defp module_from_artifact(artifact) do
    artifact |> Path.basename() |> Path.rootname() |> String.to_atom()
  end
end
