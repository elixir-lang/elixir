defmodule Mix.Deps.Lock do
  @moduledoc false
  @manifest ".compile.lock"

  @doc """
  Returns the manifest file for dependencies.
  """
  def manifest do
    Path.join(Mix.project[:compile_path], @manifest)
  end

  @doc """
  Returns the lockfile path. In case this is a nested
  dependencies, the lockfile will always point to the
  parent project lockfile.
  """
  def lockfile do
    Mix.project[:root_lockfile] || Mix.project[:lockfile]
  end

  @doc """
  Touches the manifest timestamp and updates the elixir version.
  """
  def touch do
    compile_path = Mix.project[:compile_path]
    File.mkdir_p!(compile_path)
    File.write!(Path.join(compile_path, @manifest), System.version)
  end

  @doc """
  Returns the elixir lock version.
  """
  def elixir_vsn do
    case File.read(manifest) do
      { :ok, contents } -> contents
      { :error, _ } -> nil
    end
  end

  @doc """
  Read the file, returns a keyword list containing
  the app name and its current lock information.
  """
  def read() do
    case File.read(lockfile) do
      { :ok, info } ->
        { value, _binding } = Code.eval_string(info)
        value || []
      { :error, _ } ->
        []
    end
  end

  @doc """
  Receives a keyword list and writes it to the disk.
  """
  def write(dict) do
    sorted = lc { app, rev } inlist Enum.sort(dict), rev != nil, do: { app, rev }

    unless sorted == read do
      lines  = Enum.map_join sorted, ",\n  ", fn { app, rev } ->
        %s("#{app}": #{inspect rev, raw: true, limit: :infinity})
      end

      File.write! lockfile, "[ " <> lines <> " ]\n"
      touch
    end
  end
end
