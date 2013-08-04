defmodule Mix.Deps.Lock do
  @moduledoc false
  @manifest ".compile.deps"

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
  Touches the manifest timestamp.
  """
  def touch do
    compile_path = Mix.project[:compile_path]
    File.mkdir_p!(compile_path)
    File.touch!(Path.join(compile_path, @manifest))
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

    unless read() == sorted do
      lines = Enum.map_join sorted, ",\n  ", fn { app, rev } ->
        %b("#{app}": #{inspect rev, raw: true})
      end

      File.write! lockfile, "[ " <> lines <> " ]\n"
      touch
    end
  end
end
