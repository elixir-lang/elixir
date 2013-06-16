defmodule Mix.Tasks.Local.Rebar do
  use Mix.Task

  import Mix.Generator, only: [create_file: 2]

  @rebar_url "https://github.com/downloads/basho/rebar/rebar"
  @shortdoc  "Install rebar locally"

  @moduledoc """
  Fetch a copy of rebar from the given path or url, defaulting to
  #{@rebar_url} if no parameter
  given. The local copy is stored in your MIX_HOME (defaults to ~/.mix).

  This version of rebar will be used as required by mix deps.compile
  """

  def run(argv) do
    { _, argv } = OptionParser.parse(argv)
    do_install(case argv do
      []       -> @rebar_url
      [path|_] -> path
    end)
  end

  defp do_install(path) do
    rebar = Mix.Utils.read_path(path)
    local_rebar_path = Mix.Rebar.local_rebar_path
    File.mkdir_p! Path.dirname(local_rebar_path)
    create_file local_rebar_path, rebar
    :file.change_mode local_rebar_path, 0755
  end
end
