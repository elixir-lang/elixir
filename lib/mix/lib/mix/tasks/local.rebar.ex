defmodule Mix.Tasks.Local.Rebar do
  use Mix.Task

  import Mix.Generator, only: [create_file: 2]

  @rebar_url "https://github.com/elixir-lang/elixir/raw/3cf4c21ad1f4627585b80394c37ac9f4d39d3aa8/rebar"
  @shortdoc  "Install rebar locally"

  @moduledoc """
  Fetch a copy of rebar from the given path or url. It defaults to a
  rebar copy that ships with Elixir source if available or fetches it from
  #{@rebar_url}.
The local copy is stored in your MIX_HOME (defaults to ~/.mix).

  This version of rebar will be used as required by mix deps.compile
  """

  def run(argv) do
    { _, argv } = OptionParser.parse(argv)
    do_install(case argv do
      []       -> rebar_path
      [path|_] -> path
    end)
  end

  defp rebar_path do
    bundled_with_elixir = Path.join(:code.lib_dir(:mix), "../../rebar")
    if File.regular?(bundled_with_elixir), do: bundled_with_elixir, else: @rebar_url
  end

  defp do_install(path) do
    rebar = Mix.Utils.read_path!(path)
    local_rebar_path = Mix.Rebar.local_rebar_path
    File.mkdir_p! Path.dirname(local_rebar_path)
    create_file local_rebar_path, rebar
    :file.change_mode local_rebar_path, 0755
  end
end
