# Configure ExUnit, no options supported yet.
ExUnit.start []

defmodule PathHelpers do
  def fixture_path() do
    Path.expand("../fixtures", __FILE__)
  end

  def tmp_path() do
    Path.expand("../../tmp", __FILE__)
  end

  def fixture_path(extra) do
    Path.join(fixture_path, extra)
  end

  def tmp_path(extra) do
    Path.join(tmp_path, extra)
  end

  def elixir(args) do
    :os.cmd '#{elixir_executable} #{args}'
  end

  def elixirc(args) do
    :os.cmd '#{elixirc_executable} #{args}'
  end

  def elixir_executable do
    Path.expand("../../../../../bin/elixir", __FILE__)
  end

  def elixirc_executable do
    elixir_executable <> "c"
  end
end