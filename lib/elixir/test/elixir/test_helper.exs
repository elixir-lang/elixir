# Configure ExUnit, no options supported yet.
ExUnit.start []

defmodule PathHelpers do
  def fixture_path() do
    File.expand_path("../fixtures", __FILE__)
  end

  def tmp_path() do
    File.expand_path("../tmp", __FILE__)
  end

  def fixture_path(extra) do
    File.join(fixture_path, extra)
  end

  def tmp_path(extra) do
    File.join(tmp_path, extra)
  end

  def elixir(args) do
    :os.cmd '#{elixir_executable} #{args}'
  end

  def elixirc(args) do
    :os.cmd '#{elixirc_executable} #{args}'
  end

  def elixir_executable do
    File.expand_path("../../../../../bin/elixir", __FILE__)
  end

  def elixirc_executable do
    elixir_executable <> "c"
  end
end