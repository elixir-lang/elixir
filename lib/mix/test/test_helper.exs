Mix.start
ExUnit.start []

defmodule MixTest.Case do
  defmacro __using__(opts) do
    quote do
      use ExUnit.Case, unquote(opts)
      import MixTest.Case
    end
  end

  def mix(args) do
    System.cmd "#{mix_executable} #{args}"
  end

  def mix_executable do
    File.expand_path("../../../../bin/mix", __FILE__)
  end

  def fixture_path do
    File.expand_path("../fixtures", __FILE__)
  end

  def fixture_path(extension) do
    File.join fixture_path, extension
  end

  def in_fixture(which, function) do
    File.chdir! fixture_path(which), function
  end
end

defmodule Mix.Tasks.Hello do
  use Mix.Task
  @shortdoc "This is short documentation, see."
  @moduledoc """
  A test task.
  """
  def run(_) do
    "Hello, World!"
  end
end

defmodule Mix.Tasks.Invalid do
end