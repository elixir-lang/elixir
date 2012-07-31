Mix.start()
Mix.shell(Mix.Shell.Process)

ExUnit.start []

target = File.expand_path("../fixtures/git_repo", __FILE__)

unless File.dir?(target) do
  IO.puts "Creating git repo for tests"
  File.mkdir_p!(File.join(target, "lib"))

  File.write!(File.join(target, "mix.exs"), """)
  defmodule GitRepo.Mix do
    use Mix.Project

    def project do
      [app: :git_repo, version: "0.1.0"]
    end
  end
  """

  File.cd! target, fn ->
    System.cmd("git init")
    System.cmd("git add .")
    System.cmd("git commit -m \"ok\"")
  end

  File.write!(File.join(target, "lib/git_repo.ex"), """)
  defmodule GitRepo do
    def hello do
      "World"
    end
  end
  """

  File.cd! target, fn ->
    System.cmd("git add .")
    System.cmd("git commit -m \"lib\"")
  end
end

Enum.each [:invalidapp, :invalidvsn, :noappfile, :ok], fn(dep) ->
  File.mkdir_p! File.expand_path("../fixtures/deps_status/deps/#{dep}/.git", __FILE__)
end

defmodule MixTest.Case do
  defmacro __using__(opts) do
    quote do
      use ExUnit.Case, unquote(opts)
      import MixTest.Case

      def teardown(_) do
        Mix.Task.clear
        Mix.Shell.Process.flush
        System.put_env("MIXHOME", tmp_path)
        del_tmp_paths
      end

      defoverridable [teardown: 1]
    end
  end

  def mix(args) do
    System.cmd "#{elixir_executable} #{mix_executable} #{args}"
  end

  def mix_executable do
    File.expand_path("../../../../bin/mix", __FILE__)
  end

  def elixir_executable do
    File.expand_path("../../../../bin/elixir", __FILE__)
  end

  def fixture_path do
    File.expand_path("../fixtures", __FILE__)
  end

  def fixture_path(extension) do
    File.join fixture_path, extension
  end

  def tmp_path do
    File.expand_path("../../tmp", __FILE__)
  end

  def tmp_path(extension) do
    File.join tmp_path, extension
  end

  def purge(modules) do
    Enum.each modules, fn(m) ->
      :code.delete(m)
      :code.purge(m)
    end
  end

  def del_tmp_paths do
    tmp = tmp_path /> binary_to_list
    to_remove = Enum.filter :code.get_path, fn(path) -> :string.str(path, tmp) != 0 end
    Enum.map to_remove, :code.del_path(&1)
  end

  def in_tmp(which, function) do
    path = tmp_path(which)
    File.rm_rf! path
    File.mkdir_p! path
    File.cd! path, function
  end

  defmacro in_fixture(which, block) do
    module   = inspect __CALLER__.module
    function = atom_to_binary elem(__CALLER__.function, 1)
    tmp      = File.join(module, function)

    quote do
      src  = File.join fixture_path(unquote(which)), "."
      dest = tmp_path(unquote(tmp))

      File.rm_rf!(dest)
      File.mkdir_p!(dest)
      File.cp_r!(src, dest)

      File.cd! dest, unquote(block)
    end
  end
end

defmodule Mix.Tasks.Hello do
  use Mix.Task
  @shortdoc "This is short documentation, see"

  @moduledoc """
  A test task.
  """

  def run(_) do
    "Hello, World!"
  end
end

defmodule Mix.Tasks.Invalid do
end