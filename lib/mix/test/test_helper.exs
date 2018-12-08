Mix.start()
Mix.shell(Mix.Shell.Process)
Application.put_env(:mix, :colors, enabled: false)

exclude = if match?({:win32, _}, :os.type()), do: [unix: true], else: [windows: true]
ExUnit.start(trace: "--trace" in System.argv(), exclude: exclude)

unless {1, 7, 4} <= Mix.SCM.Git.git_version() do
  IO.puts(:stderr, "Skipping tests with git sparse checkouts...")
  ExUnit.configure(exclude: :git_sparse)
end

# Clear proxy variables that may affect tests
System.delete_env("http_proxy")
System.delete_env("https_proxy")
System.delete_env("HTTP_PROXY")
System.delete_env("HTTPS_PROXY")

defmodule MixTest.Case do
  use ExUnit.CaseTemplate

  defmodule Sample do
    def project do
      [app: :sample, version: "0.1.0", aliases: [sample: "compile"]]
    end
  end

  using do
    quote do
      import MixTest.Case
    end
  end

  setup config do
    if apps = config[:apps] do
      Logger.remove_backend(:console)
      Application.put_env(:logger, :backends, [])
    end

    on_exit(fn ->
      Application.start(:logger)
      Mix.env(:dev)
      Mix.Task.clear()
      Mix.Shell.Process.flush()
      Mix.ProjectStack.clear_cache()
      Mix.ProjectStack.clear_stack()
      delete_tmp_paths()

      if apps do
        for app <- apps do
          Application.stop(app)
          Application.unload(app)
        end

        Logger.add_backend(:console, flush: true)
        Application.put_env(:logger, :backends, [:console])
      end
    end)

    :ok
  end

  def fixture_path do
    Path.expand("fixtures", __DIR__)
  end

  def fixture_path(extension) do
    Path.join(fixture_path(), remove_colons(extension))
  end

  def tmp_path do
    Path.expand("../tmp", __DIR__)
  end

  def tmp_path(extension) do
    Path.join(tmp_path(), remove_colons(extension))
  end

  defp remove_colons(term) do
    term
    |> to_string()
    |> String.replace(":", "")
  end

  def purge(modules) do
    Enum.each(modules, fn m ->
      :code.purge(m)
      :code.delete(m)
    end)
  end

  def in_tmp(which, function) do
    path = tmp_path(which)
    File.rm_rf!(path)
    File.mkdir_p!(path)
    File.cd!(path, function)
  end

  defmacro in_fixture(which, block) do
    module = inspect(__CALLER__.module)
    function = Atom.to_string(elem(__CALLER__.function, 0))
    tmp = Path.join(module, function)

    quote do
      unquote(__MODULE__).in_fixture(unquote(which), unquote(tmp), unquote(block))
    end
  end

  def in_fixture(which, tmp, function) do
    src = fixture_path(which)
    dest = tmp_path(String.replace(tmp, ":", "_"))
    flag = String.to_charlist(tmp_path())

    File.rm_rf!(dest)
    File.mkdir_p!(dest)
    File.cp_r!(src, dest)

    get_path = :code.get_path()
    previous = :code.all_loaded()

    try do
      File.cd!(dest, function)
    after
      :code.set_path(get_path)

      for {mod, file} <- :code.all_loaded() -- previous,
          file == [] or (is_list(file) and List.starts_with?(file, flag)) do
        purge([mod])
      end
    end
  end

  def ensure_touched(file) do
    ensure_touched(file, File.stat!(file).mtime)
  end

  def ensure_touched(file, current) do
    File.touch!(file)

    unless File.stat!(file).mtime > current do
      ensure_touched(file, current)
    end
  end

  def os_newline do
    case :os.type() do
      {:win32, _} -> "\r\n"
      _ -> "\n"
    end
  end

  def mix(args, envs \\ []) when is_list(args) do
    args = ["-r", mix_executable(), "--" | args]
    System.cmd(elixir_executable(), args, stderr_to_stdout: true, env: envs) |> elem(0)
  end

  def mix_port(args, envs \\ []) when is_list(args) do
    Port.open({:spawn_executable, elixir_executable()}, [
      {:args, ["-r", mix_executable(), "--" | args]},
      {:env, envs},
      :binary,
      :use_stdio,
      :stderr_to_stdout
    ])
  end

  defp mix_executable do
    Path.expand("../../../bin/mix", __DIR__)
  end

  defp elixir_executable do
    Path.expand("../../../bin/elixir", __DIR__)
  end

  defp delete_tmp_paths do
    tmp = tmp_path() |> String.to_charlist()
    for path <- :code.get_path(), :string.str(path, tmp) != 0, do: :code.del_path(path)
  end
end

## Set up Mix home with Rebar

home = MixTest.Case.tmp_path(".mix")
File.mkdir_p!(home)
System.put_env("MIX_HOME", home)

rebar = System.get_env("REBAR") || Path.expand("fixtures/rebar", __DIR__)
File.cp!(rebar, Path.join(home, "rebar"))
rebar = System.get_env("REBAR3") || Path.expand("fixtures/rebar3", __DIR__)
File.cp!(rebar, Path.join(home, "rebar3"))

## Copy fixtures to tmp

fixtures = ~w(rebar_dep rebar_override)

Enum.each(fixtures, fn fixture ->
  source = MixTest.Case.fixture_path(fixture)
  dest = MixTest.Case.tmp_path(fixture)
  File.mkdir_p!(dest)
  File.cp_r!(source, dest)
end)

## Generate Git repo fixtures

# Git repo
target = Path.expand("fixtures/git_repo", __DIR__)

unless File.dir?(target) do
  File.mkdir_p!(Path.join(target, "lib"))

  File.write!(Path.join(target, "mix.exs"), """
  ## Auto-generated fixture
  raise "I was not supposed to be loaded"
  """)

  File.cd!(target, fn ->
    System.cmd("git", ~w[-c core.hooksPath='' init])
    System.cmd("git", ~w[config user.email "mix@example.com"])
    System.cmd("git", ~w[config user.name "mix-repo"])
    System.cmd("git", ~w[add .])
    System.cmd("git", ~w[commit -m "bad"])
  end)

  File.write!(Path.join(target, "mix.exs"), """
  ## Auto-generated fixture
  defmodule GitRepo.MixProject do
    use Mix.Project

    def project do
      [
        app: :git_repo,
        version: "0.1.0"
      ]
    end
  end
  """)

  File.cd!(target, fn ->
    System.cmd("git", ~w[add .])
    System.cmd("git", ~w[commit -m "ok"])
    System.cmd("git", ~w[tag without_module])
  end)

  File.write!(Path.join(target, "lib/git_repo.ex"), """
  ## Auto-generated fixture
  defmodule GitRepo do
    def hello do
      "World"
    end
  end
  """)

  ## Sparse
  subdir = Path.join(target, "sparse_dir")
  File.mkdir_p!(Path.join(subdir, "lib"))

  File.write!(Path.join(subdir, "mix.exs"), """
  ## Auto-generated fixture
  defmodule GitSparseRepo.MixProject do
    use Mix.Project

    def project do
      [
        app: :git_sparse_repo,
        version: "0.1.0"
      ]
    end
  end
  """)

  File.write!(Path.join(subdir, "lib/git_sparse_repo.ex"), """
  ## Auto-generated fixture
  defmodule GitSparseRepo do
    def hello do
      "World"
    end
  end
  """)

  File.cd!(target, fn ->
    System.cmd("git", ~w[add .])
    System.cmd("git", ~w[commit -m "lib"])
    System.cmd("git", ~w[tag with_module])
  end)
end

# Deps on Git repo
target = Path.expand("fixtures/deps_on_git_repo", __DIR__)

unless File.dir?(target) do
  File.mkdir_p!(Path.join(target, "lib"))

  File.write!(Path.join(target, "mix.exs"), """
  ## Auto-generated fixture
  defmodule DepsOnGitRepo.MixProject do
    use Mix.Project

    def project do
      [
        app: :deps_on_git_repo,
        version: "0.1.0",
      ]
    end
  end
  """)

  File.cd!(target, fn ->
    System.cmd("git", ~w[-c core.hooksPath='' init])
    System.cmd("git", ~w[config user.email "mix@example.com"])
    System.cmd("git", ~w[config user.name "mix-repo"])
    System.cmd("git", ~w[add .])
    System.cmd("git", ~w[commit -m without-dep])
  end)

  File.write!(Path.join(target, "mix.exs"), """
  ## Auto-generated fixture
  defmodule DepsOnGitRepo.MixProject do
    use Mix.Project

    def project do
      [
        app: :deps_on_git_repo,
        version: "0.2.0",
        deps: [
          {:git_repo, git: MixTest.Case.fixture_path("git_repo")}
        ]
      ]
    end
  end
  """)

  File.write!(Path.join(target, "lib/deps_on_git_repo.ex"), """
  ## Auto-generated fixture
  GitRepo.hello()
  """)

  File.cd!(target, fn ->
    System.cmd("git", ~w[add .])
    System.cmd("git", ~w[commit -m with-dep])
  end)
end

# Git Rebar
target = Path.expand("fixtures/git_rebar", __DIR__)

unless File.dir?(target) do
  File.mkdir_p!(Path.join(target, "src"))

  File.write!(Path.join([target, "src", "git_rebar.app.src"]), """
  {application, git_rebar,
    [
      {vsn, "0.1.0"}
    ]}.
  """)

  File.write!(Path.join([target, "src", "git_rebar.erl"]), """
  -module(git_rebar).
  -export([any_function/0]).
  any_function() -> ok.
  """)

  File.cd!(target, fn ->
    System.cmd("git", ~w[-c core.hooksPath='' init])
    System.cmd("git", ~w[config user.email "mix@example.com"])
    System.cmd("git", ~w[config user.name "mix-repo"])
    System.cmd("git", ~w[add .])
    System.cmd("git", ~w[commit -m "ok"])
  end)
end

Enum.each([:invalidapp, :invalidvsn, :noappfile, :nosemver, :ok], fn dep ->
  File.mkdir_p!(Path.expand("fixtures/deps_status/deps/#{dep}/.git", __DIR__))
end)

## Generate helper modules

path = MixTest.Case.tmp_path("beams")
File.rm_rf!(path)
File.mkdir_p!(path)

write_beam = fn {:module, name, bin, _} ->
  path
  |> Path.join(Atom.to_string(name) <> ".beam")
  |> File.write!(bin)
end

defmodule Mix.Tasks.Hello do
  use Mix.Task
  @shortdoc "This is short documentation, see"

  @moduledoc """
  A test task.
  """

  def run([]) do
    "Hello, World!"
  end

  def run(["--parser" | args]) do
    OptionParser.parse!(args, strict: [int: :integer])
  end

  def run(args) do
    "Hello, #{Enum.join(args, " ")}!"
  end
end
|> write_beam.()

defmodule Mix.Tasks.Invalid do
end
|> write_beam.()

defmodule Mix.Tasks.Acronym.HTTP do
  use Mix.Task
  def run(_), do: "An HTTP Task"
end
|> write_beam.()
