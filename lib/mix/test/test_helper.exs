home = Path.expand("../tmp/.home", __DIR__)
File.mkdir_p!(home)
System.put_env("HOME", home)

mix = Path.expand("../tmp/.mix", __DIR__)
File.mkdir_p!(mix)
System.put_env("MIX_HOME", mix)

System.delete_env("XDG_DATA_HOME")
System.delete_env("XDG_CONFIG_HOME")

# Load protocols to make sure they are not unloaded during tests
[Collectable, Enumerable, Inspect, String.Chars, List.Chars]
|> Enum.each(& &1.__protocol__(:module))

## Setup Mix

Mix.start()
Mix.shell(Mix.Shell.Process)
Application.put_env(:mix, :colors, enabled: false)

Logger.remove_backend(:console)
Application.put_env(:logger, :backends, [])

## Setup ExUnit

os_exclude = if match?({:win32, _}, :os.type()), do: [unix: true], else: [windows: true]
epmd_exclude = if match?({:win32, _}, :os.type()), do: [epmd: true], else: []

git_exclude =
  Mix.SCM.Git.unsupported_options()
  |> Enum.map(fn
    :sparse -> {:git_sparse, true}
    :depth -> {:git_depth, true}
  end)

{line_exclude, line_include} =
  if line = System.get_env("LINE"), do: {[:test], [line: line]}, else: {[], []}

ExUnit.start(
  trace: !!System.get_env("TRACE"),
  exclude: epmd_exclude ++ os_exclude ++ git_exclude ++ line_exclude,
  include: line_include
)

# Clear environment variables that may affect tests
System.delete_env("http_proxy")
System.delete_env("https_proxy")
System.delete_env("HTTP_PROXY")
System.delete_env("HTTPS_PROXY")
System.delete_env("MIX_ENV")
System.delete_env("MIX_TARGET")

defmodule MixTest.Case do
  use ExUnit.CaseTemplate

  defmodule Sample do
    def project do
      [app: :sample, version: "0.1.0", aliases: [sample: "compile"]]
    end

    def application do
      Process.get({__MODULE__, :application}) || []
    end
  end

  using do
    quote do
      import MixTest.Case
    end
  end

  @apps Enum.map(Application.loaded_applications(), &elem(&1, 0))

  setup do
    on_exit(fn ->
      Application.start(:logger)
      Mix.env(:dev)
      Mix.target(:host)
      Mix.Task.clear()
      Mix.Shell.Process.flush()
      Mix.State.clear_cache()
      Mix.ProjectStack.clear_stack()
      delete_tmp_paths()

      for {app, _, _} <- Application.loaded_applications(), app not in @apps do
        Application.stop(app)
        Application.unload(app)
      end

      :ok
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

    function =
      case __CALLER__.function do
        {name, _arity} -> Atom.to_string(name)
        nil -> raise "expected in_fixture/2 to be called from a function"
      end

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
    ensure_touched(file, file)
  end

  def ensure_touched(file, current) when is_binary(current) do
    ensure_touched(file, File.stat!(current).mtime)
  end

  def ensure_touched(file, current) when is_tuple(current) do
    File.touch!(file)
    mtime = File.stat!(file).mtime

    if mtime <= current do
      seconds =
        :calendar.datetime_to_gregorian_seconds(current) -
          :calendar.datetime_to_gregorian_seconds(mtime)

      Process.sleep(seconds * 1000)
      ensure_touched(file, current)
    end
  end

  if match?({:win32, _}, :os.type()) do
    def windows?, do: true
    def os_newline, do: "\r\n"
  else
    def windows?, do: false
    def os_newline, do: "\n"
  end

  def mix(args, envs \\ []) when is_list(args) do
    mix_code(args, envs) |> elem(0)
  end

  def mix_code(args, envs \\ []) when is_list(args) do
    args = ["-r", mix_executable(), "--" | args]
    System.cmd(elixir_executable(), args, stderr_to_stdout: true, env: envs)
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

  def force_recompilation(file) do
    File.write!(file, File.read!(file) <> "\n")
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

  def get_git_repo_revs(repo) do
    File.cd!(fixture_path(repo), fn ->
      Regex.split(~r/\r?\n/, System.cmd("git", ["log", "--format=%H"]) |> elem(0), trim: true)
    end)
  end
end

## Set up Rebar fixtures

rebar3_source = System.get_env("REBAR3") || Path.expand("fixtures/rebar3", __DIR__)
[major, minor | _] = String.split(System.version(), ".")
rebar3_target = Path.join([mix, "elixir", "#{major}-#{minor}", "rebar3"])
File.mkdir_p!(Path.dirname(rebar3_target))
File.cp!(rebar3_source, rebar3_target)

fixtures = ~w(rebar_dep rebar_override)

Enum.each(fixtures, fn fixture ->
  source = MixTest.Case.fixture_path(fixture)
  dest = MixTest.Case.tmp_path(fixture)
  File.mkdir_p!(dest)
  File.cp_r!(source, dest)
end)

## Set up Git fixtures

System.cmd("git", ~w[config --global user.email mix@example.com])
System.cmd("git", ~w[config --global user.name mix-repo])
System.cmd("git", ~w[config --global init.defaultBranch not-main])

### Git repo
target = Path.expand("fixtures/git_repo", __DIR__)

if not File.dir?(target) do
  File.mkdir_p!(Path.join(target, "lib"))

  File.write!(Path.join(target, "mix.exs"), """
  ## Auto-generated fixture
  raise "I was not supposed to be loaded"
  """)

  File.cd!(target, fn ->
    System.cmd("git", ~w[init])
    System.cmd("git", ~w[add .])
    System.cmd("git", ~w[commit -m "bad"])
    System.cmd("git", ~w[checkout -q -b main])
    System.cmd("git", ~w[symbolic-ref HEAD refs/heads/main])
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

### Deps on Git repo
target = Path.expand("fixtures/deps_on_git_repo", __DIR__)

if not File.dir?(target) do
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
    System.cmd("git", ~w[init])
    System.cmd("git", ~w[add .])
    System.cmd("git", ~w[commit -m without-dep])
    System.cmd("git", ~w[checkout -q -b main])
    System.cmd("git", ~w[symbolic-ref HEAD refs/heads/main])
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

if not File.dir?(target) do
  File.mkdir_p!(Path.join(target, "ebin"))
  File.mkdir_p!(Path.join(target, "src"))

  # This is used to test that the built-in ebin is ignored
  # when build_embedded is true.
  File.write!(Path.join(target, "ebin/.unused"), """
  """)

  File.write!(Path.join(target, "src/git_rebar.app.src"), """
  {application, git_rebar,
    [
      {vsn, "0.1.0"}
    ]}.
  """)

  File.write!(Path.join(target, "src/git_rebar.erl"), """
  -module(git_rebar).
  -export([any_function/0]).
  any_function() -> ok.
  """)

  File.cd!(target, fn ->
    System.cmd("git", ~w[init])
    System.cmd("git", ~w[add .])
    System.cmd("git", ~w[commit -m "ok"])
    System.cmd("git", ~w[checkout -q -b main])
    System.cmd("git", ~w[symbolic-ref HEAD refs/heads/main])
  end)
end

Enum.each([:invalidapp, :invalidvsn, :noappfile, :nosemver, :ok], fn dep ->
  File.mkdir_p!(Path.expand("fixtures/deps_status/deps/#{dep}/.git", __DIR__))
end)

### Archive ebin
target = Path.expand("fixtures/archive", __DIR__)

if not File.dir?(Path.join(target, "ebin")) do
  File.mkdir_p!(Path.join(target, "ebin"))

  File.write!(Path.join([target, "ebin", "local_sample.app"]), """
  {application,local_sample,
    [
      {modules,['Elixir.Mix.Tasks.Local.Sample']},
      {applications,[kernel,stdlib,elixir]}
    ]
  }.
  """)

  [{name, bin}] = Code.compile_file("lib/local.sample.ex", target)

  File.write!(Path.join([target, "ebin", Atom.to_string(name) <> ".beam"]), bin)
end

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
