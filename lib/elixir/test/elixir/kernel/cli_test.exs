Code.require_file("../test_helper.exs", __DIR__)

import PathHelpers

defmodule Retry do
  # Tests that write to stderr fail on Windows due to late writes,
  # so we do a simple retry already them.
  defmacro stderr_test(msg, context \\ quote(do: _), do: block) do
    if windows?() do
      quote do
        test unquote(msg), unquote(context) do
          unquote(__MODULE__).retry(fn -> unquote(block) end, 3)
        end
      end
    else
      quote do
        test(unquote(msg), unquote(context), do: unquote(block))
      end
    end
  end

  def retry(fun, 1) do
    fun.()
  end

  def retry(fun, n) do
    try do
      fun.()
    rescue
      _ -> retry(fun, n - 1)
    end
  end
end

defmodule Kernel.CLITest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  defp run(argv) do
    {config, argv} = Kernel.CLI.parse_argv(Enum.map(argv, &String.to_charlist/1))
    assert Kernel.CLI.process_commands(config) == []
    Enum.map(argv, &IO.chardata_to_string/1)
  end

  test "argv handling" do
    assert capture_io(fn ->
             assert run(["-e", "IO.puts :ok", "sample.exs", "-o", "1", "2"]) ==
                      ["sample.exs", "-o", "1", "2"]
           end) == "ok\n"

    assert capture_io(fn ->
             assert run(["-e", "IO.puts :ok", "--", "sample.exs", "-o", "1", "2"]) ==
                      ["sample.exs", "-o", "1", "2"]
           end) == "ok\n"

    assert capture_io(fn ->
             assert run(["-e", "", "--", "sample.exs", "-o", "1", "2"]) ==
                      ["sample.exs", "-o", "1", "2"]
           end)
  end
end

test_parameters =
  if(PathHelpers.windows?(),
    do: [%{cli_extension: ".bat"}],
    else:
      [%{cli_extension: ""}] ++
        if(System.find_executable("pwsh"), do: [%{cli_extension: ".ps1"}], else: [])
  )

defmodule Kernel.CLI.ExecutableTest do
  use ExUnit.Case,
    async: true,
    parameterize: test_parameters

  import Retry

  @tag :tmp_dir
  test "file smoke test", context do
    file = Path.join(context.tmp_dir, "hello_world!.exs")
    File.write!(file, "IO.puts :hello_world123")
    {output, 0} = System.cmd(elixir_executable(context.cli_extension), [file])
    assert output =~ "hello_world123"
  end

  test "--eval smoke test", context do
    {output, 0} =
      System.cmd(elixir_executable(context.cli_extension), ["--eval", "IO.puts :hello_world123"])

    assert output =~ "hello_world123"

    # Check for -e and exclamation mark handling on Windows
    assert {_output, 0} =
             System.cmd(elixir_executable(context.cli_extension), ["-e", "Time.new!(0, 0, 0)"])

    # TODO: remove this once we bump CI to 26.3
    if not (windows?() and System.otp_release() == "26") do
      {output, 0} =
        System.cmd(iex_executable(context.cli_extension), [
          "--eval",
          "IO.puts :hello_world123; System.halt()"
        ])

      assert output =~ "hello_world123"

      {output, 0} =
        System.cmd(iex_executable(context.cli_extension), [
          "-e",
          "IO.puts :hello_world123; System.halt()"
        ])

      assert output =~ "hello_world123"
    end
  end

  test "--version smoke test", %{cli_extension: cli_extension} do
    output = elixir(~c"--version", cli_extension)
    assert output =~ "Erlang/OTP #{System.otp_release()}"
    assert output =~ "Elixir #{System.version()}"

    output = iex(~c"--version", cli_extension)
    assert output =~ "Erlang/OTP #{System.otp_release()}"
    assert output =~ "IEx #{System.version()}"

    output = elixir(~c"--version -e \"IO.puts(:test_output)\"", cli_extension)
    assert output =~ "Erlang/OTP #{System.otp_release()}"
    assert output =~ "Elixir #{System.version()}"
    assert output =~ "Standalone options can't be combined with other options"
  end

  test "--short-version smoke test", %{cli_extension: cli_extension} do
    output = elixir(~c"--short-version", cli_extension)
    assert output =~ System.version()
    refute output =~ "Erlang"
  end

  stderr_test "--help smoke test", %{cli_extension: cli_extension} do
    output = elixir(~c"--help", cli_extension)
    assert output =~ "Usage: elixir"
  end

  stderr_test "combining --help results in error", %{cli_extension: cli_extension} do
    output = elixir(~c"-e 1 --help", cli_extension)
    assert output =~ "--help : Standalone options can't be combined with other options"

    output = elixir(~c"--help -e 1", cli_extension)
    assert output =~ "--help : Standalone options can't be combined with other options"
  end

  stderr_test "combining --short-version results in error", %{cli_extension: cli_extension} do
    output = elixir(~c"--short-version -e 1", cli_extension)
    assert output =~ "--short-version : Standalone options can't be combined with other options"

    output = elixir(~c"-e 1 --short-version", cli_extension)
    assert output =~ "--short-version : Standalone options can't be combined with other options"
  end

  test "parses paths", %{cli_extension: cli_extension} do
    root = fixture_path("../../..") |> to_charlist

    args =
      ~c"-pa \"#{root}/*\" -pz \"#{root}/lib/*\" -e \"IO.inspect(:code.get_path(), limit: :infinity)\""

    list = elixir(args, cli_extension)
    {path, _} = Code.eval_string(list, [])

    # pa
    assert to_charlist(Path.expand(~c"ebin", root)) in path
    assert to_charlist(Path.expand(~c"lib", root)) in path
    assert to_charlist(Path.expand(~c"src", root)) in path

    # pz
    assert to_charlist(Path.expand(~c"lib/list", root)) in path
  end

  stderr_test "formats errors", %{cli_extension: cli_extension} do
    assert String.starts_with?(elixir(~c"-e \":erlang.throw 1\"", cli_extension), "** (throw) 1")

    assert String.starts_with?(
             elixir(~c"-e \":erlang.error 1\"", cli_extension),
             "** (ErlangError) Erlang error: 1"
           )

    assert String.starts_with?(elixir(~c"-e \"1 +\"", cli_extension), "** (TokenMissingError)")

    assert elixir(
             ~c"-e \"Task.async(fn -> raise ArgumentError end) |> Task.await\"",
             cli_extension
           ) =~
             "an exception was raised:\n    ** (ArgumentError) argument error"

    assert elixir(
             ~c"-e \"IO.puts(Process.flag(:trap_exit, false)); exit({:shutdown, 1})\"",
             cli_extension
           ) ==
             "false\n"
  end

  stderr_test "blames exceptions", %{cli_extension: cli_extension} do
    error = elixir(~c"-e \"Access.fetch :foo, :bar\"", cli_extension)
    assert error =~ "** (FunctionClauseError) no function clause matching in Access.fetch/2"
    assert error =~ "The following arguments were given to Access.fetch/2"
    assert error =~ ":foo"
    assert error =~ "def fetch(-%module{} = container-, +key+)"
    assert error =~ ~r"\(elixir #{System.version()}\) lib/access\.ex:\d+: Access\.fetch/2"
  end

  test "invokes at_exit callbacks" do
    assert elixir(fixture_path("at_exit.exs") |> to_charlist()) ==
             "goodbye cruel world with status 1\n"
  end
end

defmodule Kernel.CLI.RPCTest do
  use ExUnit.Case, async: true

  import Retry

  defp rpc_eval(command) do
    node = "cli-rpc#{System.unique_integer()}@127.0.0.1"
    elixir(~c"--name #{node} --rpc-eval #{node} \"#{command}\"")
  end

  test "invokes command on remote node" do
    assert rpc_eval("IO.puts :ok") == "ok\n"
  end

  test "invokes command on remote node without host and --name after --rpc-eval" do
    node = "cli-rpc#{System.unique_integer()}"
    assert elixir(~c"--rpc-eval #{node} \"IO.puts :ok\" --name #{node}@127.0.0.1 ") == "ok\n"
  end

  test "can be invoked multiple times" do
    node = "cli-rpc#{System.unique_integer()}"

    assert elixir(
             ~c"--name #{node}@127.0.0.1 --rpc-eval #{node} \"IO.puts :foo\" --rpc-eval #{node} \"IO.puts :bar\""
           ) == "foo\nbar\n"
  end

  # Windows does not provide an easy to check for missing args
  @tag :unix
  test "fails on wrong arguments" do
    node = "cli-rpc#{System.unique_integer()}"

    assert elixir(~c"--name #{node}@127.0.0.1 --rpc-eval") ==
             "--rpc-eval : wrong number of arguments\n"

    assert elixir(~c"--name #{node}@127.0.0.1 --rpc-eval #{node}") ==
             "--rpc-eval : wrong number of arguments\n"
  end

  stderr_test "properly formats errors" do
    assert String.starts_with?(rpc_eval(":erlang.throw 1"), "** (throw) 1")
    assert String.starts_with?(rpc_eval(":erlang.error 1"), "** (ErlangError) Erlang error: 1")
    assert String.starts_with?(rpc_eval("1 +"), "** (TokenMissingError)")

    assert rpc_eval("Task.async(fn -> raise ArgumentError end) |> Task.await") =~
             "an exception was raised:\n    ** (ArgumentError) argument error"

    assert rpc_eval("IO.puts(Process.flag(:trap_exit, false)); exit({:shutdown, 1})") ==
             "false\n"
  end
end

defmodule Kernel.CLI.CompileTest do
  use ExUnit.Case,
    async: true,
    parameterize: test_parameters

  import Retry
  @moduletag :tmp_dir

  setup context do
    beam_file_path = Path.join([context.tmp_dir, "Elixir.CompileSample.beam"])
    fixture = fixture_path("compile_sample.ex")
    {:ok, [beam_file_path: beam_file_path, fixture: fixture]}
  end

  test "compiles code", context do
    assert elixirc(~c"#{context.fixture} -o #{context.tmp_dir}", context.cli_extension) == ""
    assert File.regular?(context.beam_file_path)

    # Assert that the module is loaded into memory with the proper destination for the BEAM file.
    Code.append_path(context.tmp_dir)
    assert :code.which(CompileSample) |> List.to_string() == Path.expand(context.beam_file_path)
  after
    :code.purge(CompileSample)
    :code.delete(CompileSample)
    Code.delete_path(context.tmp_dir)
  end

  @tag :windows
  stderr_test "compiles code with Windows paths", context do
    try do
      fixture = String.replace(context.fixture, "/", "\\")
      tmp_dir_path = String.replace(context.tmp_dir, "/", "\\")
      assert elixirc(~c"#{fixture} -o #{tmp_dir_path}", context.cli_extension) == ""
      assert File.regular?(context[:beam_file_path])

      # Assert that the module is loaded into memory with the proper destination for the BEAM file.
      Code.append_path(context.tmp_dir)

      assert :code.which(CompileSample) |> List.to_string() ==
               Path.expand(context[:beam_file_path])
    after
      :code.purge(CompileSample)
      :code.delete(CompileSample)
      Code.delete_path(context.tmp_dir)
    end
  end

  stderr_test "fails on missing patterns", context do
    output =
      elixirc(~c"#{context.fixture} non_existing.ex -o #{context.tmp_dir}", context.cli_extension)

    assert output =~ "non_existing.ex"
    refute output =~ "compile_sample.ex"
    refute File.exists?(context.beam_file_path)
  end

  stderr_test "fails on missing write access to .beam file", context do
    compilation_args = ~c"#{context.fixture} -o #{context.tmp_dir}"

    assert elixirc(compilation_args, context.cli_extension) == ""
    assert File.regular?(context.beam_file_path)

    # Set the .beam file to read-only
    File.chmod!(context.beam_file_path, 4)
    {:ok, %{access: access}} = File.stat(context.beam_file_path)

    # Can only assert when read-only applies to the user
    if access != :read_write do
      output = elixirc(compilation_args, context.cli_extension)

      expected =
        "(File.Error) could not write to file #{inspect(context.beam_file_path)}: permission denied"

      assert output =~ expected
    end
  end
end
