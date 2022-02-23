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
  import Retry

  defp run(argv) do
    {config, argv} = Kernel.CLI.parse_argv(argv)
    assert Kernel.CLI.process_commands(config) == []
    argv
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

  stderr_test "--help smoke test" do
    output = elixir('--help')
    assert output =~ "Usage: elixir"
  end

  test "--version smoke test" do
    output = elixir('--version')
    assert output =~ "Erlang/OTP #{System.otp_release()}"
    assert output =~ "Elixir #{System.version()}"

    output = iex('--version')
    assert output =~ "Erlang/OTP #{System.otp_release()}"
    assert output =~ "IEx #{System.version()}"

    output = elixir('--version -e "IO.puts(:test_output)"')
    assert output =~ "Erlang/OTP #{System.otp_release()}"
    assert output =~ "Elixir #{System.version()}"
    assert output =~ "Standalone options can't be combined with other options"
  end

  test "--short-version smoke test" do
    output = elixir('--short-version')
    assert output =~ System.version()
    refute output =~ "Erlang"
  end

  stderr_test "combining --help results in error" do
    output = elixir('-e 1 --help')
    assert output =~ "--help : Standalone options can't be combined with other options"

    output = elixir('--help -e 1')
    assert output =~ "--help : Standalone options can't be combined with other options"
  end

  stderr_test "combining --short-version results in error" do
    output = elixir('--short-version -e 1')
    assert output =~ "--short-version : Standalone options can't be combined with other options"

    output = elixir('-e 1 --short-version')
    assert output =~ "--short-version : Standalone options can't be combined with other options"
  end

  test "properly parses paths" do
    root = fixture_path("../../..") |> to_charlist

    args =
      '-pa "#{root}/*" -pz "#{root}/lib/*" -e "IO.inspect(:code.get_path(), limit: :infinity)"'

    list = elixir(args)
    {path, _} = Code.eval_string(list, [])

    # pa
    assert to_charlist(Path.expand('ebin', root)) in path
    assert to_charlist(Path.expand('lib', root)) in path
    assert to_charlist(Path.expand('src', root)) in path

    # pz
    assert to_charlist(Path.expand('lib/list', root)) in path
  end

  stderr_test "properly formats errors" do
    assert String.starts_with?(elixir('-e ":erlang.throw 1"'), "** (throw) 1")
    assert String.starts_with?(elixir('-e ":erlang.error 1"'), "** (ErlangError) Erlang error: 1")
    assert String.starts_with?(elixir('-e "1 +"'), "** (TokenMissingError)")

    assert elixir('-e "Task.async(fn -> raise ArgumentError end) |> Task.await"') =~
             "an exception was raised:\n    ** (ArgumentError) argument error"

    assert elixir('-e "IO.puts(Process.flag(:trap_exit, false)); exit({:shutdown, 1})"') ==
             "false\n"
  end

  stderr_test "blames exceptions" do
    error = elixir('-e "Access.fetch :foo, :bar"')
    assert error =~ "** (FunctionClauseError) no function clause matching in Access.fetch/2"
    assert error =~ "The following arguments were given to Access.fetch/2"
    assert error =~ ":foo"
    assert error =~ "def fetch(-%module{} = container-, +key+)"
    assert error =~ ~r"\(elixir #{System.version()}\) lib/access\.ex:\d+: Access\.fetch/2"
  end
end

defmodule Kernel.CLI.RPCEvalTest do
  use ExUnit.Case, async: true

  import Retry

  defp rpc_eval(command) do
    node = "cli-rpc#{System.unique_integer()}@127.0.0.1"
    elixir('--name #{node} --rpc-eval #{node} "#{command}"')
  end

  test "invokes command on remote node" do
    assert rpc_eval("IO.puts :ok") == "ok\n"
  end

  test "invokes command on remote node without host and --name after --rpc-eval" do
    node = "cli-rpc#{System.unique_integer()}"
    assert elixir('--rpc-eval #{node} "IO.puts :ok" --name #{node}@127.0.0.1 ') == "ok\n"
  end

  test "can be invoked multiple times" do
    node = "cli-rpc#{System.unique_integer()}"

    assert elixir(
             '--name #{node}@127.0.0.1 --rpc-eval #{node} "IO.puts :foo" --rpc-eval #{node} "IO.puts :bar"'
           ) == "foo\nbar\n"
  end

  test "fails on wrong arguments" do
    node = "cli-rpc#{System.unique_integer()}"

    assert elixir('--name #{node}@127.0.0.1 --rpc-eval') ==
             "--rpc-eval : wrong number of arguments\n"

    assert elixir('--name #{node}@127.0.0.1 --rpc-eval #{node}') ==
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

    assert elixir('--name cli-rpc#{System.unique_integer()} --rpc-eval badnode "IO.puts :hi"') ==
             "--rpc-eval : RPC failed with reason :nodedown\n"
  end
end

defmodule Kernel.CLI.RPCCallTest do
  use ExUnit.Case, async: true

  import Retry

  defp rpc_call(module, function, args) do
    node = "cli-rpc#{System.unique_integer()}@127.0.0.1"
    elixir('--name #{node} --rpc-call #{node} #{module} #{function} #{args}')
  end

  test "invokes command on remote node" do
    assert rpc_call("IO", "inspect", "a b -c --d") == ~s|["a", "b", "-c", "--d"]\n|
  end

  test "invokes command on remote node without host" do
    node = "cli-rpc#{System.unique_integer()}"
    assert elixir('--name #{node}@127.0.0.1 --rpc-call #{node} IO puts hi') == "hi\n"
  end

  test "invokes Erlang modules" do
    node = "cli-rpc#{System.unique_integer()}"
    assert elixir('--name #{node}@127.0.0.1 --rpc-call #{node} :io format hi') == "hi"
  end

  test "fails on wrong arguments" do
    node = "cli-rpc#{System.unique_integer()}"

    assert elixir('--name #{node}@127.0.0.1 --rpc-call') ==
             "--rpc-call : wrong number of arguments\n"

    assert elixir('--name #{node}@127.0.0.1 --rpc-call #{node}') ==
             "--rpc-call : wrong number of arguments\n"

    assert elixir('--name #{node}@127.0.0.1 --rpc-call #{node} IO') ==
             "--rpc-call : wrong number of arguments\n"
  end

  stderr_test "properly formats errors" do
    assert String.starts_with?(
             rpc_call(":erlang", "throw", "a b"),
             ~s|** (throw) ["a", "b"]|
           )

    assert String.starts_with?(
             rpc_call(":erlang", "error", "a b"),
             ~s|** (ErlangError) Erlang error: ["a", "b"]|
           )

    # TODO: why does it time out?
    # assert elixir('--name cli-rpc#{System.unique_integer()} --rpc-call badnode IO puts hi"') ==
    #          "--rpc-call : RPC failed with reason :nodedown\n"
  end
end

defmodule Kernel.CLI.AtExitTest do
  use ExUnit.Case, async: true

  test "invokes at_exit callbacks" do
    assert elixir(fixture_path("at_exit.exs") |> to_charlist) ==
             "goodbye cruel world with status 1\n"
  end
end

defmodule Kernel.CLI.CompileTest do
  use ExUnit.Case, async: true

  import Retry
  @moduletag :tmp_dir

  setup context do
    beam_file_path = Path.join([context.tmp_dir, "Elixir.CompileSample.beam"])
    fixture = fixture_path("compile_sample.ex")
    {:ok, [beam_file_path: beam_file_path, fixture: fixture]}
  end

  test "compiles code", context do
    assert elixirc('#{context.fixture} -o #{context.tmp_dir}') == ""
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
      assert elixirc('#{fixture} -o #{tmp_dir_path}') == ""
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
    output = elixirc('#{context.fixture} non_existing.ex -o #{context.tmp_dir}')
    assert output =~ "non_existing.ex"
    refute output =~ "compile_sample.ex"
    refute File.exists?(context.beam_file_path)
  end

  stderr_test "fails on missing write access to .beam file", context do
    compilation_args = '#{context.fixture} -o #{context.tmp_dir}'

    assert elixirc(compilation_args) == ""
    assert File.regular?(context.beam_file_path)

    # Set the .beam file to read-only
    File.chmod!(context.beam_file_path, 4)
    {:ok, %{access: access}} = File.stat(context.beam_file_path)

    # Can only assert when read-only applies to the user
    if access != :read_write do
      output = elixirc(compilation_args)

      expected =
        "(File.Error) could not write to file #{inspect(context.beam_file_path)}: permission denied"

      assert output =~ expected
    end
  end
end
