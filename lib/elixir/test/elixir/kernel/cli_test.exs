Code.require_file("../test_helper.exs", __DIR__)

import PathHelpers

defmodule Kernel.CLITest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

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
             assert run(["--eval", "IO.puts :ok", "--hidden", "sample.exs", "-o", "1", "2"]) ==
                      ["sample.exs", "-o", "1", "2"]
           end) == "ok\n"

    assert capture_io(fn ->
             assert run(["--eval", "IO.puts :ok", "--", "--hidden", "sample.exs", "-o", "1", "2"]) ==
                      ["--hidden", "sample.exs", "-o", "1", "2"]
           end) == "ok\n"
  end

  test "properly parses paths" do
    root = fixture_path("../../..") |> to_charlist
    args = '-pa "#{root}/*" -pz "#{root}/lib/*" -e "IO.inspect(:code.get_path, limit: :infinity)"'
    list = elixir(args)
    {path, _} = Code.eval_string(list, [])

    # pa
    assert to_charlist(Path.expand('ebin', root)) in path
    assert to_charlist(Path.expand('lib', root)) in path
    assert to_charlist(Path.expand('src', root)) in path

    # pz
    assert to_charlist(Path.expand('lib/list', root)) in path
  end

  test "properly formats errors" do
    assert String.starts_with?(elixir('-e ":erlang.throw 1"'), "** (throw) 1")
    assert String.starts_with?(elixir('-e ":erlang.error 1"'), "** (ErlangError) Erlang error: 1")
    assert String.starts_with?(elixir('-e "1 +"'), "** (TokenMissingError)")

    assert elixir('-e "Task.async(fn -> raise ArgumentError end) |> Task.await"') =~
             "an exception was raised:\n    ** (ArgumentError) argument error"

    assert elixir('-e "IO.puts(Process.flag(:trap_exit, false)); exit({:shutdown, 1})"') ==
             "false\n"
  end

  test "blames exceptions" do
    error = elixir('-e "Access.fetch :foo, :bar"')
    assert error =~ "** (FunctionClauseError) no function clause matching in Access.fetch/2"
    assert error =~ "The following arguments were given to Access.fetch/2"
    assert error =~ ":foo"
    assert error =~ "def fetch(-%module{} = container-, +key+)"
    assert error =~ ~r"\(elixir\) lib/access\.ex:\d+: Access\.fetch/2"
  end
end

defmodule Kernel.CLI.RPCTest do
  use ExUnit.Case, async: true

  defp rpc_eval(command) do
    node = "cli-rpc#{System.unique_integer()}@127.0.0.1"
    elixir('--name #{node} --rpc-eval #{node} "#{command}"')
  end

  test "invokes command on remote node" do
    assert rpc_eval("IO.puts :ok") == "ok\n"
  end

  test "properly formats errors" do
    assert String.starts_with?(rpc_eval(":erlang.throw 1"), "** (throw) 1")
    assert String.starts_with?(rpc_eval(":erlang.error 1"), "** (ErlangError) Erlang error: 1")
    assert String.starts_with?(rpc_eval("1 +"), "** (TokenMissingError)")

    assert rpc_eval("Task.async(fn -> raise ArgumentError end) |> Task.await") =~
             "an exception was raised:\n    ** (ArgumentError) argument error"

    assert rpc_eval("IO.puts(Process.flag(:trap_exit, false)); exit({:shutdown, 1})") ==
             "false\n"
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

  setup context do
    # Set up a per-test temporary directory, so we can run these with async: true.
    # We use the test's line number as the directory name, so they won't conflict.
    tmp_dir_path = tmp_path("beams/#{context[:line]}")
    beam_file_path = Path.join([tmp_dir_path, "Elixir.CompileSample.beam"])
    fixture = fixture_path("compile_sample.ex")
    File.mkdir_p!(tmp_dir_path)
    {:ok, [tmp_dir_path: tmp_dir_path, beam_file_path: beam_file_path, fixture: fixture]}
  end

  test "compiles code", context do
    assert elixirc('#{context[:fixture]} -o #{context[:tmp_dir_path]}') == ""
    assert File.regular?(context[:beam_file_path])

    # Assert that the module is loaded into memory with the proper destination for the BEAM file.
    Code.append_path(context[:tmp_dir_path])
    assert :code.which(CompileSample) |> List.to_string() == Path.expand(context[:beam_file_path])
  after
    Code.delete_path(context[:tmp_dir_path])
  end

  test "fails on missing patterns", context do
    output = elixirc('#{context[:fixture]} non_existing.ex -o #{context[:tmp_dir_path]}')
    assert output =~ "non_existing.ex"
    refute output =~ "compile_sample.ex"
    refute File.exists?(context[:beam_file_path])
  end

  test "fails on missing write access to .beam file", context do
    compilation_args = '#{context[:fixture]} -o #{context[:tmp_dir_path]}'

    assert elixirc(compilation_args) == ""
    assert File.regular?(context[:beam_file_path])

    # Set the .beam file to read-only
    File.chmod!(context[:beam_file_path], 4)
    {:ok, %{access: access}} = File.stat(context[:beam_file_path])

    # Can only assert when read-only applies to the user
    if access != :read_write do
      output = elixirc(compilation_args)

      expected =
        "(File.Error) could not write to #{inspect(context[:beam_file_path])}: permission denied"

      assert String.contains?(output, expected)
    end
  end
end
