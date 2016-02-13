Code.require_file "../test_helper.exs", __DIR__

import PathHelpers

defmodule Kernel.CLI.ARGVTest do
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
      assert run(["-e", "IO.puts :ok", "--hidden", "sample.exs", "-o", "1", "2"]) ==
             ["sample.exs", "-o", "1", "2"]
    end) == "ok\n"

    assert capture_io(fn ->
      assert run(["-e", "IO.puts :ok", "--", "--hidden", "sample.exs", "-o", "1", "2"]) ==
             ["--hidden", "sample.exs", "-o", "1", "2"]
    end) == "ok\n"
  end
end

defmodule Kernel.CLI.OptionParsingTest do
  use ExUnit.Case, async: true

  test "properly parses paths" do
    root = fixture_path("../../..") |> to_char_list
    list = elixir('-pa "#{root}/*" -pz "#{root}/lib/*" -e "IO.inspect(:code.get_path, limit: :infinity)"')
    {path, _} = Code.eval_string list, []

    # pa
    assert to_char_list(Path.expand('ebin', root)) in path
    assert to_char_list(Path.expand('lib', root)) in path
    assert to_char_list(Path.expand('src', root)) in path

    # pz
    assert to_char_list(Path.expand('lib/list', root)) in path
  end
end

defmodule Kernel.CLI.AtExitTest do
  use ExUnit.Case, async: true

  test "invokes at_exit callbacks" do
    assert elixir(fixture_path("at_exit.exs") |> to_char_list) ==
           'goodbye cruel world with status 1\n'
  end
end

defmodule Kernel.CLI.ErrorTest do
  use ExUnit.Case, async: true

  test "properly format errors" do
    assert :string.str('** (throw) 1', elixir('-e "throw 1"')) == 0
    assert :string.str('** (ErlangError) erlang error: 1', elixir('-e "error 1"')) == 0
    assert elixir('-e "IO.puts(Process.flag(:trap_exit, false)); exit({:shutdown, 1})"') == 'false\n'
  end
end

defmodule Kernel.CLI.CompileTest do
  use ExUnit.Case, async: true

  setup context do
    # Set up a per-test temporary directory, so we can run these with async: true.
    # We use the test's line number as the directory name, so they won't conflict.
    tmp_dir_path = tmp_path("beams/#{context[:line]}")
    beam_file_path = Path.join([tmp_dir_path, "Elixir.CompileSample.beam"])
    fixture = fixture_path "compile_sample.ex"
    File.mkdir_p!(tmp_dir_path)
    {:ok, [tmp_dir_path: tmp_dir_path, beam_file_path: beam_file_path, fixture: fixture]}
  end

  test "compiles code", context do
    assert elixirc('#{context[:fixture]} -o #{context[:tmp_dir_path]}') == ''
    assert File.regular?(context[:beam_file_path])
    # Assert that the module is loaded into memory with the proper destination for the BEAM file.
    Code.append_path context[:tmp_dir_path]
    assert :code.which(CompileSample) |> List.to_string == Path.expand(context[:beam_file_path])
  after
    Code.delete_path context[:tmp_dir_path]
  end

  test "compiles code with verbose mode", context do
    assert elixirc('#{context[:fixture]} -o #{context[:tmp_dir_path]} --verbose') ==
      'Compiled #{context[:fixture]}\n'
    assert File.regular?(context[:beam_file_path])
  end

  test "fails on missing patterns", context do
    output = elixirc('#{context[:fixture]} non_existing.ex -o #{context[:tmp_dir_path]}')
    assert :string.str(output, 'non_existing.ex') > 0, "expected non_existing.ex to be mentioned"
    assert :string.str(output, 'compile_sample.ex') == 0, "expected compile_sample.ex to not be mentioned"
    refute File.exists?(context[:beam_file_path]), "expected the sample to not be compiled"
  end

  test "fails on missing write access to .beam file", context do
    compilation_args = '#{context[:fixture]} -o #{context[:tmp_dir_path]}'

    assert elixirc(compilation_args) == ''
    assert File.regular?(context[:beam_file_path])

    # Set the .beam file to read-only
    File.chmod!(context[:beam_file_path], 4)

    {:ok, %{access: access}} = File.stat(context[:beam_file_path])

    # Can only assert when read-only applies to the user
    if access != :read_write do
      output = elixirc(compilation_args)
      expected = '(File.Error) could not write to "' ++ String.to_char_list(context[:beam_file_path]) ++ '": permission denied'
      assert :string.str(output, expected) > 0, "expected compilation error message due to not having write access"
    end
  end
end

defmodule Kernel.CLI.ParallelCompilerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  test "compiles files solving dependencies" do
    fixtures = [fixture_path("parallel_compiler/bar.ex"), fixture_path("parallel_compiler/foo.ex")]
    assert capture_io(fn ->
      assert [Bar, Foo] = Kernel.ParallelCompiler.files fixtures
    end) =~ "message_from_foo"
  after
    Enum.map [Foo, Bar], fn mod ->
      :code.purge(mod)
      :code.delete(mod)
    end
  end

  test "compiles files with structs solving dependencies" do
    fixtures = [fixture_path("parallel_struct/bar.ex"), fixture_path("parallel_struct/foo.ex")]
    assert [Bar, Foo] = Kernel.ParallelCompiler.files(fixtures) |> Enum.sort
  after
    Enum.map [Foo, Bar], fn mod ->
      :code.purge(mod)
      :code.delete(mod)
    end
  end

  test "does not hang on missing dependencies" do
    fixtures = [fixture_path("parallel_compiler/bat.ex")]
    assert capture_io(fn ->
      assert catch_exit(Kernel.ParallelCompiler.files(fixtures)) == {:shutdown, 1}
    end) =~ "== Compilation error"
  end

  test "handles possible deadlocks" do
    fixtures = [fixture_path("parallel_deadlock/foo.ex"),
                fixture_path("parallel_deadlock/bar.ex")]

    msg = capture_io(fn ->
      assert catch_exit(Kernel.ParallelCompiler.files fixtures) == {:shutdown, 1}
    end)

    assert msg =~ "Compilation failed because of a deadlock between files."
    assert msg =~ "fixtures/parallel_deadlock/foo.ex => Bar"
    assert msg =~ "fixtures/parallel_deadlock/bar.ex => Foo"
    assert msg =~ ~r"== Compilation error on file .+parallel_deadlock/foo\.ex =="
    assert msg =~ "** (CompileError)  deadlocked waiting on module Bar"
    assert msg =~ ~r"== Compilation error on file .+parallel_deadlock/bar\.ex =="
    assert msg =~ "** (CompileError)  deadlocked waiting on module Foo"
  end

  test "warnings as errors" do
    warnings_as_errors = Code.compiler_options[:warnings_as_errors]
    fixtures = [fixture_path("warnings_sample.ex")]

    try do
      Code.compiler_options(warnings_as_errors: true)

      msg = capture_io :stderr, fn ->
        assert catch_exit(Kernel.ParallelCompiler.files fixtures) == {:shutdown, 1}
      end

      assert msg =~ "Compilation failed due to warnings while using the --warnings-as-errors option\n"
    after
      Code.compiler_options(warnings_as_errors: warnings_as_errors)
    end
  end
end
