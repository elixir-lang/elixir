Code.require_file "test_helper.exs", __DIR__

defmodule DialyzerTest do
  use ExUnit.Case, async: true
  import PathHelpers

  @moduletag :dialyzer

  setup_all do
    dir = tmp_path("dialyzer")
    File.mkdir_p!(dir)
    plt = Path.join(dir, "base_plt") |> String.to_char_list()
    # Add a few key elixir modules for types
    files = Enum.map([Kernel, String, Keyword, Exception], &:code.which/1)
    :dialyzer.run([analysis_type: :plt_build, output_plt: plt, apps: [:erts],
      files: files])
    {:ok, [base_dir: dir, base_plt: plt]}
  end

  setup context do
    # Set up a per-test temporary directory, so we can run these with async: true.
    # We use the test's line number as the directory name, so they won't conflict.
    dir = context[:base_dir]
      |> Path.join("line#{context[:line]}")
      |> String.to_char_list()
    File.mkdir_p!(dir)

    base_plt = context[:base_plt]
    plt = dir
      |> Path.join("plt")
      |> String.to_char_list()
    File.cp!(base_plt, plt)

    dialyzer = [analysis_type: :succ_typings, check_plt: false,
      files_rec: [dir], plts: [plt]]

    {:ok, [outdir: dir, dialyzer: dialyzer]}
  end

  test "no warnings on valid remote calls", context do
    fixture = fixture_path("dialyzer/remote_call.ex")
    assert '' = elixirc("#{fixture} -o #{context[:outdir]}")
    case :dialyzer.run(context[:dialyzer]) do
      [] ->
        :ok
      warnings ->
        flunk IO.chardata_to_string(for warn <- warnings, do: [:dialyzer.format_warning(warn), ?\n])
    end
  end

  test "no warnings on valid raise", context do
    fixture = fixture_path("dialyzer/raise.ex")
    assert '' = elixirc("#{fixture} -o #{context[:outdir]}")
    case :dialyzer.run(context[:dialyzer]) do
      [] ->
        :ok
      warnings ->
        flunk IO.chardata_to_string(for warn <- warnings, do: [:dialyzer.format_warning(warn), ?\n])
    end
  end
end
