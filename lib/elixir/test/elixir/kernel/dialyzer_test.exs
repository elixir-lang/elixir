Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.DialyzerTest do
  use ExUnit.Case, async: true

  @moduletag :dialyzer
  import PathHelpers

  setup_all do
    dir = tmp_path("dialyzer")
    File.rm_rf!(dir)
    File.mkdir_p!(dir)

    plt =
      dir
      |> Path.join("base_plt")
      |> String.to_char_list()

    # Add a few key elixir modules for types
    files = Enum.map([Kernel, String, Keyword, Exception], &:code.which/1)
    :dialyzer.run([analysis_type: :plt_build, output_plt: plt,
                   apps: [:erts], files: files])

    # Compile dialyzer fixtures
    assert '' = elixirc("#{fixture_path("dialyzer")} -o #{dir}")

    {:ok, [base_dir: dir, base_plt: plt]}
  end

  setup context do
    # Set up a per-test temporary directory, so we can run these with async: true.
    # We use the test's line number as the directory name, so they won't conflict.
    dir =
      context[:base_dir]
      |> Path.join("line#{context[:line]}")
      |> String.to_char_list()
    File.mkdir_p!(dir)

    plt =
      dir
      |> Path.join("plt")
      |> String.to_char_list()
    File.cp!(context[:base_plt], plt)

    dialyzer = [analysis_type: :succ_typings, check_plt: false,
                files_rec: [dir], plts: [plt]]

    {:ok, [outdir: dir, dialyzer: dialyzer]}
  end

  test "no warnings on valid remote calls", context do
    copy_beam! context, Dialyzer.RemoteCall
    assert_dialyze_no_warnings! context
  end

  test "no warnings on rewrites", context do
    copy_beam! context, Dialyzer.Rewrite
    assert_dialyze_no_warnings! context
  end

  test "no warnings on raise", context do
    copy_beam! context, Dialyzer.Raise
    assert_dialyze_no_warnings! context
  end

  defp copy_beam!(context, module) do
    name = "#{module}.beam"
    File.cp! Path.join(context[:base_dir], name),
             Path.join(context[:outdir], name)
  end

  defp assert_dialyze_no_warnings!(context) do
    case :dialyzer.run(context[:dialyzer]) do
      [] ->
        :ok
      warnings ->
        flunk IO.chardata_to_string(for warn <- warnings, do: [:dialyzer.format_warning(warn), ?\n])
    end
  end
end
