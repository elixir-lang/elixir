Code.require_file("../../test_helper.exs", __DIR__)

defmodule IEx.Autocomplete.PathTest do
  use ExUnit.Case, async: true

  alias IEx.Autocomplete.Path, as: Subject

  defp expand(expr) do
    shell = IEx.Broker.shell()

    expr
    |> Enum.reverse()
    |> Subject.expand(shell)
  end

  defp expandable_fragment(expr) do
    expr
    |> Enum.reverse()
    |> Subject.expandable_fragment()
  end

  defp path_autocompletion(dir, hint \\ "") do
    dir
    |> File.ls!()
    |> Stream.filter(&String.starts_with?(&1, hint))
    |> Enum.map(&String.to_charlist/1)
    |> case do
      [] -> {:no, '', []}
      list -> {:yes, '', list}
    end
  end

  test "expandable_fragment/1" do
    assert '' == expandable_fragment('Enum.')
    assert '' == expandable_fragment('""')
    assert '' == expandable_fragment('".')
    assert '' == expandable_fragment('~s(')
    assert './' == expandable_fragment('"./')
    assert '/' == expandable_fragment('"/')
  end

  @tag :tmp_dir
  test "expand/2 with path completion inside strings", %{tmp_dir: dir} do
    dir |> Path.join("single1") |> File.touch()
    dir |> Path.join("file1") |> File.touch()
    dir |> Path.join("file2") |> File.touch()
    dir |> Path.join("dir") |> File.mkdir()
    dir |> Path.join("dir/file3") |> File.touch()
    dir |> Path.join("dir/file4") |> File.touch()

    assert expand('"./') == path_autocompletion(".")
    assert expand('"/') == path_autocompletion("/")
    assert expand('"./#\{') == expand('{')
    assert expand('"./#\{Str') == expand('{Str')
    assert expand('Path.join("./", is_') == expand('is_')

    assert expand('"#{dir}/') == path_autocompletion(dir)
    assert expand('"#{dir}/sin') == {:yes, 'gle1', []}
    assert expand('"#{dir}/single1') == {:yes, '"', []}
    assert expand('"#{dir}/fi') == {:yes, 'le', []}
    assert expand('"#{dir}/file') == path_autocompletion(dir, "file")
    assert expand('"#{dir}/d') == {:yes, 'ir/', []}
    assert expand('"#{dir}/dir') == {:yes, '/', []}
    assert expand('"#{dir}/dir/') == {:yes, 'file', []}
    assert expand('"#{dir}/dir/file') == dir |> Path.join("dir") |> path_autocompletion("file")
  end
end
