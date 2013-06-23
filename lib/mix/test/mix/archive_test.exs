Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.ArchiveTest do
  use MixTest.Case

  test "archive" do
    in_fixture "archive", fn ->
      Mix.Archive.create("test archive.ez")
      archive = 'test archive.ez'
      assert File.exists?(archive)
      assert has_zip_file?(archive, 'test archive/priv/not_really_an.so')
      assert has_zip_file?(archive, 'test archive/ebin/Elixir.Mix.Tasks.Local.Sample.beam')
    end
  end

  defp has_zip_file?(archive, name) do
    :zip.list_dir(archive)
      |> elem(1)
      |> Enum.find(match?({ :zip_file, ^name, _, _, _, _ }, &1))
  end
end
