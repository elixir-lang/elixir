Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.ArchiveTest do
  use MixTest.Case

  doctest Mix.Archive

  test "archive" do
    in_fixture "archive", fn ->
      Mix.Archive.create("sample.ez")
      archive = 'sample.ez'
      assert File.exists?(archive)
      assert has_zip_file?(archive, 'sample/priv/not_really_an.so')
      assert has_zip_file?(archive, 'sample/ebin/Elixir.Mix.Tasks.Local.Sample.beam')
    end
  end

  defp has_zip_file?(archive, name) do
    :zip.list_dir(archive)
      |> elem(1)
      |> Enum.find(match?({ :zip_file, ^name, _, _, _, _ }, &1))
  end
end
