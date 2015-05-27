Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.ArchiveTest do
  use MixTest.Case

  doctest Mix.Archive

  test "archive" do
    in_fixture "archive", fn ->
      File.write ".elixir", "~> 1.0.0"
      Mix.Archive.create(".", "sample.ez")
      archive = 'sample.ez'
      assert File.exists?(archive)
      assert has_zip_file?(archive, 'sample/.elixir')
      assert has_zip_file?(archive, 'sample/priv/not_really_an.so')
      assert has_zip_file?(archive, 'sample/ebin/Elixir.Mix.Tasks.Local.Sample.beam')
      assert has_zip_file?(archive, 'sample/ebin/local_sample.app')
    end
  end

  defp has_zip_file?(archive, name) do
    {:ok, files} = :zip.list_dir(archive)
    Enum.find(files, &match?({:zip_file, ^name, _, _, _, _}, &1))
  end
end
