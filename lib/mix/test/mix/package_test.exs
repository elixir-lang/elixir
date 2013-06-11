Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.PackageTest do
  use MixTest.Case

  setup_all do
    defmodule PackageProject do
      def project do
        [ app: :fixtures ]
      end
    end
    Mix.Project.push(PackageProject)
    File.rm_rf! tmp_path("userhome")
    System.put_env "MIX_HOME", tmp_path("userhome/.mix")
  end

  test "create package" do
    archive_path = Path.join([Mix.Utils.mix_home,"tasks"])
    package = Path.join(archive_path, "fixtures.ez")
    Mix.Package.create_package("test/fixtures", archive_path)
    assert File.exists?(package)
    assert has_zip_file?(package, 'fixtures/priv/not_really_an.so')
    assert has_zip_file?(package, 'fixtures/ebin/Elixir.Mix.Tasks.Local.Sample.beam')
  end

  defp has_zip_file?(package, name) do
    :zip.list_dir(binary_to_list(package)) |> elem(1) 
    |> Enum.filter(fn(f) -> elem(f,0) == :zip_file end) 
    |> Enum.filter(fn(f) -> elem(f,1) == name end)
    |> Enum.count == 1
  end

  teardown_all do
    Mix.Project.pop
    :ok
  end
end
