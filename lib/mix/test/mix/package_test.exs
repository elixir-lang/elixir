Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.PackageTest do
  use MixTest.Case

  setup_all do
    defmodule PackageProject do
      def project do
        [ compile_path: "beams",
          app: :fixtures
        ]
      end
    end
    Mix.Project.push(PackageProject)
    File.rm_rf! tmp_path("userhome")
    System.put_env "MIX_HOME", tmp_path("userhome/.mix")
  end

  test "create package" do
    File.cd!("test/fixtures", fn() ->
      path = Path.join([Mix.Utils.mix_home,"tasks"])
      package = Path.join(path, "fixtures.ez")
      Mix.Package.create_package(path)
      assert File.exists?(package)
      Mix.Package.package_beams(package)
      |> Enum.first == "fixtures/beams/Elixir.Mix.Tasks.Local.Sample"
    end)
  end

  teardown_all do
    Mix.Project.pop
    :ok
  end
end
