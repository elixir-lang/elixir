Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.LocalInstallPackageTest do
  use MixTest.Case

  setup_all do
    defmodule PackageProject do
      def project do
        [ version: "0.0.1",
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
      # Install it!
      Mix.Tasks.Package.run ["--no_compile"] #otherwise ebin/*beam gets loaded!
      assert File.regular? "fixtures.ez"

      self <- { :mix_shell_input, :yes?, true }
      Mix.Tasks.Local.Install.run []
      assert File.regular? tmp_path("userhome/.mix/tasks/fixtures.ez")

      # List it!
      Mix.Local.append_tasks
      Mix.Tasks.Local.run []
      assert_received { :mix_shell, :info, ["mix local.sample # A local install sample"] }

      # Run it!
      Mix.Task.run "local.sample"
      assert_received { :mix_shell, :info, ["sample"] }

      # Try to remove it by task name!
      Mix.Tasks.Local.Uninstall.run ["local.sample"]
      assert_received { :mix_shell, :info, _ }
      assert File.regular? tmp_path("userhome/.mix/tasks/fixtures.ez")

      # Remove it for real!
      Mix.Tasks.Local.Uninstall.run ["fixtures"]
      refute File.regular? tmp_path("userhome/.mix/tasks/fixtures.ez")

      # Cleanup
      File.rm "fixtures.ez"
    end)
  end

  teardown_all do
    Mix.Project.pop
    :ok
  end
end
