Code.require_file("test_helper.exs", __DIR__)

defmodule MixTest do
  use MixTest.Case

  test "shell" do
    assert Mix.shell() == Mix.Shell.Process
  end

  test "env" do
    assert Mix.env() == :dev
    Mix.env(:prod)
    assert Mix.env() == :prod
  end

  test "debug" do
    refute Mix.debug?()
    Mix.debug(true)
    assert Mix.debug?()
    Mix.debug(false)
  end

  describe "install" do
    @describetag :tmp_dir
    setup :test_project

    test "default options", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      assert Protocol.consolidated?(InstallTest.Protocol)

      assert_received {:mix_shell, :info, ["==> install_test"]}
      assert_received {:mix_shell, :info, ["Compiling 1 file (.ex)"]}
      assert_received {:mix_shell, :info, ["Generated install_test app"]}
      refute_received _

      assert List.keyfind(Application.started_applications(), :install_test, 0)
      assert apply(InstallTest, :hello, []) == :world
    end

    test "can't call twice in the same VM", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      assert_raise Mix.Error, "Mix.install/2 can only be called once", fn ->
        Mix.install([
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ])
      end
    end

    test "consolidate_protocols: false", %{tmp_dir: tmp_dir} do
      Mix.install(
        [
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ],
        consolidate_protocols: false
      )

      refute Protocol.consolidated?(InstallTest.Protocol)
    end

    defp test_project(%{tmp_dir: tmp_dir}) do
      path = :code.get_path()

      on_exit(fn ->
        :code.set_path(path)
        purge([InstallTest, InstallTest.MixProject, InstallTest.Protocol])
        Application.stop(:install_test)
        Application.unload(:install_test)
      end)

      Mix.State.put(:install_called?, false)

      File.mkdir_p!("#{tmp_dir}/install_test/lib")

      File.write!("#{tmp_dir}/install_test/mix.exs", """
      defmodule InstallTest.MixProject do
        use Mix.Project

        def project do
          [
            app: :install_test,
            version: "0.1.0"
          ]
        end
      end
      """)

      File.write!("#{tmp_dir}/install_test/lib/install_test.ex", """
      defmodule InstallTest do
        def hello do
          :world
        end
      end

      defprotocol InstallTest.Protocol do
        def foo(x)
      end
      """)

      [tmp_dir: tmp_dir]
    end
  end
end
