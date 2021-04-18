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

    setup %{tmp_dir: tmp_dir} do
      System.put_env("MIX_INSTALL_DIR", Path.join(tmp_dir, "installs"))
    end

    setup :test_project

    test "default options", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      assert File.dir?(Path.join(tmp_dir, "installs"))

      assert Protocol.consolidated?(InstallTest.Protocol)

      assert_received {:mix_shell, :info, ["==> install_test"]}
      assert_received {:mix_shell, :info, ["Compiling 1 file (.ex)"]}
      assert_received {:mix_shell, :info, ["Generated install_test app"]}
      refute_received _

      assert List.keyfind(Application.started_applications(), :install_test, 0)
      assert apply(InstallTest, :hello, []) == :world
    end

    test "call with same deps in the same VM", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])
    end

    test "can't call with Elixir version mismatch", %{tmp_dir: tmp_dir} do
      assert_raise Mix.Error, ~r"Mix.install/2 declared it supports only Elixir ~> 2.0", fn ->
        Mix.install(
          [
            {:install_test, path: Path.join(tmp_dir, "install_test")}
          ],
          elixir: "~> 2.0"
        )
      end
    end

    test "can't call with same deps and force", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      assert_raise Mix.Error, ~r"Mix.install/2 can only be called", fn ->
        Mix.install(
          [
            {:install_test, path: Path.join(tmp_dir, "install_test")}
          ],
          force: true
        )
      end
    end

    test "can't call with different deps in the same VM", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      assert_raise Mix.Error, ~r"Mix.install/2 can only be called", fn ->
        Mix.install([
          {:install_test, path: Path.join(tmp_dir, "install_test")},
          :foo
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

      Mix.State.put(:installed, nil)

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
