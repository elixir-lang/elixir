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

  @tag :tmp_dir
  test "install", %{tmp_dir: tmp_dir} do
    tmp_dir = Path.expand(tmp_dir)
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
    """)

    Mix.install(
      [
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ],
      force: true
    )

    assert_received {:mix_shell, :info, ["==> install_test"]}
    assert_received {:mix_shell, :info, ["Compiling 1 file (.ex)"]}
    assert_received {:mix_shell, :info, ["Generated install_test app"]}
    refute_received _

    started_apps = Enum.map(Application.started_applications(), &elem(&1, 0))
    assert :install_test in started_apps
    assert apply(InstallTest, :hello, []) == :world
  after
    :code.purge(InstallTest)
    :code.delete(InstallTest)
    Application.stop(:install_test)
    Application.unload(:install_test)
  end
end
