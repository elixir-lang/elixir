Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.ReleaseTest do
  use MixTest.Case

  test "ships a bootable release with ERTS" do
    in_fixture("release_test", fn ->
      Mix.Project.in_project(:release_test, ".", fn _ ->
        root = Path.absname("_build/dev/rel/release_test")
        Mix.Task.run("release")
        assert System.cmd(Path.join(root, "bin/start"), []) == {"", 0}

        assert Code.eval_file("RELEASE_BOOTED") |> elem(0) == %{
                 app_dir: Path.join(root, "lib/release_test-0.1.0"),
                 cookie: File.read!(Path.join(root, "releases/COOKIE")),
                 protocols_consolidated?: true,
                 release_name: "release_test",
                 release_root: root,
                 release_vsn: "0.1.0",
                 root_dir: root,
                 static_config: :was_set
               }
      end)
    end)
  end

  test "ships a bootable release without ERTS" do
    in_fixture("release_test", fn ->
      config = [releases: [demo: [include_erts: false]]]

      Mix.Project.in_project(:release_test, ".", config, fn _ ->
        root = Path.absname("_build/dev/rel/demo")
        Mix.Task.run("release")
        assert System.cmd(Path.join(root, "bin/start"), []) == {"", 0}

        assert Code.eval_file("RELEASE_BOOTED") |> elem(0) == %{
                 app_dir: Path.join(root, "lib/release_test-0.1.0"),
                 cookie: File.read!(Path.join(root, "releases/COOKIE")),
                 protocols_consolidated?: true,
                 release_name: "demo",
                 release_root: root,
                 release_vsn: "0.1.0",
                 root_dir: :code.root_dir |> to_string(),
                 static_config: :was_set
               }
      end)
    end)
  end
end
