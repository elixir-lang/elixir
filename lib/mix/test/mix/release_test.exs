Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.ReleaseTest do
  use MixTest.Case

  import Mix.Release
  doctest Mix.Release

  describe "from_config!/3" do
    test "uses default configuration if no release is specified" do
      assert %Mix.Release{
               name: :mix,
               version: "0.1.0",
               config_source: nil,
               consolidation_source: nil,
               path: path,
               version_path: version_path
             } = from_config!(nil, config(), [])

      assert String.ends_with?(path, "from_config/_build/dev/rel/mix")
      assert String.ends_with?(version_path, "from_config/_build/dev/rel/mix/releases/0.1.0")
    end

    test "checks for config" do
      config = tmp_path("from_config/custom.exs")
      File.mkdir_p!(Path.dirname(config))
      File.write!(config, "[]")

      release = from_config!(nil, config(config_path: config), [])
      assert String.ends_with?(release.config_source(), "from_config/custom.exs")
    end

    test "checks for consolidation" do
      release = from_config!(nil, config(consolidate_protocols: true), [])

      assert String.ends_with?(
               release.consolidation_source(),
               "from_config/_build/dev/lib/mix/consolidated"
             )
    end

    test "provides default options" do
      release = from_config!(nil, config(), [])
      assert release.options == [force: false, quiet: false, strip_beams: true]
    end

    test "allows overrides" do
      overrides = [path: "demo", version: "0.2.0", force: true, quiet: true]
      release = from_config!(nil, config(), overrides)

      assert release.path == Path.absname("demo")
      assert release.version == "0.2.0"
      assert release.options[:force]
      assert release.options[:quiet]
    end

    test "uses first release available" do
      release =
        from_config!(
          nil,
          config(releases: [foo: [version: "0.2.0"], bar: [version: "0.3.0"]]),
          []
        )

      assert release.name == :foo
      assert release.version == "0.2.0"
      assert String.ends_with?(release.path, "from_config/_build/dev/rel/foo")

      assert String.ends_with?(
               release.version_path,
               "from_config/_build/dev/rel/foo/releases/0.2.0"
             )
    end

    test "uses choosen release" do
      release =
        from_config!(
          :bar,
          config(releases: [foo: [version: "0.2.0"], bar: [version: "0.3.0"]]),
          []
        )

      assert release.name == :bar
      assert release.version == "0.3.0"
      assert String.ends_with?(release.path, "from_config/_build/dev/rel/bar")

      assert String.ends_with?(
               release.version_path,
               "from_config/_build/dev/rel/bar/releases/0.3.0"
             )
    end

    test "raises for unknown release" do
      assert_raise Mix.Error, "Unknown release :foo. Found: []", fn ->
        from_config!(:foo, config(), [])
      end
    end

    test "raises for missing version" do
      assert_raise Mix.Error, ~r"No :version found", fn ->
        from_config!(nil, config() |> Keyword.drop([:version]), [])
      end
    end
  end

  describe "from_config!/3 + applications" do
    test "includes the current application" do
      release = from_config!(nil, config(), [])
      assert {:mix, _, :permanent} = List.keyfind(release.applications, :mix, 0)
      refute List.keyfind(release.applications, :eex, 0)
    end

    test "includes extra application" do
      release = from_config!(nil, release_config(applications: [eex: :permanent]), [])
      assert {:mix, _, :permanent} = List.keyfind(release.applications, :mix, 0)
      assert {:eex, _, :permanent} = List.keyfind(release.applications, :eex, 0)
    end

    test "configures other applications" do
      release = from_config!(nil, release_config(applications: [mix: :none]), [])
      assert {:mix, _, :none} = List.keyfind(release.applications, :mix, 0)
    end
  end

  describe "from_config!/3 + umbrella" do
    test "cannot infer for umbrella projects" do
      assert_raise Mix.Error,
                   ~r"Umbrella projects require releases to be explicitly defined",
                   fn -> from_config!(nil, config(apps_path: "apps"), []) end
    end

    test "requires apps for umbrella projects" do
      assert_raise Mix.Error,
                   ~r"Umbrella projects require releases to be explicitly defined",
                   fn -> from_config!(nil, config(apps_path: "apps", releases: [foo: []]), []) end
    end

    test "builds explicit releases with applications" do
      config = config(apps_path: "apps", releases: [foo: [applications: [mix: :permanent]]])

      assert %Mix.Release{
               name: :foo,
               version: "0.1.0",
               config_source: nil,
               consolidation_source: nil,
               path: path,
               version_path: version_path
             } = from_config!(nil, config, [])
    end
  end

  describe "from_config!/3 + include_erts" do
    @erts_version :erlang.system_info(:version)
    @erts_source Path.join(:code.root_dir(), "erts-#{@erts_version}")

    test "when true (default)" do
      release = from_config!(nil, config(), [])
      assert release.erts_version == @erts_version
      assert release.erts_source == to_charlist(@erts_source)
    end

    test "when false" do
      release = from_config!(nil, release_config(include_erts: false), [])
      assert release.erts_version == @erts_version
      assert release.erts_source == nil
    end

    test "with good path" do
      release = from_config!(nil, release_config(include_erts: @erts_source), [])
      assert release.erts_version == @erts_version
      assert release.erts_source == to_charlist(@erts_source)
    end

    test "with invalid path" do
      assert_raise Mix.Error, "Could not find ERTS system at \"bad\"", fn ->
        from_config!(nil, release_config(include_erts: "bad"), [])
      end
    end
  end

  defp release_config(config) do
    config(releases: [demo: config])
  end

  defp config(extra \\ []) do
    [
      app: :mix,
      version: "0.1.0",
      build_path: tmp_path("from_config/_build"),
      build_per_environment: true,
      config_path: tmp_path("from_config/config/unavailable.exs")
    ]
    |> Keyword.merge(extra)
  end
end
