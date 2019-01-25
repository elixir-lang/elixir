Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.ReleaseTest do
  use MixTest.Case

  import Mix.Release
  doctest Mix.Release

  _ = Application.ensure_started(:eex)
  _ = Application.ensure_started(:runtime_tools)

  @erts_version :erlang.system_info(:version)
  @erts_source Path.join(:code.root_dir(), "erts-#{@erts_version}")
  @eex_ebin Application.app_dir(:eex, "ebin")
  @eex_version Application.spec(:eex, :vsn)
  @runtime_tools_version Application.spec(:runtime_tools, :vsn)

  setup do
    File.rm_rf!(tmp_path("mix_release"))
    :ok
  end

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

      assert String.ends_with?(path, "mix_release/_build/dev/rel/mix")
      assert String.ends_with?(version_path, "mix_release/_build/dev/rel/mix/releases/0.1.0")
    end

    test "checks for config" do
      config = tmp_path("mix_release/custom.exs")
      File.mkdir_p!(Path.dirname(config))
      File.write!(config, "[]")

      release = from_config!(nil, config(config_path: config), [])
      assert String.ends_with?(release.config_source(), "mix_release/custom.exs")
    end

    test "checks for consolidation" do
      release = from_config!(nil, config(consolidate_protocols: true), [])

      assert String.ends_with?(
               release.consolidation_source(),
               "mix_release/_build/dev/lib/mix/consolidated"
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

    test "uses chosen release via the CLI" do
      release =
        from_config!(
          :bar,
          config(releases: [foo: [version: "0.2.0"], bar: [version: "0.3.0"]]),
          []
        )

      assert release.name == :bar
      assert release.version == "0.3.0"
      assert String.ends_with?(release.path, "mix_release/_build/dev/rel/bar")

      assert String.ends_with?(
               release.version_path,
               "mix_release/_build/dev/rel/bar/releases/0.3.0"
             )
    end

    test "uses chosen release via the default_release" do
      release =
        from_config!(
          :bar,
          config(
            default_release: :bar,
            releases: [foo: [version: "0.2.0"], bar: [version: "0.3.0"]]
          ),
          []
        )

      assert release.name == :bar
      assert release.version == "0.3.0"
      assert String.ends_with?(release.path, "mix_release/_build/dev/rel/bar")

      assert String.ends_with?(
               release.version_path,
               "mix_release/_build/dev/rel/bar/releases/0.3.0"
             )
    end

    test "raises for multiple releases and no name" do
      assert_raise Mix.Error,
                   ~r"\"mix release\" was invoked without a name but there are multiple releases",
                   fn -> from_config!(nil, config(releases: [foo: [], bar: []]), []) end
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

    test "raises on invalid release names" do
      assert_raise Mix.Error, ~r"Invalid release name", fn ->
        from_config!(nil, config(releases: ["invalid name": []]), [])
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
      release = from_config!(nil, release_config(applications: [mix: :temporary]), [])
      assert {:mix, _, :temporary} = List.keyfind(release.applications, :mix, 0)
    end

    test "works when :load/:none is set at the leaf" do
      release = from_config!(nil, release_config(applications: [mix: :none]), [])
      assert {:mix, _, :none} = List.keyfind(release.applications, :mix, 0)
    end

    test "works when :load/:none is set at the subtree" do
      apps = [mix: :load, elixir: :load, iex: :load]
      release = from_config!(nil, release_config(applications: apps), [])
      assert {:elixir, _, :load} = List.keyfind(release.applications, :elixir, 0)
      assert {:iex, _, :load} = List.keyfind(release.applications, :iex, 0)
      assert {:mix, _, :load} = List.keyfind(release.applications, :mix, 0)
    end

    test "raises on unknown app" do
      assert_raise ArgumentError, "unknown application: :unknown", fn ->
        from_config!(nil, release_config(applications: [unknown: :none]), [])
      end
    end

    test "raises on unknown mode" do
      assert_raise Mix.Error, ~r"Unknown mode :what for :mix", fn ->
        from_config!(nil, release_config(applications: [mix: :what]), [])
      end
    end

    test "raises on bad load/none" do
      assert_raise Mix.Error,
                   ~r"application :kernel was set to mode :load but the application :stdlib depends on it",
                   fn -> from_config!(nil, release_config(applications: [kernel: :load]), []) end

      assert_raise Mix.Error,
                   ~r"application :elixir was set to mode :none but the application :mix depends on it",
                   fn -> from_config!(nil, release_config(applications: [elixir: :none]), []) end
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

  describe "copy_erts/1" do
    test "copies to directory" do
      assert copy_erts(release(include_erts: true))
      destination = tmp_path("mix_release/_build/dev/rel/demo/erts-#{@erts_version}")
      assert File.exists?(destination)

      assert File.read!(Path.join(destination, "bin/erl")) =~
               ~s|ROOTDIR="$(dirname "$(dirname "$BINDIR")")"|

      refute File.exists?(Path.join(destination, "bin/erl.ini"))
    end

    test "does not copy when include_erts is false" do
      refute copy_erts(release(include_erts: false))
      destination = tmp_path("mix_release/_build/dev/rel/demo/erts-#{@erts_version}")
      refute File.exists?(destination)
    end
  end

  describe "copy_ebin/3" do
    test "copies and strips beams" do
      assert copy_ebin(release([]), @eex_ebin, tmp_path("eex_ebin"))

      assert size!(Path.join(@eex_ebin, "eex.app")) ==
               size!(tmp_path("eex_ebin/eex.app"))

      assert size!(Path.join(@eex_ebin, "Elixir.EEx.beam")) >
               size!(tmp_path("eex_ebin/Elixir.EEx.beam"))
    end

    test "copies without stripping beams" do
      assert copy_ebin(release(strip_beams: false), @eex_ebin, tmp_path("eex_ebin"))

      assert size!(Path.join(@eex_ebin, "eex.app")) ==
               size!(tmp_path("eex_ebin/eex.app"))

      assert size!(Path.join(@eex_ebin, "Elixir.EEx.beam")) ==
               size!(tmp_path("eex_ebin/Elixir.EEx.beam"))
    end

    test "returns false for unknown or empty directories" do
      source = tmp_path("mix_release")
      refute copy_ebin(release([]), source, tmp_path("mix_release"))
      File.mkdir_p!(source)
      refute copy_ebin(release([]), source, tmp_path("mix_release"))
    end
  end

  describe "copy_app/2" do
    @eex_app {:eex, @eex_version, :permanent}
    @runtime_tools_app {:runtime_tools, @runtime_tools_version, :permanent}
    @release_lib tmp_path("mix_release/_build/dev/rel/demo/lib")

    test "copies and strips beams" do
      assert copy_app(release([]), @eex_app)

      assert size!(Path.join(@eex_ebin, "eex.app")) ==
               size!(Path.join(@release_lib, "eex-#{@eex_version}/ebin/eex.app"))

      assert size!(Path.join(@eex_ebin, "Elixir.EEx.beam")) >
               size!(Path.join(@release_lib, "eex-#{@eex_version}/ebin/Elixir.EEx.beam"))
    end

    test "copies without stripping beams" do
      assert copy_app(release(strip_beams: false), @eex_app)

      assert size!(Path.join(@eex_ebin, "eex.app")) ==
               size!(Path.join(@release_lib, "eex-#{@eex_version}/ebin/eex.app"))

      assert size!(Path.join(@eex_ebin, "Elixir.EEx.beam")) ==
               size!(Path.join(@release_lib, "eex-#{@eex_version}/ebin/Elixir.EEx.beam"))
    end

    test "copies OTP apps" do
      assert copy_app(release([]), @runtime_tools_app)
      assert File.exists?(Path.join(@release_lib, "runtime_tools-#{@runtime_tools_version}/ebin"))
      assert File.exists?(Path.join(@release_lib, "runtime_tools-#{@runtime_tools_version}/priv"))
    end

    test "does not copy OTP app if include_erts is  false" do
      refute copy_app(release(include_erts: false), @runtime_tools_app)
      refute File.exists?(Path.join(@release_lib, "runtime_tools-#{@runtime_tools_version}/ebin"))
      refute File.exists?(Path.join(@release_lib, "runtime_tools-#{@runtime_tools_version}/priv"))
    end
  end

  describe "strip_beam/1" do
    test "excludes at least docs and dbgi chunks" do
      {:ok, beam} =
        Path.join(@eex_ebin, "Elixir.EEx.beam")
        |> File.read!()
        |> strip_beam()

      assert {:error, :beam_lib, {:missing_chunk, _, 'Dbgi'}} = :beam_lib.chunks(beam, ['Dbgi'])
      assert {:error, :beam_lib, {:missing_chunk, _, 'Docs'}} = :beam_lib.chunks(beam, ['Docs'])
    end
  end

  describe "copy_cookie/1" do
    @release_root tmp_path("mix_release/_build/dev/rel/demo")

    test "creates a random cookie if no cookie" do
      assert copy_cookie(release([]), "COOKIE")
      assert byte_size(File.read!(Path.join(@release_root, "COOKIE"))) == 56
    end

    test "uses the given cookie" do
      release = release(cookie: "abcdefghijk")
      assert copy_cookie(release, "COOKIE")
      assert File.read!(Path.join(@release_root, "COOKIE")) == "abcdefghijk"
      refute copy_cookie(release, "COOKIE")
    end

    test "asks to change if the cookie changes" do
      assert copy_cookie(release(cookie: "abcdefghijk"), "COOKIE")

      send(self(), {:mix_shell_input, :yes?, false})
      refute copy_cookie(release(cookie: "lmnopqrstuv"), "COOKIE")
      assert File.read!(Path.join(@release_root, "COOKIE")) == "abcdefghijk"

      send(self(), {:mix_shell_input, :yes?, true})
      assert copy_cookie(release(cookie: "lmnopqrstuv"), "COOKIE")
      assert File.read!(Path.join(@release_root, "COOKIE")) == "lmnopqrstuv"
    end
  end

  describe "copy_start_erl/1" do
    @release_root tmp_path("mix_release/_build/dev/rel/demo")

    test "writes erts and release versions" do
      assert copy_start_erl(release([]), "start_erl.data")
      assert File.read!(Path.join(@release_root, "start_erl.data")) == "#{@erts_version} 0.1.0"
    end
  end

  defp size!(path) do
    File.stat!(path).size
  end

  defp release(config) do
    from_config!(nil, release_config(config), [])
  end

  defp release_config(config) do
    config(releases: [demo: config])
  end

  defp config(extra \\ []) do
    [
      app: :mix,
      version: "0.1.0",
      build_path: tmp_path("mix_release/_build"),
      build_per_environment: true,
      config_path: tmp_path("mix_release/config/config.exs")
    ]
    |> Keyword.merge(extra)
  end
end
