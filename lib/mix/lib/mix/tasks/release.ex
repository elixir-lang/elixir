defmodule Mix.Tasks.Release do
  @moduledoc """
  Assembles a release for the current project:

      mix release
      mix release NAME

  Once a release is assembled, it can be packaged and deployed to a
  remote host, as long as the remote host runs on the same operating
  system (OS) and versions as the machine running the `mix release`
  command.

  A release can be configured in your `mix.exs` file under the releases
  key inside `def project`:

      def project do
        [
          releases: [
            demo: [
              include_executables_for: [:unix],
              applications: [runtime_tools: :permanent]
            ],

            ...
          ]
        ]
      end

  Each key represents the release name. Releasing a certain name is done
  with:

      mix release demo

  If the given name does not exist, an error is raised.

  If `mix release`, without a name, is invoked and there are multiple names,
  the first name is used. If `mix release` is invoked and there are no names,
  a release using the application name and default values is assembled.

  ## Deployments

  It is very important that a release is assembled on a machine with the
  same operating system (OS) version and tooling as the machine the system
  is meant to run on.

  There are a couple ways this can be achieved. A simple option is to
  fetch the source, compile the code and assemble the release on the
  production machine itself. It would be something like this:

      git clone remote://path/to/my_app.git my_app_source
      cd my_app_source
      mix deps.get --only prod
      MIX_ENV=prod mix release
      _build/prod/rel/my_app/bin/start

  If you prefer, you can also compile the release to a separate directory,
  so you can erase all source after the release is assembled:

      git clone remote://path/to/my_app.git my_app_source
      cd my_app_source
      mix deps.get --only prod
      MIX_ENV=prod mix release --path ../my_app_release
      cd ../my_app_release
      rm -rf ../my_app_source
      bin/start

  However, this option can be expensive if you have multiple nodes or
  if the release assembling process is a long one, as each node needs
  to individually assemble the release.

  You can automate this process in a couple different ways. One option
  is to make it part of your Continuous Integration (CI) / Continuous
  Deployment (CD) pipeline. When you have a CI/CD pipeline, it is common
  that the machines in your CI/CD pipeline run on the exact same operating
  system and tooling than your production servers (if they don't, they should).
  In this case, you can assemble the release at the end of your CI/CD
  pipeline and push it to S3 or any other network storage. To perform the
  deployment, your production machines can fetch the deployment from the
  network storage.

  Another mechanism to automate deployments is to use images, such as
  Amazon Machine Images, or container platforms, such as Docker.
  For instance, you can use Docker to locally run the same operating
  system that you have in your production servers, allowing you to
  assemble a release or build a container or image locally, which is
  then pushed to production. One can also build images and/or containers
  at the end of their CI/CD pipelines.

  In other words, there are multiple ways systems can be deployed and
  releases can be automated and incorporated into all of them as long
  as you remember that the release must be assembled in the same
  operating system and operating system version as the production server.

  Once a system is deployed, shutting down said systems can be done by
  sending SIGINT/SIGTERM to the system, which is what most containers,
  platforms and tools do, or by explicitly invoking `bin/RELEASE_NAME stop`.
  Shutting down the system consists of stopping each application and
  their respective supervisiont trees, one by one, in the opposite order
  that they were started.

  ## Options

  Inside each release, here are the built-in options:

    * `:applications` - a keyword list that configures and adds new applications
      to the release. The key is the application name and the value is one of:

        * `:permanent` - the application is started and the node shuts down
          if the application terminates, regardless of reason
        * `:transient` - the application is started and the node shuts down
          if the application terminates abnormally
        * `:temporary` - the application is started and the node does not
          shut down if the application terminates
        * `:load` - the application is only loaded
        * `:none` - the application is part of the release but it isn't loaded nor
          started

      All applications default to `:permanent`. You can use this configuration
      to change the mode of an existing application or to add new application.
      If a release is assembled from an umbrella project, `:applications` is required.

    * `:strip_beams` - a boolean that controls if BEAM files should have their debug
      information, documentation chunks and other non-essential metadata removed.
      Defaults to `true`.

    * `:include_executables_for` - a list of atoms with executables to include on
      the release

    * `:cookie` - a string representing the Erlang Distribution cookie. If no cookie
      is set, one is automatically generated when the first release is assembled. The
      cookie will be written to `releases/COOKIE` and shared across multiple release
      versions. If you are setting this option manually, we recommend the cookie option
      to be a long and randomly generated, such as:
      `Base.url_encode64(:crypto.strong_rand_bytes(40))`

    * `:path` - the path the release should be installed to.
      Defaults to `_build/MIX_ENV/rel/RELEASE_NAME`.

    * `:version` - the release version as a string. Defaults to the current
      application version.

    * `:force` - a boolean that controls if conflicting operations should be
      always performed, without asking for confirmation before. Defaults to `false`.

    * `:quiet` - a boolean that controls if releases should write to the standard
      output its steps. Defaults to `false`.

    * `:include_erts` - a boolean indicating if the Erlang Runtime System (ERTS)
      should be included in the release. The default is `true`, which is also the
      recommended value. It may also be a path to an existing ERTS installation.
      You may also set it to `false` if you desire to use the ERTS version installed
      on the target. Note, however, the ERTS version on the target must have THE EXACT
      VERSION as the ERTS version used when the release is assembled. Setting it to
      `false` also disables hot code upgrades. Therefore, `:include_erts` should be
      set to `false` with caution and only if you are assembling the release on the
      same server that runs it.

  ## Command line options

    * `--force` - forces files to be overridden
    * `--quiet` - do not write progress to the standard output
    * `--path` - the path of the release
    * `--version` - the version of the release
    * `--no-archives-check` - does not check archive
    * `--no-deps-check` - does not check dependencies
    * `--no-elixir-version-check` - does not check Elixir version
    * `--no-compile` - does not compile before assembling the release

  ## Umbrellas

  Releases are well integrated with umbrella projects, allowing you to
  release multiple subsets of your umbrellas child. The only difference
  between performing a release in the umbrella project compared to a
  regular application is that umbrellas require you to explicitly list
  your release and the starting point for each release. For example,
  imagine this umbrella applications:

      my_app_umbrella/
        apps/
          my_app_core/
          my_app_event_processing/
          my_app_web/

  where both `my_app_event_processing` and `my_app_web` depend on
  `my_app_core` but they do not depend on each other.

  Inside your umbrella, you can define multiple releases:

      releases: [
        web_and_event_processing: [
          applications: [
            my_app_event_processing: :permanent,
            my_app_web: :permanent
          ]
        ],

        web_only: [
          applications: [my_app_web: :permanent]
        ],

        event_processing_only: [
          applications: [my_app_event_processing: :permanent]
        ]
      ]

  Note you don't need to define all applications in `:applications`,
  only the entry points. Also remember that the recommended mode
  for all applications in the system is `:permanent`.

  Finally, keep in mind it is not required for you to assemble the
  release from the umbrella root. You can also assemble the release
  from each child application individually. Doing it from the root,
  however, allows you to include two applications that do not depend
  on each other as part of the same release.

  ## Build-time configuration and runtime configuration

  All of the configuration in your `config` directory is computed
  when the release is **assembled**. Therefore, if your configuration
  file does something like:

      config :my_app, :secret_key, System.get_env("MY_APP_SECRET_KEY")

  Releases will read said environment variables when the release is
  assembled and on the machine the release is assembled, and not before
  boot in your production servers. That's because the `config` directory
  is about build-time configuration.

  TODO: Implement runtime configuration and config providers.

  ## Steps

  TODO: Implement :steps.

  ## vm.args

  TODO: Implement custom vm.args.

  ## Internals

  This section covers some details about how releases are assembled.

  ### Structure of a release

  A release is orgnized as follows:

      bin/
        RELEASE_NAME
        start
      erts-ERTS_VSN/
      lib/
        APP_VSN/
          ebin/
          include/
          priv/
      releases
        RELEASE_VSN/
          elixir
          iex
          remote.boot
          remote.script
          start.boot
          start.script
          sys.config
          vm.args
        COOKIE
        start_erl.data

  ### Environment variables

  The system running under releases has the following environment variables set:

    * `RELEASE_ROOT` - points to the root of the release. In the system
      includes ERTS, then it is the same as `:code.root_dir/0`

    * `RELEASE_NAME` - the name of the release. It can be overridden when
      the release is started

    * `RELEASE_VSN` - the version of the release. It can be overridden when
      the release is started

    * `COOKIE` - the release COOKIE. It can be overridden when the release
      is started
  """

  # v0.1
  # All other pending TODOs
  # TODO: Test protocol consolidation and environment variables

  use Mix.Task
  import Mix.Generator

  @remote_apps [:kernel, :stdlib, :iex, :elixir, :compiler, :logger]

  @switches [
    force: :boolean,
    quiet: :boolean,
    path: :string,
    version: :string,
    compile: :boolean,
    deps_check: :boolean,
    archives_check: :boolean,
    elixir_version_check: :boolean
  ]

  @aliases [
    f: :force
  ]

  @impl true
  def run(args) do
    Mix.Project.get!()
    config = Mix.Project.config()
    Mix.Task.run("loadpaths", args)

    unless "--no-compile" in args do
      Mix.Project.compile(args, config)
    end

    release =
      case OptionParser.parse!(args, strict: @switches, aliases: @aliases) do
        {overrides, [name]} -> Mix.Release.from_config!(name, config, overrides)
        {overrides, []} -> Mix.Release.from_config!(nil, config, overrides)
        {_, _} -> Mix.raise("Expected `mix release` or `mix release NAME`")
      end

    if not File.exists?(release.version_path) or
         yes?(release, "Release #{release.name}-#{release.version} already exists. Override?") do
      assemble(release)

      unless release.options[:quiet] do
        announce(release)
      end
    end
  end

  defp yes?(release, message) do
    release.options[:force] or Mix.shell().yes?(message)
  end

  defp assemble(release) do
    # releases/
    #   VERSION/
    #     consolidated/
    #     NAME.rel
    #     remote.boot
    #     remote.script
    #     start.boot
    #     start.script
    #     sys.config
    build_rel(release)

    # releases/
    #   COOKIE
    #   start_erl.data
    build_meta(release)

    [
      # bin/
      #   RELEASE_NAME
      #   RELEASE_NAME.bat
      #   start
      #   start.bat
      # releases/
      #   VERSION/
      #     elixir
      #     elixir.bat
      #     iex
      #     iex.bat
      :executables,
      # erts-VSN/
      :erts,
      # releases/VERSION/consolidated
      :consolidated
      # lib/APP_NAME-APP_VSN/
      | release.applications
    ]
    |> Task.async_stream(&copy(&1, release), ordered: false, timeout: :infinity)
    |> Stream.run()
  end

  # build_rel and build_meta

  defp build_rel(release) do
    File.rm_rf!(release.version_path)
    File.mkdir_p!(release.version_path)
    variables = build_variables()

    with :ok <- build_sys_config(release),
         :ok <- build_vm_args(release),
         :ok <- build_release_rel(release, variables),
         :ok <- build_remote_rel(release, variables) do
      if release.consolidation_source do
        rewrite_rel_script_with_consolidated(release)
      else
        rename_rel_script(release)
      end
    else
      {:error, message} ->
        File.rm_rf!(release.version_path)
        Mix.raise(message)
    end
  end

  defp build_variables do
    erts_dir = :code.lib_dir()

    for path <- :code.get_path(),
        path != '.',
        not List.starts_with?(path, erts_dir),
        uniq: true,
        do: {'RELEASE_LIB', path |> :filename.dirname() |> :filename.dirname()}
  end

  defp build_vm_args(release) do
    File.write!(Path.join(release.version_path, "vm.args"), vm_args_text())
    :ok
  end

  defp build_sys_config(release) do
    contents =
      if release.config_source do
        release.config_source |> Mix.Config.eval!() |> elem(0)
      else
        []
      end

    sys_config = Path.join(release.version_path, "sys.config")
    File.write!(sys_config, consultable("config", contents))

    case :file.consult(sys_config) do
      {:ok, _} ->
        :ok

      {:error, reason} ->
        {:error, "Could not write configuration file. Reason: #{inspect(reason)}"}
    end
  end

  defp build_release_rel(release, variables) do
    rel_path = Path.join(release.version_path, "#{release.name}.rel")
    build_rel_boot_and_script(rel_path, release, release.applications, variables)
  end

  defp build_remote_rel(release, variables) do
    remote_apps = for app <- release.applications, elem(app, 0) in @remote_apps, do: app
    rel_path = Path.join(release.version_path, "remote.rel")
    result = build_rel_boot_and_script(rel_path, release, remote_apps, variables)
    File.rm(rel_path)
    result
  end

  defp build_rel_boot_and_script(rel_path, release, apps, variables) do
    %{name: name, version: version, erts_version: erts_version} = release
    rel_spec = {:release, {to_charlist(name), to_charlist(version)}, {:erts, erts_version}, apps}
    File.write!(rel_path, consultable("rel", rel_spec))

    sys_path = rel_path |> Path.rootname() |> to_charlist()
    sys_options = [:silent, :no_dot_erlang, :no_warn_sasl, variables: variables]

    case :systools.make_script(sys_path, sys_options) do
      {:ok, _module, _warnings} ->
        :ok

      {:error, module, info} ->
        {:error, module.format_error(info) |> to_string() |> String.trim()}
    end
  end

  defp rewrite_rel_script_with_consolidated(release) do
    consolidated = '$RELEASE_LIB/../releases/#{release.version}/consolidated'

    {:ok, [{:script, rel_info, instructions}]} =
      :file.consult(Path.join(release.version_path, "#{release.name}.script"))

    new_instructions =
      Enum.map(instructions, fn
        {:path, paths} ->
          if Enum.any?(paths, &List.starts_with?(&1, '$RELEASE_LIB')) do
            {:path, [consolidated | paths]}
          else
            {:path, paths}
          end

        other ->
          other
      end)

    script = {:script, rel_info, new_instructions}
    File.write!(Path.join(release.version_path, "start.script"), consultable("script", script))
    :ok = :systools.script2boot(to_charlist(Path.join(release.version_path, "start")))
  after
    File.rm(Path.join(release.version_path, "#{release.name}.script"))
    File.rm(Path.join(release.version_path, "#{release.name}.boot"))
  end

  defp rename_rel_script(release) do
    for ext <- [:boot, :script] do
      File.rename!(
        Path.join(release.version_path, "#{release.name}.#{ext}"),
        Path.join(release.version_path, "start.#{ext}")
      )
    end

    :ok
  end

  defp consultable(kind, term) do
    {date, time} = :erlang.localtime()
    args = [kind, date, time, term]
    :io_lib.format("%% coding: utf-8~n%% ~ts generated at ~p ~p~n~p.~n", args)
  end

  defp build_meta(release) do
    cookie_path = Path.join(release.path, "releases/COOKIE")

    # TODO: If there is a cookie option and the cookie option
    # is not the same as the file, ask to override.
    unless File.exists?(cookie_path) do
      File.write!(cookie_path, random_cookie())
    end

    start_erl_path = Path.join(release.path, "releases/start_erl.data")
    File.write!(start_erl_path, "#{release.erts_version} #{release.version}")
    :ok
  end

  defp random_cookie, do: Base.url_encode64(:crypto.strong_rand_bytes(40))

  defp announce(release) do
    path = Path.relative_to_cwd(release.path)
    cmd = "#{path}/bin/#{release.name}"

    Mix.shell().info([:green, "Release created at #{path}!"])

    Mix.shell().info("""

        # To start your system
        #{path}/bin/start

    Check bin/start for more information. Once the release is running:

        # To connect to it remotely
        #{cmd} remote

        # To stop it gracefully (you may also send SIGINT/SIGTERM)
        #{cmd} stop

        # To list all commands
        #{cmd}
    """)
  end

  ## Copy operations

  defp copy(app_spec, release) when is_tuple(app_spec) do
    Mix.Release.copy_app(release, app_spec)
  end

  defp copy(:erts, release) do
    _ = Mix.Release.copy_erts(release)
    :ok
  end

  defp copy(:consolidated, release) do
    if consolidation_source = release.consolidation_source do
      consolidation_target = Path.join(release.version_path, "consolidated")
      _ = Mix.Release.copy_ebin(release, consolidation_source, consolidation_target)
    end

    :ok
  end

  defp copy(:executables, release) do
    elixir_bin_path = Application.app_dir(:elixir, "../../bin")
    bin_path = Path.join(release.path, "bin")
    File.mkdir_p!(bin_path)

    for os <- Keyword.get(release.options, :include_executables_for, [:unix, :windows]) do
      [{start, contents} | clis] = cli_for(os, release)
      start_path = Path.join(bin_path, start)

      unless File.exists?(start_path) do
        File.write!(start_path, contents)
        executable!(start_path)
      end

      for {filename, contents} <- clis do
        path = Path.join(bin_path, filename)
        File.write!(path, contents)
        executable!(path)
      end

      unless File.exists?(elixir_bin_path) do
        Mix.raise("Could not find bin files from Elixir installation")
      end

      for {filename, contents} <- elixir_cli_for(os, elixir_bin_path, release) do
        path = Path.join(release.version_path, filename)
        File.write!(path, contents)
        executable!(path)
      end
    end
  end

  # TODO: Implement windows CLI
  defp cli_for(_os, release) do
    [
      {"start", start_template(name: release.name)},
      {"#{release.name}", cli_template(name: release.name)}
    ]
  end

  defp elixir_cli_for(:unix, bin_path, release) do
    [
      {"elixir",
       Path.join(bin_path, "elixir")
       |> File.read!()
       |> String.replace(~s[ -pa "$SCRIPT_PATH"/../lib/*/ebin], "")
       |> replace_erts_bin(release, ~s["$SCRIPT_PATH"/../../erts-#{release.erts_version}/bin/])},
      {"iex", File.read!(Path.join(bin_path, "iex"))}
    ]
  end

  defp elixir_cli_for(:windows, bin_path, release) do
    [
      {"elixir.bat",
       Path.join(bin_path, "elixir.bat")
       |> File.read!()
       |> String.replace(~s[goto expand_erl_libs], ~s[goto run])
       |> replace_erts_bin(release, ~s["%dp0\\..\\..\\erts-#{release.erts_version}\\bin\\"])},
      {"iex.bat", File.read!(Path.join(bin_path, "iex"))}
    ]
  end

  defp executable!(path), do: File.chmod!(path, 0o744)

  defp replace_erts_bin(contents, release, new_path) do
    if release.erts_source do
      String.replace(contents, ~s[ERTS_BIN=""], ~s[ERTS_BIN=#{new_path}])
    else
      contents
    end
  end

  ## Templates

  embed_text(:vm_args, ~S"""
  ## Do not load code from filesystem as all modules are preloaded
  -mode embedded

  ## Disable the heartbeat system to automatically restart the VM
  ## if it dies or becomes unresponsive. Useful only in daemon mode.
  ##-heart

  ## Number of diry schedulers doing IO work (file, sockets, etc)
  ##+SDio 5

  ## Increase number of concurrent ports/sockets
  ##-env ERL_MAX_PORTS 4096

  ## Tweak GC to run more often
  ##-env ERL_FULLSWEEP_AFTER 10
  """)

  embed_template(:start, ~S"""
  #!/bin/sh
  set -e
  # Feel free to edit this file in anyway you want
  # To start your system using IEx: . $(dirname "$0")/<%= @name %> start iex
  # To start it as a daemon using IEx: . $(dirname "$0")/<%= @name %> daemon iex
  . $(dirname "$0")/<%= @name %> start
  """)

  embed_template(:cli, ~S"""
  #!/bin/sh
  set -e

  SELF=$(readlink "$0" || true)
  if [ -z "$SELF" ]; then SELF="$0"; fi
  export RELEASE_ROOT="$(cd "$(dirname "$SELF")/.." && pwd -P)"
  export RELEASE_NAME="${RELEASE_NAME:-"<%= @name %>"}"
  export RELEASE_VSN="${RELEASE_VSN:-"$(cut -d' ' -f2 "$RELEASE_ROOT/releases/start_erl.data")"}"
  export COOKIE=${COOKIE:-$(cat "$RELEASE_ROOT/releases/COOKIE")}
  REL_VSN_DIR="$RELEASE_ROOT/releases/$RELEASE_VSN"

  gen_id () {
    od -t x -N 4 /dev/urandom | head -n1 | awk '{print $2}'
  }

  rpc () {
    exec "$REL_VSN_DIR/elixir" \
         --hidden --name "rpc-$(gen_id)@127.0.0.1" --cookie "$COOKIE" \
         --boot "${REL_VSN_DIR}/remote" \
         --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
         --rpc-eval "$RELEASE_NAME@127.0.0.1" "$1"
  }

  start () {
    exec "$REL_VSN_DIR/$1" --no-halt \
         --werl --name "$RELEASE_NAME@127.0.0.1" --cookie "$COOKIE" \
         --erl-config "${REL_VSN_DIR}/sys" \
         --boot "${REL_VSN_DIR}/start" \
         --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
         --vm-args "${REL_VSN_DIR}/vm.args" "${@:2}"
  }

  case $1 in
    start)
      start ${2:-elixir}
      ;;

    daemon)
      export RELEASE_TMP="${RELEASE_TMP:-"$RELEASE_ROOT/tmp"}"
      start ${2:-elixir} --pipe-to "${RELEASE_TMP}/pipe" "${RELEASE_TMP}/log"
      ;;

    remote)
      exec "$REL_VSN_DIR/iex" \
           --werl --hidden --name "remote-$(gen_id)@127.0.0.1" --cookie "$COOKIE" \
           --boot "${REL_VSN_DIR}/remote" \
           --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
           --remsh "$RELEASE_NAME@127.0.0.1"
      ;;

    rpc)
      if [ -z "$2" ]; then
        echo "ERROR: RPC expects an expression as argument" >&2
        exit 1
      fi
      rpc "$2"
      ;;

    restart|stop)
      rpc "System.$1"
      ;;

    pid)
      rpc "IO.puts System.pid"
      ;;

    *)
      echo "Usage: $(basename $0) COMMAND [ARGS]

  The known commands are:

    - start [elixir | iex]
      Starts the system using elixir or iex.

    - daemon [elixir | iex]
      Starts the system as a deamon using elixir or iex.
      See the --pipe-to option in elixir --help for more info.

    - remote
      Connects to the currently running system via a remote shell.

    - rpc \"EXPR\"
      Executes the given expression remotely on the running system.

    - restart
      Restarts the running system via a remote command.

    - stop
      Stops the running system via a remote command.

    - pid
      Prints the OS PID of the running system via a remote command.
    " >&2

      if [ -n "$1" ]; then
        echo "ERROR: Unknown command $1" >&2
        exit 1
      fi
      ;;
  esac
  """)
end
