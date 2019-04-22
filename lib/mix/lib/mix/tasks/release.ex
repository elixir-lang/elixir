defmodule Mix.Tasks.Release do
  use Mix.Task

  @shortdoc "Assembles a self-contained release"

  @moduledoc """
  Assembles a self-contained release for the current project:

      MIX_ENV=prod mix release
      MIX_ENV=prod mix release NAME

  Once a release is assembled, it can be packaged and deployed to a
  target, as long as the target runs on the same operating system (OS)
  distribution and version as the machine running the `mix release`
  command.

  A release can be configured in your `mix.exs` file under the `:releases`
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

  You can specify multiple releases where the key is the release name
  and the value is a keyword list with the release configuration.
  Releasing a certain name is done with:

      MIX_ENV=prod mix release demo

  If the given name does not exist, an error is raised.

  If `mix release`, without a name, is invoked and there are multiple names,
  an error will be raised unless you set `default_release: NAME` at the root
  of your project configuration.

  If `mix release` is invoked and there are no names, a release using the
  application name and default values is assembled.

  ## Running the release

  Once a release is assembled, you can start it by calling `bin/start`
  inside the release. In production, you would do:

      MIX_ENV=prod mix release
      _build/prod/rel/my_app/bin/start

  `bin/start` is a very short script that invokes the proper instruction
  on `bin/RELEASE_NAME`. You can customize `bin/start` in any way you want,
  see the "Customization" section, but you are not supposed to change
  `bin/RELEASE_NAME`.

  `bin/start` will start the system connected to the current standard
  input/output, where logs are also written to by default. This is the
  preferred way to run the system. Many tools, such as `systemd`, platforms
  as a service, such as Heroku, and many containers platforms, such as Docker,
  are capable of processing the standard input/output and redirecting
  the log contents elsewhere. Those tools and platforms also take care
  of restarting the system in case it crashes.

  For those looking for alternate ways of running the system, you can
  run it as a daemon on Unix-like system or install it as a service on
  Windows. Then we list all commands supported by `bin/RELEASE_NAME`.

  ### One-off commands (eval and rpc)

  If you want to invoke specific modules and functions in your release,
  you can do so in two ways: using `eval` or `rpc`.

      bin/RELEASE_NAME eval "IO.puts :hello"
      bin/RELEASE_NAME rpc "IO.puts :hello"

  The `eval` command starts its own instance of the VM but without
  starting any of the applications in the release and without starting
  distribution. For example, if you need to do some prep work before
  running the actual system, like updating your database, `eval` can
  be a good fit. Just keep in mind any application you may use during
  eval has to be explicitly started.

  Another way to run commands is with `rpc`, which will connect to the
  system currently running and instruct it to execute the given
  expression. This means you need to guarantee the system was already
  started and be careful with the instructions you are executing.
  You can also use `remote` to connect a remote IEx session to the
  system.

  ### Daemon mode (Unix-like)

  If you open up `bin/start`, you will also see there is an option to
  run the release in daemon mode written as a comment:

      bin/RELEASE_NAME daemon_iex

  In daemon mode, the system is started on the background via
  [run_erl](http://erlang.org/doc/man/run_erl.html). You may also
  want to enable [heart](http://erlang.org/doc/man/heart.html)
  in daemon mode so it automatically restarts the system in case
  of crashes. See the generated `bin/start`.

  The daemon will write all of its standard output to the "tmp/log/"
  directory in the release root. A developer can also attach
  to the standard input of the daemon by invoking "to_erl tmp/pipe/"
  from the release root. However, note that attaching to the system
  should be done with extreme care, since the usual commands for
  exiting an Elixir system, such as hitting Ctrl+C twice or Ctrl+\\,
  will actually shut down the daemon. Therefore, using
  `bin/RELEASE_NAME remote` should be preferred, even in daemon mode.

  You can customize the tmp directory used both for logging and for
  piping in daemon mode by setting the `RELEASE_TMP` environment
  variable on `bin/start`. See the "Customization" section.

  ### Services mode (Windows)

  While daemons are not available on Windows, it is possible to install a
  released system as a service on Windows with the help of
  [erlsrv](http://erlang.org/doc/man/erlsrv.html). This can be done by
  running:

      bin/RELEASE_NAME install

  Once installed, the service must be explicitly managed via the `erlsrv`
  executable, which is included in the `erts-VSN/bin` directory.
  The service is not started automatically after installing.

  For example, if you have a release named `demo`, you can install
  the service and then start it from the release root as follows:

      bin/demo install
      erts-VSN/bin/erlsrv.exs start demo_demo

  The name of the service is `demo_demo` because the name is built
  by concatenating the node name with the release name. Since Elixir
  automatically uses the same name for both, the service will be
  referenced as `demo_demo`.

  The `install` command must be executed as an administrator.

  ## `bin/RELEASE_NAME` commands

  The following commands are supported by `bin/RELEASE_NAME`:

      start        Starts the system
      start_iex    Starts the system with IEx attached
      daemon       Starts the system as a daemon (Unix-like only)
      daemon_iex   Starts the system as a daemon with IEx attached (Unix-like only)
      install      Installs this system as a Windows service (Windows only)
      eval "EXPR"  Executes the given expression on a new, non-booted system
      rpc "EXPR"   Executes the given expression remotely on the running system
      remote       Connects to the running system via a remote shell
      restart      Restarts the running system via a remote command
      stop         Stops the running system via a remote command
      pid          Prints the OS PID of the running system via a remote command
      version      Prints the release name and version to be booted

  ## Deployments

  ### Requirements

  A release is built on a **host**, a machine which contains Erlang, Elixir,
  and any other dependencies needed to compile your application. A release is
  then deployed to a **target**, potentially the same machine as the host,
  but usually separate, and often there are many targets (either multiple
  instances, or the release is deployed to heterogeneous environments).

  To deploy straight from a host to a separate target without cross-compilation,
  the following must be the same between the host and the target:

    * Target architecture (e.g. x86_64 vs ARM)
    * Target Vendor+OS (e.g. Windows, Linux, Darwin/macOS)
    * Target ABI (e.g. musl, gnu)

  This is often represented in the form of target triples, e.g.
  x86_64-unknown-linux-gnu, x86_64-unknown-linux-musl, x86_64-apple-darwin.

  So to be more precise, to deploy straight from a host to a separate target,
  the Erlang Runtime System (ERTS), and any native dependencies (NIFs), must
  be compiled for the same target triple. If you are building on a MacBook
  (x86_64-apple-darwin) and trying to deploy to a typical Ubuntu machine
  (x86_64-unknown-linux-gnu), the release will not work. Instead you should
  build the release on a x86_64-unknown-linux-gnu host. As we will see, this
  can be done in multiple ways, such as releasing on the target itself, or by
  using virtual machines or containers, usually as part of your release pipeline.

  In addition to matching the target triple, it is also important that the
  target has all of the system packages that your application will need at
  runtime. A common one is the need for OpenSSL when building an application
  that uses `:crypto` or `:ssl`, which is dynamically linked to ERTS. The other
  common source for native dependencies like this comes from dependencies
  containing NIFs (natively-implemented functions) which may expect to
  dynamically link to libraries they use.

  These system packages are typically managed using the system package manager,
  but if necessary, you can also bundle the compiled object files in the release,
  as long as they were compiled for the same target. If doing so, you need to
  update LD_LIBRARY_PATH with the paths containing the bundled objects.

  Currently, there is no official way to cross-compile a release from one
  target triple to another, due to the complexities involved in the process.

  ### Techniques

  There a couple ways to guarantee that a release is built on a host with
  the same properties as the target. A simple option is to fetch the source,
  compile the code and assemble the release on the target itself. It would
  be something like this:

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

  However, this option can be expensive if you have multiple production
  nodes or if the release assembling process is a long one, as each node
  needs to individually assemble the release.

  You can automate this process in a couple different ways. One option
  is to make it part of your Continuous Integration (CI) / Continuous
  Deployment (CD) pipeline. When you have a CI/CD pipeline, it is common
  that the machines in your CI/CD pipeline run on the exact same target
  triple as your production servers (if they don't, they should).
  In this case, you can assemble the release at the end of your CI/CD
  pipeline by calling `MIX_ENV=prod mix release` and push the artifact
  to S3 or any other network storage. To perform the deployment, your
  production machines can fetch the deployment from the network storage
  and run the `bin/start` script.

  Another mechanism to automate deployments is to use images, such as
  Amazon Machine Images, or container platforms, such as Docker.
  For instance, you can use Docker to run locally a system with the
  exact same target triple as your production servers. Inside the
  container, you can invoke `MIX_ENV=prod mix release` and build
  a complete image and/or container with the operating system, all
  dependencies as well as the releases.

  In other words, there are multiple ways systems can be deployed and
  releases can be automated and incorporated into all of them as long
  as you remember to build the system in the same target triple.

  Once a system is deployed, shutting down the system can be done by
  sending SIGINT/SIGTERM to the system, which is what most containers,
  platforms and tools do, or by explicitly invoking `bin/RELEASE_NAME stop`.
  Once the system receives the shutdown request, each application and
  their respective supervision trees will stop, one by one, in the
  opposite order that they were started.

  ## Options

  Here are the general options available to each release:

    * `:applications` - a keyword list that configures and adds new applications
      to the release. The key is the application name and the value is one of:

        * `:permanent` - the application is started and the node shuts down
          if the application terminates, regardless of reason
        * `:transient` - the application is started and the node shuts down
          if the application terminates abnormally
        * `:temporary` - the application is started and the node does not
          shut down if the application terminates
        * `:load` - the application is only loaded
        * `:none` - the application is part of the release but it is neither loaded nor
          started

      All applications default to `:permanent`.

      By default `:applications` include the current application and then we
      proceed to include all applications the current application depends on
      recursively. You can include new applications or change the mode of
      existing ones by listing them here. The order of the applications given
      in `:applications` will be preserved as much as possible, with only
      `:kernel`, `:stdlib`, `:sasl` and `:elixir` listed before the given
      application list.

      Releases assembled from an umbrella project require this configuration
      to be explicitly given.

    * `:strip_beams` - a boolean that controls if BEAM files should have their debug
      information, documentation chunks and other non-essential metadata removed.
      Defaults to `true`.

    * `:cookie` - a string representing the Erlang Distribution cookie. If no cookie
      is set, one is automatically generated when the first release is assembled. The
      cookie will be written to `releases/COOKIE` and shared across multiple release
      versions. If you are setting this option manually, we recommend the cookie option
      to be a long and randomly generated, such as:
      `Base.url_encode64(:crypto.strong_rand_bytes(40))`. We also recommend to restrict
      the characters in the cookie to the subset returned by `Base.url_encode64/1`.

    * `:path` - the path the release should be installed to.
      Defaults to `"_build/MIX_ENV/rel/RELEASE_NAME"`.

    * `:version` - the release version as a string. Defaults to the current
      application version.

    * `:force` - a boolean that controls if conflicting operations should be
      always performed, without asking for confirmation before. Defaults to `false`.

    * `:quiet` - a boolean that controls if releases should write to the standard
      output its steps. Defaults to `false`.

    * `:include_erts` - a boolean indicating if the Erlang Runtime System (ERTS),
      which includes the Erlang VM, should be included in the release. The default
      is `true`, which is also the recommended value. It may also be a string as a
      path to an existing ERTS installation or an anonymous function of zero arity
      which should return any of the above.

      You may also set it to `false` if you desire to use the ERTS version installed
      on the target. Note, however, the ERTS version on the target must have THE EXACT
      VERSION as the ERTS version used when the release is assembled. Setting it to
      `false` also disables hot code upgrades. Therefore, `:include_erts` should be
      set to `false` with caution and only if you are assembling the release on the
      same server that runs it.

  Also see the "Customization" section for other options and the hooks
  available to customize your release.

  Other features, such as runtime configuration, may introduce their
  options. Those will be listed in their own respective sections.

  ## Configuration

  Releases provides two mechanisms for configuration: built-time and
  runtime.

  ### Build-time configuration

  Whenever you invoke a `mix` command, Mix loads the configuration
  in `config/config.exs`, if said file exists. It is common for the
  `config/config.exs` file itself import other configuration based
  on the current `MIX_ENV`, such as `config/dev.exs`, `config/test.exs`,
  and `config/prod.exs`. We say that this configuration is a build-time
  configuration as it is evaluated whenever you compile your code or
  whenever you assemble the release.

  In other words, if your configuration does something like:

      config :my_app, :secret_key, System.fetch_env!("MY_APP_SECRET_KEY")

  The `:secret_key` key under `:my_app` will be computed on the
  host machine, whenever the release is built. Setting the
  `MY_APP_SECRET_KEY` right before starting your release will have
  no effect.

  Luckily, releases also provide runtime configuration, which we will
  see next.

  ### Runtime configuration

  To enable runtime configuration in your release, all you need to do is
  to create a file named `config/releases.exs`:

      import Config
      config :my_app, :secret_key, System.fetch_env!("MY_APP_SECRET_KEY")

  Your `config/releases.exs` file needs to follow three important rules:

    * It MUST `import Config` at the top instead of the deprecated `use Mix.Config`
    * It MUST NOT import any other configuration file via `import_file`
    * It MUST NOT access `Mix` in any way, as `Mix` is a build tool and it not
      available inside releases

  If a `config/releases.exs` exists, it will be copied to your release
  and executed as soon the system starts. Once the configuration is loaded,
  the Erlang system will be restarted (within the same Operating System
  process) and the new configuration will take place.

  Therefore, for runtime configuration to work properly, it needs to be
  able to persist the newly computed configuration to disk. The computed
  config file will be written to "tmp" directory inside the release every
  time the system boots. You can configure the "tmp" directory by setting
  the `RELEASE_TMP` environment variable, either explicitly or inside your
  `bin/start` script. See the "Customization" section.

  Releases also supports custom mechanisms, called config providers, to load
  any sort of runtime configuration to the system while it boots. For example,
  if you need to access a vault or load configuration from a JSON file, it
  can be achieved with config providers. See the `Config.Provider` for more
  information and a simple example.

  The following options can be set inside your releases key in yourr mix.exs
  to control how runtime configuration and config providers work:

    * `:runtime_config_path` - the path to your runtime configuration file.
      Defaults to `config/releases.exs`.

    * `:start_distribution_during_config` - on Erlang/OTP 22+, releases
      only start the Erlang VM distribution features after the config files
      are evaluated. You can set it to `true` if you need distribution during
      configurationn. Defaults to `false`.

    * `:prune_runtime_sys_config_after_boot` - every time your system boots,
      the release will write a config file to your tmp directory. These
      configuration files are generally small. But if you are concerned with
      disk space or if you have other restrictions, you can ask the system to
      remove said config files after boot. The downside is that you will no
      longer be able to restart the system internally (neither via
      `System.restart/0` nor `bin/RELEASE_NAME start`). If you need a restart,
      you will have to terminate the Operating System process and start a new
      one. Defaults to `false`.

    * `:config_providers` - a list of tuples with custom config providers.
      See `Config.Provider` for more information. Defaults to `[]`.

  ## Customization

  There are a couple ways in which developers can customize the generated
  artifacts inside a release.

  ### Executables

  By default, a release will generate executables for both Windows and Unix.
  You can customize those by setting the `:include_executables_for` option
  inside your release:

      releases: [
        demo: [
          include_executables_for: [:unix] # Or [:windows] or []
        ]
      ]

  ### vm.args and bin/start

  Developers may want to customize the VM flags and environment variables
  given when the release starts. This is typically done by customizing
  two files: `vm.args` and `bin/start` (or `bin/start.bat` on Windows).

  The `vm.args` is available on every release version, inside
  `releases/RELEASE_VSN/vm.args`, and in there you can set all flags
  known by the `erl` command: http://erlang.org/doc/man/erl.html

  The `bin/start` is shared across all released versions and it is
  the entry point to starting your release. It is also the best place
  to set any environment variable you may need in your release, such
  as `RELEASE_TMP`. You may also set `ELIXIR_ERL_OPTIONS` to
  dynamically set VM options.

  To configure both files, all you need to do is to define an EEx
  template inside the `rel` folder at the root of your project. For
  example, if you define `rel/vm.args.eex`, it will be evaluated
  and written to `releases/RELEASE_VSN/vm.args` when the release
  is assembled.

  Similarly, if you have `rel/start.eex` (or `rel/start.bat.eex`
  for Windows) inside `rel`, it will be copied to the release when
  it is assembled.

  When the EEx template is evaluated, it will have a single assign,
  called `@release`, with the `Mix.Release` struct.

  Finally, you can invoke `mix release.init` and it will create
  a `rel` folder with the default `vm.args.eex`, `start.eex`, and
  `start.bat.eex` as example files.

  ### Steps

  It is possible to add one or more steps before and after the release is
  assembled. This can be done with the `:steps` option:

      releases: [
        demo: [
          steps: [&set_configs/1, :assemble, &copy_extra_files/1]
        ]
      ]

  The `:steps` option must be a list and it must always include the
  atom `:assemble`, which does most of the release assembling. You
  can pass anonymous functions before and after the `:assemble` to
  customize your release assembling pipeline. Those anonymous functions
  will receive a `Mix.Release` struct and must return the same or
  an updated `Mix.Release` struct.

  See `Mix.Release` for more documentation on the struct and which
  fields can be modified. Note that `:steps` field itself can be
  modified and it is updated every time a step is called. Therefore,
  if you need to execute a command before and after assembling the
  release, you only need to declare the first steps in your pipeline
  and then inject the last step into the release struct. The steps
  field can also be used to verify if the step was set before or
  after assembling the release.

  ## Directory structure and environment variables

  A release is organized as follows:

      bin/
        RELEASE_NAME
        start
      erts-ERTS_VSN/
      lib/
        APP_NAME-APP_VSN/
          ebin/
          include/
          priv/
      releases/
        RELEASE_VSN/
          consolidated/
          elixir
          iex
          releases.exs
          start.boot
          start.script
          start_clean.boot
          start_clean.script
          sys.config
          vm.args
        COOKIE
        start_erl.data
      tmp/

  Furthermore, the system can be configured and sets the following
  environment variables:

    * `RELEASE_ROOT` - points to the root of the release. If the system
      includes ERTS, then it is the same as `:code.root_dir/0`

    * `RELEASE_NAME` - the name of the release. It can be set in a
      custom `bin/start` file to a custom value

    * `RELEASE_VSN` - the version of the release, otherwise the latest
      version is used. It can be set in a custom `bin/start` file to
      a custom value

    * `RELEASE_COOKIE` - the release cookie. By default uses the value
      in `releases/COOKIE`. It can be set in a custom `bin/start` file
      to a custom value

    * `RELEASE_NODE` - the release node name, in the format `name@host`.
      It can be set in a custom `bin/start` file to a custom value

    * `RELEASE_VM_ARGS` - the location of the vm.args file. It can be set in
      a custom `bin/start` file to a custom value

    * `RELEASE_TMP` - the directory in the release to write temporary
      files to. It can be set in a custom `bin/start` file to a custom value.
      Defaults to the `$RELEASE_ROOT/tmp`

  ## Umbrellas

  Releases are well integrated with umbrella projects, allowing you to
  release one or more subsets of your umbrella children. The only difference
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

  ## Hot Code Upgrades

  Erlang and Elixir are sometimes known for the capability of upgrading
  a node that is running in production without shutting down that node.
  However, this feature is not supported out of the box by Elixir releases.

  The reason we don't provide hot code upgrades is because they are very
  complicated to perform in practice, as they require careful coding of
  your processes and applications as well as extensive testing. Given most
  teams can use other techniques that are language agnostic to upgrade
  their systems, such as Blue/Green deployments, Canary deployments,
  Rolling deployments, and others, hot upgrades are rarely a viable
  option. Let's understand why.

  In a hot code upgrade, you want to update a node from version A to
  version B. To do so, the first step is to write recipes for every application
  that changed between those two releases, telling exactly how the application
  changed between versions, those recipes are called `.appup` files.
  While some of the steps in building `.appup` files can be automated,
  not all of them can. Furthermore, each process in the application needs
  to be explicitly coded with hot code upgrades in mind. Let's see an example.
  Imagine your application has a counter process as a GenServer:

      defmodule Counter do
        use GenServer

        def start_link(_) do
          GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
        end

        def bump do
          GenServer.call(__MODULE__, :bump)
        end

        ## Callbacks

        def init(:ok) do
          {:ok, 0}
        end

        def handle_call(:bump, counter) do
          {:reply, :ok, counter + 1}
        end
      end

  You add this process as part of your supervision tree and ship version
  0.1.0 of your system. Now let's imagine that on version 0.2.0 you added
  two changes: instead of `bump/0`, that always increments the counter by
  one, you introduce `bump/1` that passes the exact value to bump the
  counter. You also change the state, because you want to store the maximum
  bump value:

      defmodule Counter do
        use GenServer

        def start_link(_) do
          GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
        end

        def bump(by) do
          GenServer.call(__MODULE__, {:bump, by})
        end

        ## Callbacks

        def init(:ok) do
          {:ok, {0, 0}}
        end

        def handle_call({:bump, by}, {counter, max}) do
          {:reply, :ok, {counter + by, max(max, by)}}
        end
      end

  If you to perform a hot code upgrade in such application, it would
  crash, because in the initial version the state was just a counter
  but in the new version the state is a tuple. Furthermore, you changed
  the format of the `call` message from `:bump` to  `{:bump, by}` and
  the process may have both old and new messages temporarily mixed, so
  we need to handle both. The final version would be:

      defmodule Counter do
        use GenServer

        def start_link(_) do
          GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
        end

        def bump(by) do
          GenServer.call(__MODULE__, {:bump, by})
        end

        ## Callbacks

        def init(:ok) do
          {:ok, {0, 0}}
        end

        def handle_call(:bump, {counter, max}) do
          {:reply, :ok, {counter + 1, max(max, 1)}}
        end

        def handle_call({:bump, by}, {counter, max}) do
          {:reply, :ok, {counter + by, max(max, by)}}
        end

        def code_change(_, counter, _) do
          {:ok, {counter, 0}}
        end
      end

  Now you can proceed to list this process in the `.appup` file and
  hot code upgrade it. This is one of the many steps one necessary
  to perform hot code upgrades and it must be taken into account by
  every process and application being upgraded in the system.
  The [`.appup` cookbook](http://erlang.org/doc/design_principles/appup_cookbook.html)
  provides a good reference and more examples.

  Once `.appup`s are created, the next step is to create a `.relup`
  file with all instructions necessary to update the release itself.
  Erlang documentation does provide a chapter on
  [Creating and Upgrading a Target System](http://erlang.org/doc/system_principles/create_target.html).
  [Learn You Some Erlang has a chapter on hot code upgrades](https://learnyousomeerlang.com/relups).

  Overall, there are many steps, complexities and assumptions made
  during hot code upgrades, which is ultimately why they are not
  provided by Elixir out of the box. However, hot code upgrades can
  still be achieved by teams who desire to implement those steps
  on top of `mix release` in their projects or as separate libraries.

  ## Command line options

    * `--force` - forces files to be overridden
    * `--quiet` - do not write progress to the standard output
    * `--path` - the path of the release
    * `--version` - the version of the release
    * `--no-archives-check` - does not check archive
    * `--no-deps-check` - does not check dependencies
    * `--no-elixir-version-check` - does not check Elixir version
    * `--no-compile` - does not compile before assembling the release

  """

  import Mix.Generator

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
        {overrides, [name]} -> Mix.Release.from_config!(String.to_atom(name), config, overrides)
        {overrides, []} -> Mix.Release.from_config!(nil, config, overrides)
        {_, _} -> Mix.raise("Expected \"mix release\" or \"mix release NAME\"")
      end

    if not File.exists?(release.version_path) or
         yes?(release, "Release #{release.name}-#{release.version} already exists. Overwrite?") do
      run_steps(release)
    end
  end

  defp yes?(release, message) do
    release.options[:force] or Mix.shell().yes?(message)
  end

  defp run_steps(%{steps: [step | steps]} = release) when is_function(step) do
    case step.(%{release | steps: steps}) do
      %Mix.Release{} = release ->
        run_steps(release)

      other ->
        Mix.raise(
          "Expected step #{inspect(step)} to return a Mix.Release, got: #{inspect(other)}"
        )
    end
  end

  defp run_steps(%{steps: [:assemble | steps]} = release) do
    %{release | steps: steps} |> assemble() |> run_steps()
  end

  defp run_steps(%{steps: []} = release) do
    announce(release)
  end

  defp assemble(release) do
    config = Mix.Project.config()
    message = "#{release.name}-#{release.version} on MIX_ENV=#{Mix.env()}"
    info(release, [:green, "* assembling ", :reset, message])

    # releases/
    #   VERSION/
    #     consolidated/
    #     NAME.rel
    #     start.boot
    #     start.script
    #     start_clean.boot
    #     start_clean.script
    #     sys.config
    # releases/
    #   COOKIE
    #   start_erl.data
    consolidation_path = build_rel(release, config)

    [
      # erts-VSN/
      :erts,
      # releases/VERSION/consolidated
      {:consolidated, consolidation_path},
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
      {:executables, Keyword.get(release.options, :include_executables_for, [:unix, :windows])}
      # lib/APP_NAME-APP_VSN/
      | Map.keys(release.applications)
    ]
    |> Task.async_stream(&copy(&1, release), ordered: false, timeout: :infinity)
    |> Stream.run()

    release
  end

  # build_rel

  defp build_rel(release, config) do
    version_path = release.version_path
    File.rm_rf!(version_path)
    File.mkdir_p!(version_path)

    consolidation_path =
      if config[:consolidate_protocols] do
        Mix.Project.consolidation_path(config)
      end

    sys_config =
      if File.regular?(config[:config_path]) do
        config[:config_path] |> Config.Reader.read!()
      else
        []
      end

    release = maybe_add_config_reader_provider(release, version_path)
    vm_args_path = Path.join(version_path, "vm.args")
    cookie_path = Path.join(release.path, "releases/COOKIE")
    start_erl_path = Path.join(release.path, "releases/start_erl.data")
    config_provider_path = {:system, "RELEASE_SYS_CONFIG", ".config"}

    with :ok <- make_boot_scripts(release, version_path, consolidation_path),
         :ok <- make_vm_args(release, vm_args_path),
         :ok <- Mix.Release.make_sys_config(release, sys_config, config_provider_path),
         :ok <- Mix.Release.make_cookie(release, cookie_path),
         :ok <- Mix.Release.make_start_erl(release, start_erl_path) do
      consolidation_path
    else
      {:error, message} ->
        File.rm_rf!(version_path)
        Mix.raise(message)
    end
  end

  defp maybe_add_config_reader_provider(%{options: opts} = release, version_path) do
    path =
      cond do
        path = opts[:runtime_config_path] ->
          path

        File.exists?("config/releases.exs") ->
          "config/releases.exs"

        true ->
          nil
      end

    cond do
      path ->
        msg = "#{path} to configure the release at runtime"
        Mix.shell().info([:green, "* using ", :reset, msg])
        File.cp!(path, Path.join(version_path, "releases.exs"))
        init = {:system, "RELEASE_ROOT", "/releases/#{release.version}/releases.exs"}
        update_in(release.config_providers, &[{Config.Reader, init} | &1])

      release.config_providers == [] ->
        msg = "runtime configuration (config/releases.exs was not found)"
        Mix.shell().info([:yellow, "* skipping ", :reset, msg])
        release

      true ->
        release
    end
  end

  defp make_boot_scripts(release, version_path, consolidation_path) do
    prepend_paths =
      if consolidation_path do
        ["$RELEASE_LIB/../releases/#{release.version}/consolidated"]
      else
        []
      end

    results =
      for {boot_name, modes} <- release.boot_scripts do
        sys_path = Path.join(version_path, Atom.to_string(boot_name))

        with :ok <- Mix.Release.make_boot_script(release, sys_path, modes, prepend_paths) do
          if boot_name == :start do
            rel_path = Path.join(Path.dirname(sys_path), "#{release.name}.rel")
            File.rename!(sys_path <> ".rel", rel_path)
          else
            File.rm(sys_path <> ".rel")
          end

          :ok
        end
      end

    Enum.find(results, :ok, &(&1 != :ok))
  end

  defp make_vm_args(release, path) do
    if File.exists?("rel/vm.args.eex") do
      copy_template("rel/vm.args.eex", path, [release: release], force: true)
    else
      File.write!(path, vm_args_template(release: release))
    end

    :ok
  end

  defp announce(release) do
    path = Path.relative_to_cwd(release.path)
    cmd = "#{path}/bin/#{release.name}"

    info(release, """

    Release created at #{path}!

        # To start your system
        #{path}/bin/start

    Check bin/start for more information. Once the release is running:

        # To connect to it remotely
        #{cmd} remote

        # To stop it gracefully (you may also send SIGINT/SIGTERM)
        #{cmd} stop

    To list all commands:

        #{cmd}
    """)
  end

  defp info(release, command) do
    unless release.options[:quiet] do
      Mix.shell().info(command)
    end
  end

  ## Copy operations

  defp copy(:erts, release) do
    _ = Mix.Release.copy_erts(release)
    :ok
  end

  defp copy(app, release) when is_atom(app) do
    Mix.Release.copy_app(release, app)
  end

  defp copy({:consolidated, consolidation_path}, release) do
    if consolidation_path do
      consolidation_target = Path.join(release.version_path, "consolidated")
      _ = Mix.Release.copy_ebin(release, consolidation_path, consolidation_target)
    end

    :ok
  end

  defp copy({:executables, include_executables_for}, release) do
    elixir_bin_path = Application.app_dir(:elixir, "../../bin")
    bin_path = Path.join(release.path, "bin")
    File.mkdir_p!(bin_path)

    for os <- include_executables_for do
      {start, start_fun, clis} = cli_for(os, release)
      start_path = Path.join(bin_path, start)
      start_template_path = Path.join("rel", start <> ".eex")

      if File.exists?(start_template_path) do
        copy_template(start_template_path, start_path, [release: release], force: true)
      else
        File.write!(start_path, start_fun.(release))
      end

      executable!(start_path)

      for {filename, contents} <- clis do
        path = Path.join(bin_path, filename)
        File.write!(path, contents)
        executable!(path)
      end

      unless File.exists?(elixir_bin_path) do
        Mix.raise("Could not find bin files from Elixir installation for #{os}")
      end

      for {filename, contents} <- elixir_cli_for(os, elixir_bin_path, release) do
        path = Path.join(release.version_path, filename)
        File.write!(path, contents)
        executable!(path)
      end
    end
  end

  defp cli_for(:unix, release) do
    {"start", &start_template(release: &1), [{"#{release.name}", cli_template(release: release)}]}
  end

  defp cli_for(:windows, release) do
    {"start.bat", &start_bat_template(release: &1),
     [{"#{release.name}.bat", cli_bat_template(release: release)}]}
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
       |> replace_erts_bin(release, ~s[%~dp0\\..\\..\\erts-#{release.erts_version}\\bin\\])},
      {"iex.bat", File.read!(Path.join(bin_path, "iex.bat"))}
    ]
  end

  defp replace_erts_bin(contents, release, new_path) do
    if release.erts_source do
      String.replace(contents, ~s[ERTS_BIN=], ~s[ERTS_BIN=#{new_path}])
    else
      contents
    end
  end

  defp executable!(path), do: File.chmod!(path, 0o744)

  embed_template(:vm_args, Mix.Tasks.Release.Init.vm_args_text())
  embed_template(:start, Mix.Tasks.Release.Init.start_text())
  embed_template(:cli, Mix.Tasks.Release.Init.cli_text())
  embed_template(:start_bat, Mix.Tasks.Release.Init.start_bat_text())
  embed_template(:cli_bat, Mix.Tasks.Release.Init.cli_bat_text())
end
