defmodule Mix.Config do
  @moduledoc ~S"""
  A simple configuration API and functions for managing config files.

  ## Setting configuration

  Most commonly, this module is used to define your own configuration:

      use Mix.Config

      config :root_key,
        key1: "value1",
        key2: "value2"

      import_config "#{Mix.env()}.exs"

  `use Mix.Config` will import the functions `config/2`, `config/3`
  and `import_config/1` to help you manage your configuration.

  ## Evaluating configuration

  Once a configuration is written to a file, the functions in this
  module can be used to read and merge said configuration. The `eval!/2`
  function allows you evaluate a given configuration file and `merge/2`
  allows to deep merge the results of multiple configurations. Those
  functions should not be invoked by users writing configurations but
  rather by library authors.

  ## Examples

  The most common use of `Mix.Config` is to define application
  configuration so that `Application.get_env/3` and other `Application`
  functions can be used to retrieve or further change them.

  Application config files are typically placed in the `config/`
  directory of your Mix projects. For example, the following config

      # config/config.exs
      config :my_app, :key, "value"

  will be automatically loaded by Mix and persisted into the
  `:my_app`'s application environment, which can be accessed in
  its source code as follows:

      "value" = Application.fetch_env!(:my_app, :key1)

  """

  # TODO: Break the user API and the reader API apart.

  @doc false
  defmacro __using__(_) do
    quote do
      import Mix.Config, only: [config: 2, config: 3, import_config: 1]
    end
  end

  @config_key {__MODULE__, :config}
  @files_key {__MODULE__, :files}

  defp get_config!() do
    Process.get(@config_key) || raise_improper_use!()
  end

  defp put_config(value) do
    Process.put(@config_key, value)
  end

  defp delete_config() do
    Process.delete(@config_key)
  end

  defp get_files!() do
    Process.get(@files_key) || raise_improper_use!()
  end

  defp put_files(value) do
    Process.put(@files_key, value)
  end

  defp delete_files() do
    Process.delete(@files_key)
  end

  defp raise_improper_use!() do
    raise "could not set configuration via Mix.Config. " <>
            "This usually means you are trying to execute a configuration file " <>
            "directly instead of using the proper command, such as mix loadconfig"
  end

  ## User API

  @doc """
  Configures the given `root_key`.

  Keyword lists are always deep merged.

  ## Examples

  The given `opts` are merged into the existing configuration
  for the given `root_key`. Conflicting keys are overridden by the
  ones specified in `opts`. For example, the application
  configuration below

      config :logger,
        level: :warn,
        backends: [:console]

      config :logger,
        level: :info,
        truncate: 1024

  will have a final configuration for `:logger` of:

      [level: :info, backends: [:console], truncate: 1024]

  """
  def config(root_key, opts) when is_atom(root_key) and is_list(opts) do
    get_config!()
    |> merge([{root_key, opts}])
    |> put_config()
  end

  @doc """
  Configures the given `key` for the given `root_key`.

  Keyword lists are always deep merged.

  ## Examples

  The given `opts` are merged into the existing values for `key`
  in the given `root_key`. Conflicting keys are overridden by the
  ones specified in `opts`. For example, the application
  configuration below

      config :ecto, Repo,
        log_level: :warn,
        adapter: Ecto.Adapters.Postgres

      config :ecto, Repo,
        log_level: :info,
        pool_size: 10

  will have a final value of the configuration for the `Repo`
  key in the `:ecto` application of:

      [log_level: :info, pool_size: 10, adapter: Ecto.Adapters.Postgres]

  """
  def config(root_key, key, opts) when is_atom(root_key) do
    get_config!()
    |> merge([{root_key, [{key, opts}]}])
    |> put_config()
  end

  @doc ~S"""
  Imports configuration from the given file or files.

  If `path_or_wildcard` is a wildcard, then all the files
  matching that wildcard will be imported; if no file matches
  the wildcard, no errors are raised. If `path_or_wildcard` is
  not a wildcard but a path to a single file, then that file is
  imported; in case the file doesn't exist, an error is raised.

  If path/wildcard is a relative path/wildcard, it will be expanded
  relatively to the directory the current configuration file is in.

  ## Examples

  This is often used to emulate configuration across environments:

      import_config "#{Mix.env()}.exs"

  Or to import files from children in umbrella projects:

      import_config "../apps/*/config/config.exs"

  """
  defmacro import_config(path_or_wildcard) do
    quote do
      Mix.Config.__import__!(unquote(path_or_wildcard), __DIR__)
    end
  end

  @doc false
  def __import__!(path_or_wildcard, dir) do
    path_or_wildcard = Path.expand(path_or_wildcard, dir)

    paths =
      if String.contains?(path_or_wildcard, ~w(* ? [ {)) do
        Path.wildcard(path_or_wildcard)
      else
        [path_or_wildcard]
      end

    for path <- paths do
      eval_config!(path)
    end

    :ok
  end

  defp eval_config!(file) do
    current_files = get_files!()

    if file in current_files do
      raise ArgumentError,
            "attempting to load configuration #{Path.relative_to_cwd(file)} recursively"
    end

    put_files([file | current_files])
    Code.eval_file(file)
  end

  ## Mix API

  @doc """
  Evaluates the given configuration file.

  It accepts a list of `imported_paths` that should raise if attempted
  to be imported again (to avoid recursive imports).

  It returns a tuple with the configuration and the imported paths.
  """
  @spec eval!(Path.t(), [Path.t()]) :: {keyword, [Path.t()]}
  def eval!(file, imported_paths \\ []) do
    previous_config = put_config([])
    previous_files = put_files(imported_paths)

    try do
      {eval_config, _} = eval_config!(Path.expand(file))

      case get_config!() do
        [] when is_list(eval_config) ->
          {validate!(eval_config), get_files!()}

        pdict_config ->
          {pdict_config, get_files!()}
      end
    after
      if previous_config, do: put_config(previous_config), else: delete_config()
      if previous_files, do: put_files(previous_files), else: delete_files()
    end
  end

  @doc """
  Reads the configuration file.

  The same as `eval!/2` but only returns the configuration
  in the given file, without returning the imported paths.

  It exists for convenience purposes. For example, you could
  invoke it inside your `mix.exs` to read some external data
  you decided to move to a configuration file:

      releases: Mix.Config.read!("rel/releases.exs")

  """
  @spec read!(Path.t(), [Path.t()]) :: keyword
  def read!(file, imported_paths \\ []) do
    eval!(file, imported_paths) |> elem(0)
  end

  @doc false
  @deprecated "Use eval!/2 instead"
  def read_wildcard!(path, loaded_paths \\ []) do
    paths =
      if String.contains?(path, ~w(* ? [ {)) do
        Path.wildcard(path)
      else
        [path]
      end

    Enum.reduce(paths, [], &merge(&2, read!(&1, loaded_paths)))
  end

  @doc false
  @deprecated "Manually validate the data instead"
  def validate!(config) do
    validate!(config, "runtime")
  end

  defp validate!(config, file) do
    if is_list(config) do
      Enum.all?(config, fn
        {app, value} when is_atom(app) ->
          if Keyword.keyword?(value) do
            true
          else
            raise ArgumentError,
                  "expected #{Path.relative_to_cwd(file)} config for app #{inspect(app)} " <>
                    "to return keyword list, got: #{inspect(value)}"
          end

        _ ->
          false
      end)
    else
      raise ArgumentError,
            "expected #{Path.relative_to_cwd(file)} config to return " <>
              "keyword list, got: #{inspect(config)}"
    end

    config
  end

  @doc """
  Persists the given configuration by modifying
  the configured applications environment.

  `config` should be a list of `{app, app_config}` tuples or a
  `%{app => app_config}` map where `app` are the applications to
  be configured and `app_config` are the configuration (as key-value
  pairs) for each of those applications.

  Returns the configured applications.

  ## Examples

      Mix.Config.persist(logger: [level: :error], my_app: [my_config: 1])
      #=> [:logger, :my_app]

  """
  # TODO: Deprecate this once merge_env is added to Application
  def persist(config) do
    for {app, kw} <- config do
      for {k, v} <- kw do
        Application.put_env(app, k, v, persistent: true)
      end

      app
    end
  end

  @doc """
  Merges two configurations.

  The configurations are merged together with the values in
  the second one having higher preference than the first in
  case of conflicts. In case both values are set to keyword
  lists, it deep merges them.

  ## Examples

      iex> Mix.Config.merge([app: [k: :v1]], [app: [k: :v2]])
      [app: [k: :v2]]

      iex> Mix.Config.merge([app: [k: [v1: 1, v2: 2]]], [app: [k: [v2: :a, v3: :b]]])
      [app: [k: [v1: 1, v2: :a, v3: :b]]]

      iex> Mix.Config.merge([app1: []], [app2: []])
      [app1: [], app2: []]

  """
  def merge(config1, config2) do
    Keyword.merge(config1, config2, fn _, app1, app2 ->
      Keyword.merge(app1, app2, &deep_merge/3)
    end)
  end

  defp deep_merge(_key, value1, value2) do
    if Keyword.keyword?(value1) and Keyword.keyword?(value2) do
      Keyword.merge(value1, value2, &deep_merge/3)
    else
      value2
    end
  end
end
