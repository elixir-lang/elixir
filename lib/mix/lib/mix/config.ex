defmodule Mix.Config do
  @moduledoc ~S"""
  Module for defining, reading and merging app configurations.

  Most commonly, this module is used to define your own configuration:

      use Mix.Config

      config :plug,
        key1: "value1",
        key2: "value2"

      import_config "#{Mix.env}.exs"

  All `config/*` macros, including `import_config/1`, are used
  to help define such configuration files.

  Furthermore, this module provides functions like `read!/1`,
  `merge/2` and friends which help manipulate configurations
  in general.

  Configuration set using `Mix.Config` will set the application env, so
  that `Application.get_env/3` and other `Application` functions can be used
  at run or compile time to retrieve or change the configuration.

  For example, the `:key1` value from application `:plug` (see above) can be
  retrieved with:

      "value1" = Application.fetch_env!(:plug, :key1)

  """

  @doc false
  defmacro __using__(_) do
    quote do
      # TODO: If we split User API from Mix API, we no longer need to use Mix.Config.
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
    :ok
  end

  defp delete_config() do
    Process.delete(@config_key)
  end

  defp get_files!() do
    Process.get(@files_key) || raise_improper_use!()
  end

  defp put_files(value) do
    Process.put(@files_key, value)
    :ok
  end

  defp delete_files() do
    Process.delete(@files_key)
  end

  defp raise_improper_use!() do
    raise "could not set configuration via Mix.Config. " <>
            "This usually means you are trying to execute a configuration file " <>
            "directly instead of using the proper command, such as mix loadconfig"
  end

  @doc """
  Configures the given application.

  Keyword lists are always deep merged.

  ## Examples

  The given `opts` are merged into the existing configuration
  for the given `app`. Conflicting keys are overridden by the
  ones specified in `opts`. For example, the declaration below:

      config :lager,
        log_level: :warn,
        mode: :truncate

      config :lager,
        log_level: :info,
        threshold: 1024

  Will have a final configuration of:

      [log_level: :info, mode: :truncate, threshold: 1024]

  This final configuration can be retrieved at run or compile time:

      Application.get_all_env(:lager)

  """
  def config(app, opts) when is_atom(app) and is_list(opts) do
    get_config!()
    |> merge([{app, opts}])
    |> put_config()
  end

  @doc """
  Configures the given key for the given application.

  Keyword lists are always deep merged.

  ## Examples

  The given `opts` are merged into the existing values for `key`
  in the given `app`. Conflicting keys are overridden by the
  ones specified in `opts`. For example, given the two configurations
  below:

      config :ecto, Repo,
        log_level: :warn,
        adapter: Ecto.Adapters.Postgres

      config :ecto, Repo,
        log_level: :info,
        pool_size: 10

  the final value of the configuration for the `Repo` key in the `:ecto`
  application will be:

      [log_level: :info, pool_size: 10, adapter: Ecto.Adapters.Postgres]

  This final value can be retrieved at runtime or compile time with:

      Application.get_env(:ecto, Repo)

  """
  def config(app, key, opts) when is_atom(app) do
    get_config!()
    |> merge([{app, [{key, opts}]}])
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

      import_config "#{Mix.env}.exs"

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

  @doc """
  Reads and validates a configuration file.
  """
  def read!(file, loaded_paths \\ []) do
    put_config([])
    put_files(loaded_paths)
    {eval_config, _} = eval_config!(Path.expand(file))

    case get_config!() do
      [] when is_list(eval_config) ->
        validate!(eval_config)

      pdict_config ->
        pdict_config
    end
  after
    delete_config()
    delete_files()
  end

  @doc false
  @deprecated "Use read!/2 instead"
  def read_wildcard!(path, loaded_paths \\ []) do
    paths =
      if String.contains?(path, ~w(* ? [ {)) do
        Path.wildcard(path)
      else
        [path]
      end

    Enum.reduce(paths, [], &merge(&2, read!(&1, loaded_paths)))
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
  def persist(config) do
    for {app, kw} <- config do
      for {k, v} <- kw do
        Application.put_env(app, k, v, persistent: true)
      end

      app
    end
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
  Merges two configurations.

  The configuration of each application is merged together
  with the values in the second one having higher preference
  than the first in case of conflicts.

  ## Examples

      iex> Mix.Config.merge([app: [k: :v1]], [app: [k: :v2]])
      [app: [k: :v2]]

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
