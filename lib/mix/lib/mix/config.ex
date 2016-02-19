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

  defmodule LoadError do
    defexception [:file, :error]

    def message(%LoadError{file: file, error: error}) do
      "could not load config #{Path.relative_to_cwd(file)}\n    " <>
        "#{Exception.format_banner(:error, error)}"
    end
  end

  @doc false
  defmacro __using__(_) do
    quote do
      import Mix.Config, only: [config: 2, config: 3, import_config: 1]
      {:ok, agent} = Mix.Config.Agent.start_link
      var!(config_agent, Mix.Config) = agent
    end
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
  defmacro config(app, opts) do
    quote do
      Mix.Config.Agent.merge var!(config_agent, Mix.Config), [{unquote(app), unquote(opts)}]
    end
  end

  @doc """
  Configures the given key for the given application.

  Keyword lists are always deep merged.

  ## Examples

  The given `opts` are merged into the existing values for `key`
  in the given `app`. Conflicting keys are overridden by the
  ones specified in `opts`. For example, the declaration below:

      config :ecto, Repo,
        log_level: :warn

      config :ecto, Repo,
        log_level: :info,
        pool_size: 10

  Will have a final value for `Repo` of:

      [log_level: :info, pool_size: 10]

  This final value can be retrieved at run or compile time:

      Application.get_env(:ecto, Repo)

  """
  defmacro config(app, key, opts) do
    quote do
      Mix.Config.Agent.merge var!(config_agent, Mix.Config),
        [{unquote(app), [{unquote(key), unquote(opts)}]}]
    end
  end

  @doc ~S"""
  Imports configuration from the given file or files.

  If `path_or_wildcard` is a wildcard, then all the files
  matching that wildcard will be imported; if no file matches
  the wildcard, no errors are raised. If `path_or_wildcard` is
  not a wildcard but a path to a single file, then that file is
  imported; in case the file doesn't exist, an error is raised.
  This behaviour is analogous to the one for `read_wildcard!/1`.

  If path/wildcard is a relative path/wildcard, it will be expanded relatively
  to the directory the current configuration file is in.

  ## Examples

  This is often used to emulate configuration across environments:

      import_config "#{Mix.env}.exs"

  Or to import files from children in umbrella projects:

      import_config "../apps/*/config/config.exs"

  """
  defmacro import_config(path_or_wildcard) do
    quote do
      Mix.Config.Agent.merge(
        var!(config_agent, Mix.Config),
         Mix.Config.read_wildcard!(Path.expand(unquote(path_or_wildcard), __DIR__))
      )
    end
  end

  @doc """
  Reads and validates a configuration file.

  `file` is the path to the configuration file to be read. If that file doesn't
  exist or if there's an error loading it, a `Mix.Config.LoadError` exception
  will be raised.
  """
  def read!(file) do
    try do
      {config, binding} = Code.eval_file(file)

      config = case List.keyfind(binding, {:config_agent, Mix.Config}, 0) do
        {_, agent} -> get_config_and_stop_agent(agent)
        nil        -> config
      end

      validate!(config)
      config
    rescue
      e in [LoadError] -> reraise(e, System.stacktrace)
      e -> reraise(LoadError, [file: file, error: e], System.stacktrace)
    end
  end

  defp get_config_and_stop_agent(agent) do
    config = Mix.Config.Agent.get(agent)
    Mix.Config.Agent.stop(agent)
    config
  end

  @doc """
  Reads many configuration files given by wildcard into a single config.

  Raises an error if `path` is a concrete filename (with no wildcards)
  but the corresponding file does not exist; if `path` matches no files,
  no errors are raised.
  """
  def read_wildcard!(path) do
    paths = if String.contains?(path, ~w(* ? [ {))do
      Path.wildcard(path)
    else
      [path]
    end
    Enum.reduce(paths, [], &merge(&2, read!(&1)))
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

  @doc """
  Validates a configuration.
  """
  def validate!(config) do
    if is_list(config) do
      Enum.all?(config, fn
        {app, value} when is_atom(app) ->
          if Keyword.keyword?(value) do
            true
          else
            raise ArgumentError,
              "expected config for app #{inspect app} to return keyword list, got: #{inspect value}"
          end
        _ ->
          false
      end)
    else
      raise ArgumentError,
        "expected config file to return keyword list, got: #{inspect config}"
    end
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
