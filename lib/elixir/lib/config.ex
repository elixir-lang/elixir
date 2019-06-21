defmodule Config do
  @moduledoc ~S"""
  A simple keyword-based configuration API.

  ## Example

  This module is most commonly used to define application configuration,
  typically in `config/config.exs`:

      import Config

      config :some_app,
        key1: "value1",
        key2: "value2"

      import_config "#{Mix.env()}.exs"

  `import Config` will import the functions `config/2`, `config/3`
  and `import_config/1` to help you manage your configuration.

  `config/2` and `config/3` are used to define key-value configuration
  for a given application. Once Mix starts, it will automatically
  evaluate the configuration file and persist the configuration above
  into `:some_app`'s application environment, which can be accessed in
  as follows:

      "value1" = Application.fetch_env!(:some_app, :key1)

  Finally, the line `import_config "#{Mix.env()}.exs"` will import other
  config files, based on the current Mix environment, such as
  `config/dev.exs` and `config/test.exs`.

  `Config` also provides a low-level API for evaluating and reading
  configuration, under the `Config.Reader` module.

  **Important:** if you are writing a library to be used by other developers,
  it is generally recommended to avoid the application environment, as the
  application environment is effectively a global storage. For more information,
  read our [library guidelines](library-guidelines.html).

  ## Migrating from `use Mix.Config`

  The `Config` module in Elixir was introduced in v1.9 as a replacement to
  `Mix.Config`, which was specific to Mix and has been deprecated.

  You can leverage `Config` instead of `Mix.Config` in two steps. The first
  step is to replace `use Mix.Config` at the top of your config files by
  `import Config`.

  The second is to make sure your `import_config/1` calls do not have a
  wildcard character. If so, you need to perform the wildcard lookup
  manually. For example, if you did:

      import_config "../apps/*/config/config.exs"

  It has to be replaced by:

      for config <- "apps/*/config/config.exs" |> Path.expand() |> Path.wildcard() do
        import_config config
      end

  ## config/releases.exs

  If you are using releases, see `mix release`, there another configuration
  file called `config/releases.exs`. While `config/config.exs` and friends
  mentioned in the previous section are executed whenever you run a Mix
  command, including when you assemble a release, `config/releases.exs` is
  execute every time your production system boots. Since Mix is not available
  in a production system, `config/releases.exs` must not use any of the
  functions from Mix.
  """

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
    raise "could not set configuration via Config. " <>
            "This usually means you are trying to execute a configuration file " <>
            "directly, instead of reading it with Config.Reader"
  end

  @doc """
  Configures the given `root_key`.

  Keyword lists are always deep-merged.

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
  @doc since: "1.9.0"
  def config(root_key, opts) when is_atom(root_key) and is_list(opts) do
    unless Keyword.keyword?(opts) do
      raise ArgumentError, "config/2 expected a keyword list, got: #{inspect(opts)}"
    end

    get_config!()
    |> __merge__([{root_key, opts}])
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
  @doc since: "1.9.0"
  def config(root_key, key, opts) when is_atom(root_key) and is_atom(key) do
    get_config!()
    |> __merge__([{root_key, [{key, opts}]}])
    |> put_config()
  end

  @doc ~S"""
  Imports configuration from the given file.

  In case the file doesn't exist, an error is raised.

  If file is a relative, it will be expanded relatively to the
  directory the current configuration file is in.

  ## Examples

  This is often used to emulate configuration across environments:

      import_config "#{Mix.env()}.exs"

  """
  @doc since: "1.9.0"
  defmacro import_config(file) do
    quote do
      Config.__import__!(Path.expand(unquote(file), __DIR__))
      :ok
    end
  end

  @doc false
  @spec __import__!(Path.t()) :: keyword()
  def __import__!(file) when is_binary(file) do
    current_files = get_files!()

    if file in current_files do
      raise ArgumentError,
            "attempting to load configuration #{Path.relative_to_cwd(file)} recursively"
    end

    put_files([file | current_files])
    Code.eval_file(file)
  end

  @doc false
  @spec __eval__!(Path.t(), [Path.t()]) :: {keyword, [Path.t()]}
  def __eval__!(file, imported_paths \\ []) when is_binary(file) and is_list(imported_paths) do
    previous_config = put_config([])
    previous_files = put_files(imported_paths)

    try do
      {eval_config, _} = __import__!(Path.expand(file))

      case get_config!() do
        [] when is_list(eval_config) ->
          {validate!(eval_config, file), get_files!()}

        pdict_config ->
          {pdict_config, get_files!()}
      end
    after
      if previous_config, do: put_config(previous_config), else: delete_config()
      if previous_files, do: put_files(previous_files), else: delete_files()
    end
  end

  @doc false
  def __merge__(config1, config2) when is_list(config1) and is_list(config2) do
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

  defp validate!(config, file) do
    Enum.all?(config, fn
      {app, value} when is_atom(app) ->
        if Keyword.keyword?(value) do
          true
        else
          raise ArgumentError,
                "expected config for app #{inspect(app)} in #{Path.relative_to_cwd(file)} " <>
                  "to return keyword list, got: #{inspect(value)}"
        end

      _ ->
        false
    end)

    config
  end
end
