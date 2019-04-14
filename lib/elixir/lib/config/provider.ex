defmodule Config.Provider do
  @type config :: keyword
  @type state :: term

  @callback init(term) :: state
  @callback boot(config, state) :: config

  @doc false
  defstruct [:path, :providers, :config]

  def validate_config_path!({:system, name, path})
      when is_binary(name) and is_binary(path),
      do: :ok

  def validate_config_path!(path) do
    if is_binary(path) and Path.type(path) != :relative do
      :ok
    else
      raise ArgumentError, """
      expected configuration path to be:

        * a binary representing an absolute path
        * a tuple {:system, system_var, relative_path} where the config is \
          a relative path to the value in the system variable `system_var`

      Got: #{inspect(path)}
      """
    end
  end

  def resolve_config_path!(path) when is_binary(path), do: path
  def resolve_config_path!({:system, name, path}), do: Path.join(System.fetch_env!(name), path)

  @doc false
  def init(path, providers, config) when is_list(providers) and is_list(config) do
    validate_config_path!(path)
    providers = for {provider, opts} <- providers, do: {provider, provider.init(opts)}
    %Config.Provider{path: path, providers: providers, config: config}
  end

  @doc false
  def boot(app, key, restart_fun \\ &System.restart/0) do
    case :application.get_env(app, key) do
      {:ok, %Config.Provider{} = provider} ->
        path = resolve_config_path!(provider.path)
        validate_no_cyclic_boot!(path)
        config = [{app, [{key, :booted}]} | provider.config]
        config = Enum.reduce(provider.providers, config, &boot_provider/2)
        write_config!(path, config)
        restart_fun.()

      _ ->
        :skip
    end
  end

  defp validate_no_cyclic_boot!(path) do
    if System.get_env("ELIXIR_CONFIG_PROVIDER_BOOTED") do
      bad_path_abort("Got infinite loop when running Config.Provider", path)
    else
      System.put_env("ELIXIR_CONFIG_PROVIDER_BOOTED", "1")
    end
  end

  defp boot_provider({provider, state}, acc) do
    case provider.boot(acc, state) do
      term when is_list(term) ->
        term

      term ->
        abort("Expected provider #{inspect(provider)} to return a list, got: #{inspect(term)}")
    end
  end

  defp write_config!(path, config) do
    {date, time} = :erlang.localtime()
    args = [date, time, config]
    contents = :io_lib.format("%% coding: utf-8~n%% config generated at ~p ~p~n~p.~n", args)
    _ = File.mkdir_p(Path.dirname(path))

    case File.write(path, contents) do
      :ok ->
        :ok

      {:error, reason} ->
        bad_path_abort(
          "Could not write runtime configuration due to reason: #{inspect(reason)}",
          path
        )
    end
  end

  defp bad_path_abort(msg, path) do
    abort(
      msg <>
        ". Please make sure #{inspect(path)} is writable and accessible " <>
        "or choose a different path"
    )
  end

  defp abort(msg) do
    IO.puts(:stderr, "ERROR! " <> msg)
    raise(msg)
  end
end
