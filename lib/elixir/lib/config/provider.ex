defmodule Config.Provider do
  @type config :: keyword
  @type state :: term
  @type config_path :: {:system, binary(), binary()} | binary()

  @callback init(term) :: state
  @callback boot(config, state) :: config

  @doc false
  defstruct [:providers, :config_path, extra_config: [], prune_after_boot: true]

  @spec validate_config_path!(config_path) :: :ok
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
        * a tuple {:system, system_var, path} where the config is the \
          concatenation of the `system_var` with the given `path`

      Got: #{inspect(path)}
      """
    end
  end

  @spec resolve_config_path!(config_path) :: binary
  def resolve_config_path!(path) when is_binary(path), do: path
  def resolve_config_path!({:system, name, path}), do: System.fetch_env!(name) <> path

  @doc false
  def init(providers, config_path, opts \\ []) when is_list(providers) and is_list(opts) do
    validate_config_path!(config_path)
    providers = for {provider, init} <- providers, do: {provider, provider.init(init)}
    struct!(%Config.Provider{config_path: config_path, providers: providers}, opts)
  end

  @doc false
  def boot(app, key, restart_fun \\ &System.restart/0) do
    case :application.get_env(app, key) do
      {:ok, %Config.Provider{} = provider} ->
        path = resolve_config_path!(provider.config_path)
        validate_no_cyclic_boot!(path)

        read_config!(path)
        |> Config.__merge__([{app, [{key, booted_key(provider, path)}]} | provider.extra_config])
        |> run_providers(provider)
        |> write_config!(path)

        restart_fun.()

      {:ok, {:booted, path}} ->
        File.rm(path)
        :booted

      {:ok, :booted} ->
        :booted

      _ ->
        :skip
    end
  end

  defp booted_key(%{prune_after_boot: true}, path), do: {:booted, path}
  defp booted_key(%{prune_after_boot: false}, _path), do: :booted

  defp validate_no_cyclic_boot!(path) do
    if System.get_env("ELIXIR_CONFIG_PROVIDER_BOOTED") do
      bad_path_abort("Got infinite loop when running Config.Provider", path)
    else
      System.put_env("ELIXIR_CONFIG_PROVIDER_BOOTED", "1")
    end
  end

  defp read_config!(path) do
    case :file.consult(path) do
      {:ok, [inner]} ->
        inner

      {:error, reason} ->
        bad_path_abort(
          "Could not read runtime configuration due to reason: #{inspect(reason)}",
          path
        )
    end
  end

  defp run_providers(config, %{providers: providers}) do
    Enum.reduce(providers, config, fn {provider, state}, acc ->
      case provider.boot(acc, state) do
        term when is_list(term) ->
          term

        term ->
          abort("Expected provider #{inspect(provider)} to return a list, got: #{inspect(term)}")
      end
    end)
  end

  defp write_config!(config, path) do
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
