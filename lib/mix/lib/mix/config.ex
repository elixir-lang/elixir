defmodule Mix.Config do
  @moduledoc false

  @deprecated "Use the Config module instead"
  defmacro __using__(_) do
    quote do
      import Mix.Config, only: [config: 2, config: 3, import_config: 1]
    end
  end

  @deprecated "Use the Config module instead"
  defdelegate config(root_key, opts), to: Config

  @deprecated "Use the Config module instead"
  defdelegate config(root_key, key, opts), to: Config

  @deprecated "Use the Config module instead"
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
      Config.__import__!(path)
    end

    :ok
  end

  ## Mix API

  @deprecated "Use Config.Reader.read_imports!/2 instead"
  def eval!(file, imported_paths \\ []) do
    Config.Reader.read_imports!(file,
      imports: imported_paths,
      env: Mix.env(),
      target: Mix.target()
    )
  end

  @deprecated "Use Config.Reader.read!/2 instead"
  def read!(file, imported_paths \\ []) do
    Config.Reader.read!(file, imports: imported_paths, env: Mix.env(), target: Mix.target())
  end

  @deprecated "Use Config.Reader.merge/2 instead"
  def merge(config1, config2) do
    Config.__merge__(config1, config2)
  end

  @deprecated "Use Application.put_all_env/2 instead"
  def persist(config) do
    Application.put_all_env(config, persistent: true)
  end

  @deprecated "Use the Config.Reader module instead"
  def read_wildcard!(path, loaded_paths \\ []) do
    paths =
      if String.contains?(path, ~w(* ? [ {)) do
        Path.wildcard(path)
      else
        [path]
      end

    Enum.reduce(paths, [], &merge(&2, read!(&1, loaded_paths)))
  end

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
end
