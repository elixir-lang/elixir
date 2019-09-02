defmodule Config.Reader do
  @moduledoc """
  API for reading config files defined with `Config`.

  ## As a provider

  `Config.Reader` can also be used as a `Config.Provider`. When used
  as a provider, it expects a single argument: the configuration path
  (as outlined in `t:Config.Provider.config_path/0`) for the file to
  be read and loaded during the system boot.

  For example, if you expect the target system to have a config file
  in an absolute path, you can configure your `mix release` as:

      config_providers: [{Config.Reader, "/etc/config.json"}]

  Or if you want to read a custom path inside the release:

      config_provider: [{Config.Reader, {:system, "RELEASE_ROOT", "/config.exs"}}]

  Note by default Mix releases supports runtime configuration via
  a `config/releases.exs`. If a `config/releases.exs` exists in your
  application, it is automatically copied inside the release and
  automatically set as a config provider.
  """

  @behaviour Config.Provider

  @impl true
  def init(path) do
    Config.Provider.validate_config_path!(path)
    path
  end

  @impl true
  def load(config, path) do
    merge(config, path |> Config.Provider.resolve_config_path!() |> read!())
  end

  @doc """
  Reads the configuration file.

  The same as `read_imports!/2` but only returns the configuration
  in the given file, without returning the imported paths.

  It exists for convenience purposes. For example, you could
  invoke it inside your `mix.exs` to read some external data
  you decided to move to a configuration file:

      releases: Config.Reader.read!("rel/releases.exs")

  """
  @doc since: "1.9.0"
  @spec read!(Path.t(), [Path.t()]) :: keyword
  def read!(file, imported_paths \\ [])
      when is_binary(file) and is_list(imported_paths) do
    Config.__eval__!(file, imported_paths) |> elem(0)
  end

  @doc """
  Reads the given configuration file alongside its imports.

  It accepts a list of `imported_paths` that should raise if attempted
  to be imported again (to avoid recursive imports).

  It returns a tuple with the configuration and the imported paths.
  """
  @doc since: "1.9.0"
  @spec read_imports!(Path.t(), [Path.t()]) :: {keyword, [Path.t()]}
  def read_imports!(file, imported_paths \\ [])
      when is_binary(file) and is_list(imported_paths) do
    Config.__eval__!(file, imported_paths)
  end

  @doc """
  Merges two configurations.

  The configurations are merged together with the values in
  the second one having higher preference than the first in
  case of conflicts. In case both values are set to keyword
  lists, it deep merges them.

  ## Examples

      iex> Config.Reader.merge([app: [k: :v1]], [app: [k: :v2]])
      [app: [k: :v2]]

      iex> Config.Reader.merge([app: [k: [v1: 1, v2: 2]]], [app: [k: [v2: :a, v3: :b]]])
      [app: [k: [v1: 1, v2: :a, v3: :b]]]

      iex> Config.Reader.merge([app1: []], [app2: []])
      [app1: [], app2: []]

  """
  @doc since: "1.9.0"
  @spec merge(keyword, keyword) :: keyword
  def merge(config1, config2) when is_list(config1) and is_list(config2) do
    Config.__merge__(config1, config2)
  end
end
