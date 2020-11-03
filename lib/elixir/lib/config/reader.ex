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

      config_providers: [{Config.Reader, "/etc/config.exs"}]

  Or if you want to read a custom path inside the release:

      config_providers: [{Config.Reader, {:system, "RELEASE_ROOT", "/config.exs"}}]

  You can also pass a keyword list of options to the reader,
  where the `:path` is a required key:

      config_providers: [
        {Config.Reader,
         path: "/etc/config.exs",
         env: :prod,
         imports: :disabled}
      ]

  Note by default Mix releases supports runtime configuration via
  a `config/runtime.exs`. If a `config/runtime.exs` exists in your
  application, it is automatically copied inside the release and
  automatically set as a config provider.
  """

  @behaviour Config.Provider

  @impl true
  def init(opts) when is_list(opts) do
    {path, opts} = Keyword.pop!(opts, :path)
    Config.Provider.validate_config_path!(path)
    {path, opts}
  end

  def init(path) do
    init(path: path)
  end

  @impl true
  def load(config, {path, opts}) do
    merge(config, path |> Config.Provider.resolve_config_path!() |> read!(opts))
  end

  @doc """
  Evaluates the configuration `contents` for the given `file`.

  Accepts the same options as `read!/2`.
  """
  @doc since: "1.11.0"
  @spec eval!(Path.t(), binary, keyword) :: keyword
  def eval!(file, contents, opts \\ [])
      when is_binary(file) and is_binary(contents) and is_list(opts) do
    Config.__eval__!(Path.expand(file), contents, opts) |> elem(0)
  end

  @doc """
  Reads the configuration file.

  ## Options

    * `:imports` - a list of already imported paths or `:disabled`
      to disable imports

    * `:env` - the environment the configuration file runs on.
      See `Config.config_env/0` for sample usage

    * `:target` - the target the configuration file runs on.
      See `Config.config_target/0` for sample usage

  """
  @doc since: "1.9.0"
  @spec read!(Path.t(), keyword) :: keyword
  def read!(file, opts \\ []) when is_binary(file) and is_list(opts) do
    file = Path.expand(file)
    Config.__eval__!(file, File.read!(file), opts) |> elem(0)
  end

  @doc """
  Reads the given configuration file and returns the configuration
  with its imports.

  Accepts the same options as `read!/2`. Although note the `:imports`
  option cannot be disabled in `read_imports!/2`.
  """
  @doc since: "1.9.0"
  @spec read_imports!(Path.t(), keyword) :: {keyword, [Path.t()]}
  def read_imports!(file, opts \\ []) when is_binary(file) and is_list(opts) do
    if opts[:imports] == :disabled do
      raise ArgumentError, ":imports must be a list of paths"
    end

    file = Path.expand(file)
    Config.__eval__!(file, File.read!(file), opts)
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
