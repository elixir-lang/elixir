defmodule Mix.Local.Fetcher do
  @moduledoc false

  @doc """
  Generates a new mix project in a temporary directory with the given
  `package_name` and `dep_spec` added to a mix.exs. Then, `in_fetcher` is
  executed in the fetcher project. By default, this fetches the dependencies,
  but you can provide an `in_fetcher` during test or for other purposes. After
  the `in_fetcher` is executed, `in_package` is executed in the now (presumably)
  fetched package, with the package's config overridden with the deps_path and
  lockfile of the fetcher package. Also, the Mix env is set to :prod.
  """
  @spec fetch(String.t, tuple, ((atom) -> any), ((atom) -> any)) :: any
  def fetch(package_name, dep_spec, in_fetcher \\ &in_fetcher/1, in_package) do
    with_tmp_dir fn tmp_path ->
      File.mkdir_p!(tmp_path)

      File.write! Path.join(tmp_path, "mix.exs"), """
      defmodule Mix.Local.Fetcher.Mixfile do
        use Mix.Project

        def project do
          [app: Mix.Local.Fetcher,
           version: "1.0.0",
           deps: [#{inspect dep_spec}]]
        end
      end
      """

      with_mix_env_prod fn ->
        Mix.Project.in_project(Mix.Local.Fetcher, tmp_path, in_fetcher)

        package_path = Path.join([tmp_path, "deps", package_name])
        package_name = String.to_atom(package_name)
        post_config = [
          deps_path: Path.join(tmp_path, "deps"),
          lockfile: Path.join(tmp_path, "mix.lock")
        ]

        Mix.Project.in_project(package_name, package_path, post_config, in_package)
      end
    end
  end

  defp in_fetcher(_mixfile) do
    Mix.Task.run("deps.get", [])
  end

  defp with_tmp_dir(fun) do
    unique = :crypto.strong_rand_bytes(4) |> Base.url_encode64(padding: false)
    tmp_path = Path.join(System.tmp_dir!(), "mix-local-fetcher-" <> unique)

    try do
      fun.(tmp_path)
    after
      File.rm_rf!(tmp_path)
    end
  end

  defp with_mix_env_prod(fun) do
    previous_env = Mix.env()

    try do
      Mix.env(:prod)
      fun.()
    after
      Mix.env(previous_env)
    end
  end
end
