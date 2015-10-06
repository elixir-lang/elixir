# Manifest file where we treat Elixir and SCMs as a dependency.
defmodule Mix.Dep.ElixirSCM do
  @moduledoc false
  @manifest ".compile.elixir_scm"
  @manifest_vsn :v1

  def manifest(manifest_path \\ Mix.Project.manifest_path) do
    Path.join(manifest_path, @manifest)
  end

  def update(manifest_path \\ Mix.Project.manifest_path) do
    config = Mix.Project.config
    data = {@manifest_vsn, System.version, config[:build_scm]}
    File.mkdir_p!(manifest_path)
    File.write!(manifest(manifest_path), :io_lib.format('~p.~n', [data]))
    :ok
  end

  def read(manifest_path \\ Mix.Project.manifest_path) do
    case :file.consult(manifest(manifest_path)) do
      {:ok, [{@manifest_vsn, vsn, scm}]} ->
        {:ok, vsn, scm}
      {:error, {_, :erl_parse, _}} ->
        {:ok, "1.0.0", nil} # Force old version if file exists but old format
      _ ->
        :error
    end
  end
end
