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
    File.mkdir_p!(manifest_path)

    manifest_data =
      {@manifest_vsn, System.version, config[:build_scm]}
      |> :erlang.term_to_binary(compressed: 9)

    File.write!(manifest(manifest_path), manifest_data)
  end

  def read(manifest_path \\ Mix.Project.manifest_path) do
    case File.read(manifest(manifest_path)) do
      {:ok, contents} ->
        try do
          :erlang.binary_to_term(contents)
        else
          {@manifest_vsn, vsn, scm} ->
            {:ok, vsn, scm}
          _ ->
            {:ok, "1.0.0", nil} # Force old version if file exists but old format
        rescue
          _ ->
            {:ok, "1.0.0", nil} # Force old version if file exists but old format
        end
      _ ->
        :error
    end
  end
end
