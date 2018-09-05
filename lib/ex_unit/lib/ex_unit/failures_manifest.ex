defmodule ExUnit.FailuresManifest do
  @moduledoc false

  @type test_id :: {module, name :: atom}
  @opaque t :: %{test_id => test_file :: Path.t()}

  @manifest_vsn 1

  @spec new() :: t
  def new, do: %{}

  @spec files_with_failures(t) :: MapSet.t(Path.t())
  def files_with_failures(%{} = manifest) do
    manifest
    |> Map.values()
    |> MapSet.new()
  end

  @spec failed_test_ids(t) :: MapSet.t(test_id)
  def failed_test_ids(%{} = manifest) do
    manifest
    |> Map.keys()
    |> MapSet.new()
  end

  @spec put_test(t, ExUnit.Test.t()) :: t
  def put_test(%{} = manifest, %ExUnit.Test{state: {ignored_state, _}})
      when ignored_state in [:skipped, :excluded, :not_executed],
      do: manifest

  def put_test(%{} = manifest, %ExUnit.Test{state: nil} = test) do
    Map.delete(manifest, {test.module, test.name})
  end

  def put_test(%{} = manifest, %ExUnit.Test{state: {failed_state, _}} = test)
      when failed_state in [:failed, :invalid] do
    Map.put(manifest, {test.module, test.name}, test.tags.file)
  end

  @spec write!(t, Path.t()) :: :ok
  def write!(manifest, file) when is_binary(file) do
    manifest = prune_deleted_tests(manifest)
    binary = :erlang.term_to_binary({@manifest_vsn, manifest})
    Path.dirname(file) |> File.mkdir_p!()
    File.write!(file, binary)
  end

  @spec read(Path.t()) :: t
  def read(file) when is_binary(file) do
    with {:ok, binary} <- File.read(file),
         {:ok, {@manifest_vsn, manifest}} when is_map(manifest) <- safe_binary_to_term(binary) do
      manifest
    else
      _ -> new()
    end
  end

  defp safe_binary_to_term(binary) do
    {:ok, :erlang.binary_to_term(binary)}
  rescue
    ArgumentError ->
      :error
  end

  defp prune_deleted_tests(manifest) do
    Map.drop(manifest, find_deleted_tests(Enum.to_list(manifest), %{}, []))
  end

  defp find_deleted_tests([], _file_existence, deleted_tests), do: deleted_tests

  defp find_deleted_tests([{{mod, name} = id, file} | rest] = all, file_existence, acc) do
    file_exists = Map.fetch(file_existence, file)

    cond do
      file_exists == :error ->
        # This is the first time we've looked up the existence of the file.
        # Cache the result and try again.
        file_existence = Map.put(file_existence, file, File.regular?(file))
        find_deleted_tests(all, file_existence, acc)

      file_exists == {:ok, false} ->
        # The file does not exist, so the test has been deleted.
        find_deleted_tests(rest, file_existence, [id | acc])

      :code.is_loaded(mod) != false and not function_exported?(mod, name, 1) ->
        # The test module has been loaded, but the test no longer exists.
        find_deleted_tests(rest, file_existence, [id | acc])

      true ->
        # The file exists and the test module was not loaded (which means the test
        # *might* still exist) or the function is exported (which means the test
        # *definitely* still exists). Either way, we do not want to prune it.
        find_deleted_tests(rest, file_existence, acc)
    end
  end
end
