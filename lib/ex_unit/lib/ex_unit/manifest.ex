defmodule ExUnit.Manifest do
  @moduledoc false

  import Record

  defrecord :entry, [:last_run_status, :file]
  @type status :: :passed | :failed
  @type entry :: record(:entry, last_run_status: status, file: Path.t())
  @type test_id :: {module, name :: atom}
  @type t :: %{test_id => entry}

  @manifest_vsn 1

  @spec add_test(t, ExUnit.Test.t()) :: t
  def add_test(%{} = manifest, %ExUnit.Test{state: {ignored_state, _}})
      when ignored_state in [:skipped, :excluded],
      do: manifest

  def add_test(%{} = manifest, %ExUnit.Test{} = test) do
    status =
      case test.state do
        nil -> :passed
        {:failed, _} -> :failed
        {:invalid, _} -> :failed
      end

    entry = entry(last_run_status: status, file: test.tags.file)
    Map.put(manifest, {test.module, test.name}, entry)
  end

  @spec write!(t, Path.t()) :: :ok
  def write!(%{} = manifest, file) when is_binary(file) do
    binary = :erlang.term_to_binary({@manifest_vsn, manifest})
    Path.dirname(file) |> File.mkdir_p!()
    File.write!(file, binary)
  end

  @spec read(Path.t()) :: t
  def read(file) when is_binary(file) do
    with {:ok, binary} <- File.read(file),
         {:ok, {@manifest_vsn, manifest}} <- safe_binary_to_term(binary) do
      manifest
    else
      _ -> %{}
    end
  end

  defp safe_binary_to_term(binary) do
    {:ok, :erlang.binary_to_term(binary)}
  rescue
    ArgumentError ->
      :error
  end

  # Responsible for smartly merging an old and new manifest, using the following rules:
  #
  #   1. Entries in the new manifest are accepted as-is.
  #   2. Entries in the old manifest that are not in the new manifest are kept
  #      if the identified test either *definitely* exists or *might* exist.
  #
  # More specifically, old manifest entries that satisfy either of these
  # criteria are deleted:
  #
  #   1. The file the test came from no longer exists.
  #   2. The test no longer exists, as indicated by the module no longer
  #      exporting the test function. Note that we can only check this for
  #      test modules that have been loaded.
  @spec merge(t, t) :: t
  def merge(%{} = old_manifest, %{} = new_manifest) do
    old_manifest
    |> Enum.to_list()
    |> prune_deleted_tests(%{}, [])
    |> Map.new()
    |> Map.merge(new_manifest)
  end

  defp prune_deleted_tests([], _, acc), do: acc

  defp prune_deleted_tests([{{mod, name}, entry} | rest] = all, file_existence, acc) do
    file = entry(entry, :file)
    file_exists = Map.fetch(file_existence, file)

    cond do
      file_exists == :error ->
        # This is the first time we've looked up the existence of the file.
        # Cache the result and try again.
        file_existence = Map.put(file_existence, file, File.regular?(file))
        prune_deleted_tests(all, file_existence, acc)

      file_exists == {:ok, false} ->
        # The file does not exist, so we should prune the test.
        prune_deleted_tests(rest, file_existence, acc)

      Code.ensure_loaded?(mod) and not function_exported?(mod, name, 1) ->
        # The test module has been loaded, but the test no longer exists, so prune it.
        prune_deleted_tests(rest, file_existence, acc)

      true ->
        # The file exists, but the test module was not loaded. We do not know
        # if the test still exists, so we err on the side of keeping it.
        prune_deleted_tests(rest, file_existence, [{{mod, name}, entry} | acc])
    end
  end
end
