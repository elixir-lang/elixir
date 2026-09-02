# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule ExUnit.FailuresManifest do
  @moduledoc false

  @opaque t :: {passed, failed}

  @typep passed :: [ExUnit.test_id()]
  @typep failed :: %{optional(ExUnit.test_id()) => test_file :: Path.t()}

  @manifest_vsn 1

  @spec new() :: t
  def new, do: {[], %{}}

  @spec put_test(t, ExUnit.Test.t()) :: t
  def put_test({_passed, _failed} = manifest, %ExUnit.Test{state: {ignored_state, _}})
      when ignored_state in [:skipped, :excluded],
      do: manifest

  def put_test({passed, failed}, %ExUnit.Test{state: nil} = test) do
    test_id = {test.module, test.name}
    {[test_id | passed], failed}
  end

  def put_test({passed, failed}, %ExUnit.Test{state: {failed_state, _}} = test)
      when failed_state in [:failed, :invalid] do
    test_id = {test.module, test.name}

    {passed, Map.put(failed, test_id, test.tags.file)}
  end

  @spec update!(t, Path.t()) :: :ok
  def update!({passed, failed}, file) when is_binary(file) do
    manifest =
      file
      |> read()
      |> prune_deleted_tests()
      |> Map.drop(passed)
      |> Map.merge(failed)

    binary = :erlang.term_to_binary({@manifest_vsn, manifest})
    Path.dirname(file) |> File.mkdir_p!()
    File.write!(file, binary)
  end

  @spec fail_all!(Path.t()) :: :ok
  def fail_all!(file) when is_binary(file) do
    binary = :erlang.term_to_binary({@manifest_vsn, :all})
    Path.dirname(file) |> File.mkdir_p!()
    File.write!(file, binary)
  end

  @spec read(Path.t()) :: failed
  def read(file) when is_binary(file) do
    with {:ok, binary} <- File.read(file),
         {:ok, {@manifest_vsn, %{} = manifest}} <- safe_binary_to_term(binary) do
      manifest
    else
      _ -> %{}
    end
  end

  @spec info(Path.t()) :: {MapSet.t(Path.t()), MapSet.t(ExUnit.test_id())} | :all
  def info(file) when is_binary(file) do
    with {:ok, binary} <- File.read(file),
         {:ok, {@manifest_vsn, manifest}} <- safe_binary_to_term(binary) do
      case manifest do
        :all ->
          :all

        %{} ->
          {manifest |> Map.values() |> MapSet.new(), manifest |> Map.keys() |> MapSet.new()}
      end
    else
      _ -> {MapSet.new(), MapSet.new()}
    end
  end

  defp safe_binary_to_term(binary) do
    {:ok, :erlang.binary_to_term(binary)}
  rescue
    ArgumentError -> :error
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

      Code.loaded?(mod) and not function_exported?(mod, name, 1) ->
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
