Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.FailuresManifestTest do
  use ExUnit.Case, async: false

  import ExUnit.FailuresManifest
  import ExUnit.TestHelpers, only: [tmp_path: 0, in_tmp: 2]

  @passed nil
  @skipped {:skipped, "reason"}
  @excluded {:excluded, "reason"}
  @failed {:failed, []}
  @invalid {:invalid, SomeMod}

  describe "files_with_failures/1" do
    test "returns the set of files with failures" do
      manifest =
        new()
        |> put_test(new_test(@failed, "file_1"))
        |> put_test(new_test(@failed, "file_2"))
        |> put_test(new_test(@passed, "file_3"))
        |> put_test(new_test(@failed, "file_1"))

      assert files_with_failures(manifest) == MapSet.new(["file_1", "file_2"])
    end
  end

  describe "failed_test_ids/1" do
    test "returns the set of failed test ids" do
      manifest =
        new()
        |> put_test(failed_1 = new_test(@failed))
        |> put_test(__passed = new_test(@passed))
        |> put_test(failed_2 = new_test(@invalid))

      assert failed_test_ids(manifest) == MapSet.new([test_id(failed_1), test_id(failed_2)])
    end
  end

  describe "put_test/2 when the test is not already in the manifest" do
    test "ignores passed tests since we only care to store failures" do
      assert put_test(new(), new_test(@passed)) == new()
    end

    test "stores failed tests" do
      test = new_test(@failed)
      assert put_test(new(), test) == %{test_id(test) => file(test)}
    end

    test "stores invalid tests" do
      test = new_test(@invalid)
      assert put_test(new(), test) == %{test_id(test) => file(test)}
    end

    test "ignores skipped tests since we know nothing about their pass/fail status" do
      assert put_test(new(), new_test(@skipped)) == new()
    end

    test "ignores excluded tests since we know nothing about their pass/fail status" do
      assert put_test(new(), new_test(@excluded)) == new()
    end
  end

  describe "put_test/2 when the test is already in the manifest" do
    setup do
      failed_test = new_test(@failed)
      manifest = put_test(new(), failed_test)
      {:ok, %{failed_test: failed_test, manifest: manifest}}
    end

    test "removes a newly passed test, since it is no longer failing", context do
      test = %{context.failed_test | state: @passed}
      assert put_test(context.manifest, test) == new()
    end

    test "stores failed tests, updating the stored file value", context do
      test = %{context.failed_test | tags: %{file: "some-other-file"}}
      assert put_test(context.manifest, test) == %{test_id(test) => file(test)}
    end

    test "stores invalid tests, updating the stored file value", context do
      test = %{context.failed_test | tags: %{file: "some-other-file"}, state: @invalid}
      assert put_test(context.manifest, test) == %{test_id(test) => file(test)}
    end

    test "ignores skipped tests since we know nothing about their pass/fail status", context do
      test = %{context.failed_test | state: @skipped}
      assert put_test(context.manifest, test) == context.manifest
    end

    test "ignores excluded tests since we know nothing about their pass/fail status", context do
      test = %{context.failed_test | state: @excluded}
      assert put_test(context.manifest, test) == context.manifest
    end
  end

  @manifest_path "example.manifest"

  describe "write!/2" do
    test "stores a manifest that can later be read with read/1", context do
      manifest = non_blank_manifest(context)

      in_tmp(context.test, fn ->
        assert write!(manifest, @manifest_path) == :ok
        assert read(@manifest_path) == manifest
      end)
    end

    test "prunes tests from files that no longer exist", context do
      test = new_test(@failed, %{context | file: "missing_file.exs"})

      in_tmp(context.test, fn ->
        new()
        |> put_test(test)
        |> write!(@manifest_path)

        assert read(@manifest_path) == new()
      end)
    end

    test "keeps tests from modules that were not loaded", context do
      test = new_test(@failed, %{context | module: SomeUnloadedModule})
      manifest = new() |> put_test(test)

      in_tmp(context.test, fn ->
        write!(manifest, @manifest_path)
        assert read(@manifest_path) == manifest
      end)
    end

    test "prunes tests defined in a function that no longer exists", context do
      test = new_test(@failed, %{context | test: :not_a_function_anymore})

      in_tmp(context.test, fn ->
        new()
        |> put_test(test)
        |> write!(@manifest_path)

        assert read(@manifest_path) == new()
      end)
    end
  end

  describe "read/1" do
    test "returns a blank manifest when loading a file that does not exit" do
      path = tmp_path() <> "missing.manifest"
      refute File.exists?(path)
      assert read(path) == new()
    end

    test "returns a blank manifest when the file is corrupted", context do
      manifest = non_blank_manifest(context)

      in_tmp(context.test, fn ->
        assert write!(manifest, @manifest_path) == :ok
        corrupted = "corrupted" <> File.read!(@manifest_path)
        File.write!(@manifest_path, corrupted)
        assert read(@manifest_path) == new()
      end)
    end

    test "returns a blank manifest when the file was saved at a prior version", context do
      manifest = non_blank_manifest(context)

      in_tmp(context.test, fn ->
        assert write!(manifest, @manifest_path) == :ok
        assert {vsn, ^manifest} = @manifest_path |> File.read!() |> :erlang.binary_to_term()
        File.write!(@manifest_path, :erlang.term_to_binary({vsn + 1, manifest}))

        assert read(@manifest_path) == new()
      end)
    end
  end

  defp new_test(state, file \\ "file")

  defp new_test(state, %{} = context) do
    %ExUnit.Test{
      state: state,
      module: context.module,
      name: context.test,
      tags: %{file: context.file}
    }
  end

  defp new_test(state, file) do
    %ExUnit.Test{
      state: state,
      module: SomeMod,
      name: :"test #{System.unique_integer()}",
      tags: %{file: file}
    }
  end

  defp test_id(test), do: {test.module, test.name}

  defp file(test), do: test.tags.file

  defp non_blank_manifest(context), do: new() |> put_test(new_test(@failed, context))
end
