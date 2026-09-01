# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.FailuresManifestTest do
  use ExUnit.Case, async: false

  import ExUnit.FailuresManifest

  @passed nil
  @skipped {:skipped, "reason"}
  @excluded {:excluded, "reason"}
  @failed {:failed, []}
  @invalid {:invalid, %ExUnit.TestModule{}}
  @manifest_path "example.manifest"

  describe "info/1" do
    @tag :tmp_dir
    test "returns the sets of files and test IDs with failures", context do
      manifest =
        new()
        |> put_test(failed_1 = new_test(@failed, context))
        |> put_test(failed_2 = new_test(@failed, context))
        |> put_test(new_test(@passed, context))
        |> put_test(invalid_1 = new_test(@invalid, context))

      File.cd!(context.tmp_dir, fn ->
        write!(manifest, @manifest_path)

        assert info(@manifest_path) ==
                 {MapSet.new([context.file]),
                  MapSet.new([test_id(failed_1), test_id(failed_2), test_id(invalid_1)])}
      end)
    end

    @tag :tmp_dir
    test "returns all when the whole suite should be considered as failed", context do
      File.cd!(context.tmp_dir, fn ->
        fail_all!(@manifest_path)
        assert info(@manifest_path) == :all
      end)
    end

    @tag :tmp_dir
    test "returns no information when loading a file that does not exit", context do
      path = Path.join(context.tmp_dir, "missing.manifest")
      refute File.exists?(path)
      assert info(path) == {MapSet.new(), MapSet.new()}
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

  describe "put_test/2 with parameterized tests" do
    test "a passing variant does not erase a failing sibling variant from the manifest" do
      failing = new_test(@failed)
      failing = %{failing | parameters: %{value: :a}}
      passing = %{failing | state: @passed, parameters: %{value: :b}}

      manifest =
        new()
        |> put_test(failing)
        |> put_test(passing)

      assert manifest == %{test_id(failing) => file(failing)}
    end

    test "distinct failing variants both keep their own manifest entry", context do
      a_variant = %{new_test(@failed, context) | parameters: %{value: :a}}
      b_variant = %{new_test(@failed, context) | parameters: %{value: :b}}

      manifest =
        new()
        |> put_test(a_variant)
        |> put_test(b_variant)

      assert manifest == %{
               test_id(a_variant) => file(a_variant),
               test_id(b_variant) => file(b_variant)
             }
    end
  end

  describe "test_id/1" do
    test "disambiguates parameterized variants of the same test" do
      no_params = new_test(@passed)
      assert test_id(no_params) == {no_params.module, no_params.name}

      with_params = %{no_params | parameters: %{value: :a}}
      assert test_id(with_params) == {with_params.module, with_params.name, %{value: :a}}

      with_params_b = %{no_params | parameters: %{value: :b}}
      assert test_id(with_params) != test_id(with_params_b)
    end
  end

  describe "write!/2" do
    @tag :tmp_dir
    test "stores a manifest that can later be read with read/1", context do
      manifest = non_blank_manifest(context)

      File.cd!(context.tmp_dir, fn ->
        assert write!(manifest, @manifest_path) == :ok
        assert read(@manifest_path) == manifest
      end)
    end

    @tag :tmp_dir
    test "prunes tests from files that no longer exist", context do
      test = new_test(@failed, %{context | file: "missing_file.exs"})

      File.cd!(context.tmp_dir, fn ->
        new()
        |> put_test(test)
        |> write!(@manifest_path)

        assert read(@manifest_path) == new()
      end)
    end

    @tag :tmp_dir
    test "keeps tests from modules that were not loaded", context do
      test = new_test(@failed, %{context | module: SomeUnloadedModule})
      manifest = new() |> put_test(test)

      File.cd!(context.tmp_dir, fn ->
        write!(manifest, @manifest_path)
        assert read(@manifest_path) == manifest
      end)
    end

    @tag :tmp_dir
    test "prunes tests defined in a function that no longer exists", context do
      test = new_test(@failed, %{context | test: :not_a_function_anymore})

      File.cd!(context.tmp_dir, fn ->
        new()
        |> put_test(test)
        |> write!(@manifest_path)

        assert read(@manifest_path) == new()
      end)
    end
  end

  describe "read/1" do
    @tag :tmp_dir
    test "returns a blank manifest when loading a file that does not exit", context do
      path = Path.join(context.tmp_dir, "missing.manifest")
      refute File.exists?(path)
      assert read(path) == new()
    end

    @tag :tmp_dir
    test "returns a blank manifest when the file is corrupted", context do
      manifest = non_blank_manifest(context)

      File.cd!(context.tmp_dir, fn ->
        assert write!(manifest, @manifest_path) == :ok
        corrupted = "corrupted" <> File.read!(@manifest_path)
        File.write!(@manifest_path, corrupted)
        assert read(@manifest_path) == new()
      end)
    end

    @tag :tmp_dir
    test "returns a blank manifest when the file was saved at a prior version", context do
      manifest = non_blank_manifest(context)

      File.cd!(context.tmp_dir, fn ->
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
      name: String.to_unsafe_atom("test #{System.unique_integer()}"),
      tags: %{file: file}
    }
  end

  defp file(test), do: test.tags.file

  defp non_blank_manifest(context), do: new() |> put_test(new_test(@failed, context))
end
