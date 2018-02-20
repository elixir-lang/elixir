Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.ManifestTest do
  use ExUnit.Case, async: false

  import ExUnit.Manifest
  import ExUnit.TestHelpers, only: [tmp_path: 0, in_tmp: 2]

  describe "add_test/2" do
    test "ignores tests that have an invalid :file value (which can happen when returning a `:file` option from `setup`))" do
      test = %ExUnit.Test{state: nil, tags: %{file: :not_a_file}}

      assert add_test(new(), test) == new()
    end

    test "ignores skipped tests since we know nothing about their pass/fail status" do
      test = %ExUnit.Test{state: {:skipped, "reason"}}

      assert add_test(new(), test) == new()
    end

    test "ignores excluded tests since we know nothing about their pass/fail status" do
      test = %ExUnit.Test{state: {:excluded, "reason"}}

      assert add_test(new(), test) == new()
    end

    test "stores passed tests, keyed by module and name" do
      test = %ExUnit.Test{
        state: nil,
        module: SomeMod,
        name: :t1,
        tags: %{file: "file"}
      }

      assert add_test(new(), test) == [
               {{SomeMod, :t1}, entry(last_run_status: :passed, file: "file")}
             ]
    end

    test "stores failed tests, keyed by module and name" do
      test = %ExUnit.Test{
        state: {:failed, []},
        module: SomeMod,
        name: :t1,
        tags: %{file: "file"}
      }

      assert add_test(new(), test) == [
               {{SomeMod, :t1}, entry(last_run_status: :failed, file: "file")}
             ]
    end

    test "stores invalid tests as failed, keyed by module and name" do
      test = %ExUnit.Test{
        state: {:invalid, SomeMod},
        module: SomeMod,
        name: :t1,
        tags: %{file: "file"}
      }

      assert add_test(new(), test) == [
               {{SomeMod, :t1}, entry(last_run_status: :failed, file: "file")}
             ]
    end
  end

  @manifest_path "example.manifest"

  describe "write!/2 and read/1" do
    test "can roundtrip a manifest", context do
      manifest = non_blank_manifest()

      in_tmp(context.test, fn ->
        assert write!(manifest, @manifest_path) == :ok
        assert read(@manifest_path) == manifest
      end)
    end

    test "returns a blank manifest when loading a file that does not exit" do
      path = tmp_path() <> "missing.manifest"
      refute File.exists?(path)
      assert read(path) == new()
    end

    test "returns a blank manifest when the file is corrupted", context do
      manifest = non_blank_manifest()

      in_tmp(context.test, fn ->
        assert write!(manifest, @manifest_path) == :ok
        corrupted = "corrupted" <> File.read!(@manifest_path)
        File.write!(@manifest_path, corrupted)
        assert read(@manifest_path) == new()
      end)
    end

    test "returns a blank manifest when the file was saved at a prior version", context do
      manifest = non_blank_manifest()

      in_tmp(context.test, fn ->
        assert write!(manifest, @manifest_path) == :ok
        assert {vsn, ^manifest} = @manifest_path |> File.read!() |> :erlang.binary_to_term()
        File.write!(@manifest_path, :erlang.term_to_binary({vsn + 1, manifest}))

        assert read(@manifest_path) == new()
      end)
    end
  end

  defp non_blank_manifest do
    test = %ExUnit.Test{
      state: nil,
      module: SomeMod,
      name: :t1,
      tags: %{file: "file"}
    }

    add_test(new(), test)
  end

  describe "merge/2" do
    @existing_file_1 __ENV__.file
    @existing_file_2 Path.join(__DIR__, "../test_helper.exs")
    @missing_file "missing_file_test.exs"

    defmodule TestMod1 do
      def test_1(_), do: :ok
      def test_2(_), do: :ok
    end

    defmodule TestMod2 do
      def test_1(_), do: :ok
      def test_2(_), do: :ok
    end

    defp merge_and_sort(old, new) do
      old |> ExUnit.Manifest.merge(new) |> Enum.sort()
    end

    test "returns the new manifest when the old manifest is blank" do
      new_manifest = [{{TestMod1, :test_1}, entry()}]

      assert merge_and_sort(new(), new_manifest) == new_manifest
    end

    test "replaces old entries with their updated status" do
      old_manifest = [
        {{TestMod1, :test_1}, entry(last_run_status: :failed, file: @existing_file_1)},
        {{TestMod1, :test_2}, entry(last_run_status: :passed, file: @existing_file_1)}
      ]

      new_manifest = [
        {{TestMod1, :test_1}, entry(last_run_status: :passed, file: @existing_file_1)},
        {{TestMod1, :test_2}, entry(last_run_status: :failed, file: @existing_file_1)}
      ]

      assert merge_and_sort(old_manifest, new_manifest) == new_manifest
    end

    test "keeps old entries for test modules in existing files that were not part of this run" do
      old_manifest = [
        {{UnknownMod1, :test_1}, entry(last_run_status: :failed, file: @existing_file_1)},
        {{UnknownMod1, :test_2}, entry(last_run_status: :passed, file: @existing_file_1)}
      ]

      new_manifest = [
        {{TestMod2, :test_1}, entry(last_run_status: :passed, file: @existing_file_2)},
        {{TestMod2, :test_2}, entry(last_run_status: :failed, file: @existing_file_2)}
      ]

      assert merge_and_sort(old_manifest, new_manifest) == [
               {{TestMod2, :test_1}, entry(last_run_status: :passed, file: @existing_file_2)},
               {{TestMod2, :test_2}, entry(last_run_status: :failed, file: @existing_file_2)},
               {{UnknownMod1, :test_1}, entry(last_run_status: :failed, file: @existing_file_1)},
               {{UnknownMod1, :test_2}, entry(last_run_status: :passed, file: @existing_file_1)}
             ]
    end

    test "keeps old entries for tests that were loaded but skipped as part of this run" do
      old_manifest = [
        {{TestMod1, :test_1}, entry(last_run_status: :failed, file: @existing_file_1)},
        {{TestMod1, :test_2}, entry(last_run_status: :passed, file: @existing_file_1)}
      ]

      new_manifest = [
        {{TestMod1, :test_1}, entry(last_run_status: :passed, file: @existing_file_1)}
      ]

      assert merge_and_sort(old_manifest, new_manifest) == [
               {{TestMod1, :test_1}, entry(last_run_status: :passed, file: @existing_file_1)},
               {{TestMod1, :test_2}, entry(last_run_status: :passed, file: @existing_file_1)}
             ]
    end

    test "drops old entries from test files that no longer exist" do
      old_manifest = [
        {{TestMod2, :test_1}, entry(last_run_status: :failed, file: @missing_file)},
        {{TestMod2, :test_2}, entry(last_run_status: :passed, file: @missing_file)}
      ]

      new_manifest = [
        {{TestMod1, :test_1}, entry(last_run_status: :passed, file: @existing_file_1)},
        {{TestMod1, :test_2}, entry(last_run_status: :failed, file: @existing_file_1)}
      ]

      assert merge_and_sort(old_manifest, new_manifest) == [
               {{TestMod1, :test_1}, entry(last_run_status: :passed, file: @existing_file_1)},
               {{TestMod1, :test_2}, entry(last_run_status: :failed, file: @existing_file_1)}
             ]
    end

    test "drops old entries for deleted tests from loaded modules" do
      old_manifest = [
        {{TestMod2, :missing_1}, entry(last_run_status: :failed, file: @existing_file_1)},
        {{TestMod2, :missing_2}, entry(last_run_status: :passed, file: @existing_file_2)}
      ]

      new_manifest = [
        {{TestMod1, :test_1}, entry(last_run_status: :passed, file: @existing_file_1)},
        {{TestMod1, :test_2}, entry(last_run_status: :failed, file: @existing_file_1)}
      ]

      assert merge_and_sort(old_manifest, new_manifest) == [
               {{TestMod1, :test_1}, entry(last_run_status: :passed, file: @existing_file_1)},
               {{TestMod1, :test_2}, entry(last_run_status: :failed, file: @existing_file_1)}
             ]
    end
  end
end
