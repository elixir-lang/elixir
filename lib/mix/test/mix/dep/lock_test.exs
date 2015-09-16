Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Dep.LockTest do
  use MixTest.Case

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "creates new lock and manifest files" do
    in_fixture "no_mixfile", fn ->
      Mix.Dep.Lock.write %{foo: :bar}
      assert File.regular? "mix.lock"
      assert File.regular? "_build/dev/lib/sample/.compile.lock"
    end
  end

  test "does not touch manifest file there is no change" do
    in_fixture "no_mixfile", fn ->
      Mix.Dep.Lock.write %{foo: :bar, bar: :bat}
      File.rm! "_build/dev/lib/sample/.compile.lock"

      Mix.Dep.Lock.write %{bar: :bat, foo: :bar}
      refute File.regular? "_build/dev/lib/sample/.compile.lock"
    end
  end

  test "stores status in manifest" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Dep.Lock.status == :error
      Mix.Dep.Lock.touch
      assert Mix.Dep.Lock.status == {:ok, System.version, nil}
    end
  end
end
