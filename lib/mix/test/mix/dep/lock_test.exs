Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Dep.LockTest do
  use MixTest.Case

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "creates new lock and manifest files", context do
    in_tmp context.test, fn ->
      Mix.Dep.Lock.write %{foo: :bar}
      assert File.regular? "mix.lock"
      assert File.regular? "_build/dev/lib/sample/.compile.lock"
    end
  end

  test "does not touch manifest file there is no change", context do
    in_tmp context.test, fn ->
      Mix.Dep.Lock.write %{foo: :bar, bar: :bat}
      File.rm! "_build/dev/lib/sample/.compile.lock"

      Mix.Dep.Lock.write %{bar: :bat, foo: :bar}
      refute File.regular? "_build/dev/lib/sample/.compile.lock"
    end
  end

  test "raises a proper error for merge conflicts", context do
    in_tmp context.test, fn ->
      File.write "mix.lock", ~S"""
      %{"dep": {:hex, :dep, "0.1.0"},
      <<<<<<< HEAD
        "foo": {:hex, :foo, "0.1.0"},
      =======
        "bar": {:hex, :bar, "0.1.0"},
      >>>>>>> foobar
        "baz": {:hex, :baz, "0.1.0"}}
      """
      assert_raise Mix.Error, ~r/Your mix\.lock contains merge conflicts/, fn ->
        Mix.Dep.Lock.read()
      end
    end
  end
end
