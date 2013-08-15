Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Deps.LockTest do
  use MixTest.Case

  test "creates new lock and manifest files" do
    in_fixture "no_mixfile", fn ->
      Mix.Deps.Lock.write [foo: :bar]
      assert File.regular? "mix.lock"
      assert File.regular? "ebin/.compile.lock"
    end
  end

  test "does not touch manifest file there is no change" do
    in_fixture "no_mixfile", fn ->
      Mix.Deps.Lock.write [foo: :bar, bar: :bat]
      File.rm! "ebin/.compile.lock"

      Mix.Deps.Lock.write [bar: :bat, foo: :bar]
      refute File.regular? "ebin/.compile.lock"
    end
  end
end
