Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Deps.LockTest do
  use MixTest.Case

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  teardown do
    Mix.Project.pop
    :ok
  end

  test "creates new lock and manifest files" do
    in_fixture "no_mixfile", fn ->
      Mix.Deps.Lock.write [foo: :bar]
      assert File.regular? "mix.lock"
      assert File.regular? "_build/shared/lib/sample/.compile.lock"
    end
  end

  test "does not touch manifest file there is no change" do
    in_fixture "no_mixfile", fn ->
      Mix.Deps.Lock.write [foo: :bar, bar: :bat]
      File.rm! "_build/shared/lib/sample/.compile.lock"

      Mix.Deps.Lock.write [bar: :bat, foo: :bar]
      refute File.regular? "_build/shared/lib/sample/.compile.lock"
    end
  end

  test "stores version and env in manifest" do
    in_fixture "no_mixfile", fn ->
      assert nil? Mix.Deps.Lock.elixir_vsn
      assert nil? Mix.Deps.Lock.mix_env

      Mix.Deps.Lock.touch

      assert Mix.Deps.Lock.elixir_vsn == System.version
      assert Mix.Deps.Lock.mix_env == "dev"
    end
  end
end
