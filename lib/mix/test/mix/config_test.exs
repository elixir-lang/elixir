Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.ConfigTest do
  use MixTest.Case, async: true

  doctest Mix.Config

  test "read/1" do
    assert Mix.Config.read(fixture_path("configs/good.exs")) ==
           [my_app: [key: :value]]

    msg = "expected config for app :sample to return keyword list, got: :oops"
    assert_raise ArgumentError, msg, fn ->
      Mix.Config.read fixture_path("configs/bad_app.exs")
    end

    msg = "expected config to return keyword list, got: :oops"
    assert_raise ArgumentError, msg, fn ->
      Mix.Config.read fixture_path("configs/bad_root.exs")
    end
  end

  test "persist/1" do
    assert Application.get_env(:my_app, :key) == nil
    Mix.Config.persist [my_app: [key: :value]]
    assert Application.get_env(:my_app, :key) == :value
  after
    Application.delete_env(:my_app, :key)
  end
end
