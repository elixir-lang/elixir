Code.require_file("../test_helper.exs", __DIR__)

defmodule Config.ReaderTest do
  use ExUnit.Case, async: true

  doctest Config.Reader
  import PathHelpers

  test "read_imports!/2" do
    assert Config.Reader.read_imports!(fixture_path("configs/good_kw.exs")) ==
             {[my_app: [key: :value]], [fixture_path("configs/good_kw.exs")]}

    assert Config.Reader.read_imports!(fixture_path("configs/good_config.exs")) ==
             {[my_app: [key: :value]], [fixture_path("configs/good_config.exs")]}

    assert Config.Reader.read_imports!(fixture_path("configs/good_import.exs")) ==
             {[my_app: [key: :value]],
              [fixture_path("configs/good_config.exs"), fixture_path("configs/good_import.exs")]}

    assert Config.Reader.read_imports!(fixture_path("configs/nested_import.exs")) ==
             {[], [fixture_path("configs/nested_import.exs")]}

    assert_raise ArgumentError,
                 ~r"expected config for app :sample in .*/bad_app.exs to return keyword list",
                 fn -> Config.Reader.read_imports!(fixture_path("configs/bad_app.exs")) end

    assert_raise Code.LoadError,
                 fn ->
                   Config.Reader.read_imports!(fixture_path("configs/bad_root.exs"))
                 end

    assert_raise Code.LoadError,
                 fn ->
                   Config.Reader.read_imports!(fixture_path("configs/bad_import.exs"))
                 end
  end

  test "read!/2" do
    assert Config.Reader.read!(fixture_path("configs/good_kw.exs")) ==
             [my_app: [key: :value]]

    assert Config.Reader.read!(fixture_path("configs/good_config.exs")) ==
             [my_app: [key: :value]]

    assert Config.Reader.read!(fixture_path("configs/good_import.exs")) ==
             [my_app: [key: :value]]
  end
end
