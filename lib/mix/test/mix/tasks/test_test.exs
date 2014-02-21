Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.TestTest do
  use MixTest.Case

  import Mix.Tasks.Test, only: [ex_unit_opts: 1]

  test "ex_unit_opts returns ex unit options" do
    assert ex_unit_opts([unknown: "ok", seed: 13]) == [seed: 13]
  end

  test "ex_unit_opts returns includes and excludes" do
    assert ex_unit_opts([include: "focus", include: "key:val"]) ==
           [include: [:focus, key: "val"]]

    assert ex_unit_opts([exclude: "focus", exclude: "key:val"]) ==
           [exclude: [:focus, key: "val"]]
  end

  test "ex_unit_opts translates only into includes and excludes" do
    assert ex_unit_opts([only: "focus"]) ==
           [exclude: [:test], include: [:focus]]

    assert ex_unit_opts([only: "focus", include: "special"]) ==
           [exclude: [:test], include: [:focus, :special]]
  end
end
