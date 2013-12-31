Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.FiltersTest do
  use ExUnit.Case, async: true

  doctest ExUnit.Filters

  test "evaluating filters" do
    include = [os: [:unix, :win32], type: [:unit]]
    exclude = []

    assert :ok = ExUnit.Filters.eval(include, exclude, [os: :unix])
    assert :ok = ExUnit.Filters.eval(include, exclude, [os: :unix, type: :unit])
    refute :ok = ExUnit.Filters.eval(include, exclude, [os: :unix, type: :integration])
    assert :ok = ExUnit.Filters.eval(include, exclude, [os: :win32])
    assert :ok = ExUnit.Filters.eval(include, exclude, [os: :win32, type: :unit])
    refute :ok = ExUnit.Filters.eval(include, exclude, [os: :win32, type: :integration])
    assert :ok = ExUnit.Filters.eval(include, exclude, [os: :unix, os: :win32])
  end

  test "parsing filters" do
    assert ExUnit.Filters.parse(["run"]) == [run: true]
    assert ExUnit.Filters.parse(["run:true"]) == [run: true]
    assert ExUnit.Filters.parse(["run:test"]) == [run: "test"]
  end
end
