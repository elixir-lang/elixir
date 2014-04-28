Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.FiltersTest do
  use ExUnit.Case, async: true

  doctest ExUnit.Filters

  test "evaluating filters" do
    assert ExUnit.Filters.eval([], [:os], [])                == :ok
    assert ExUnit.Filters.eval([], [os: :win], [os: :unix])  == :ok
    assert ExUnit.Filters.eval([], [:os], [os: :unix])       == {:error, :os}
    assert ExUnit.Filters.eval([], [os: :unix], [os: :unix]) == {:error, :os}

    assert ExUnit.Filters.eval([os: :win], [], [])            == :ok
    assert ExUnit.Filters.eval([os: :win], [], [os: :unix])   == :ok
    assert ExUnit.Filters.eval([os: :win], [:os], [])         == :ok
    assert ExUnit.Filters.eval([os: :win], [:os], [os: :win]) == :ok

    assert ExUnit.Filters.eval([os: :win, os: :unix], [:os], [os: :win]) == :ok
  end

  test "evaluating filters matches integers" do
    assert ExUnit.Filters.eval([line: "1"], [], [line: 1])        == :ok
    assert ExUnit.Filters.eval([line: "1"], [line: 5], [line: 1]) == :ok
    assert ExUnit.Filters.eval([line: "1"], [:line], [line: 1])   == :ok
  end

  test "evaluating filter matches atoms" do
    assert ExUnit.Filters.eval([os: "win"], [], [os: :win])          == :ok
    assert ExUnit.Filters.eval([os: "win"], [os: :unix], [os: :win]) == :ok
    assert ExUnit.Filters.eval([os: "win"], [:os], [os: :win])       == :ok
  end

  test "parsing filters" do
    assert ExUnit.Filters.parse(["run"]) == [:run]
    assert ExUnit.Filters.parse(["run:true"]) == [run: "true"]
    assert ExUnit.Filters.parse(["run:test"]) == [run: "test"]
    assert ExUnit.Filters.parse(["line:9"]) == [line: "9"]
  end
end
