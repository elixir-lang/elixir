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
    assert ExUnit.Filters.eval([case: "Foo"], [:os], [case: Foo])    == :ok
  end

  test "evaluating filter matches regexes" do
    assert ExUnit.Filters.eval([os: ~r"win"], [], [os: :win])           == :ok
    assert ExUnit.Filters.eval([os: ~r"mac"], [os: :unix], [os: :unix]) == {:error, :os}
  end

  test "parsing filters" do
    assert ExUnit.Filters.parse(["run"]) == [:run]
    assert ExUnit.Filters.parse(["run:true"]) == [run: "true"]
    assert ExUnit.Filters.parse(["run:test"]) == [run: "test"]
    assert ExUnit.Filters.parse(["line:9"]) == [line: "9"]
  end

  test "file paths with line numbers" do
    assert ExUnit.Filters.parse_path("test/some/path.exs:123") ==
      {"test/some/path.exs", [exclude: [:test], include: [line: "123"]]}

    assert ExUnit.Filters.parse_path("test/some/path.exs") ==
      {"test/some/path.exs", []}

    assert ExUnit.Filters.parse_path("test/some/path.exs:123notreallyalinenumber123") ==
      {"test/some/path.exs:123notreallyalinenumber123", []}

    assert ExUnit.Filters.parse_path("C:\\some\\path.exs:123") ==
      {"C:\\some\\path.exs", [exclude: [:test], include: [line: "123"]]}

    assert ExUnit.Filters.parse_path("C:\\some\\path.exs") ==
      {"C:\\some\\path.exs", []}

    assert ExUnit.Filters.parse_path("C:\\some\\path.exs:123notreallyalinenumber123") ==
      {"C:\\some\\path.exs:123notreallyalinenumber123", []}
  end
end
