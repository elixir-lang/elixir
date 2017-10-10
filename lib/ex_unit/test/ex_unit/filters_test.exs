Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.FiltersTest do
  use ExUnit.Case, async: true

  alias ExUnit.Filters
  doctest(Filters)

  test "evaluating filters" do
    assert Filters.eval([], [:os], %{}, []) == :ok
    assert Filters.eval([], [os: :win], %{os: :unix}, []) == :ok
    assert Filters.eval([], [:os], %{os: :unix}, []) == {:error, "due to os filter"}
    assert Filters.eval([], [os: :unix], %{os: :unix}, []) == {:error, "due to os filter"}

    assert Filters.eval([os: :win], [], %{}, []) == :ok
    assert Filters.eval([os: :win], [], %{os: :unix}, []) == :ok
    assert Filters.eval([os: :win], [:os], %{}, []) == :ok
    assert Filters.eval([os: :win], [:os], %{os: :win}, []) == :ok

    assert Filters.eval([os: :win, os: :unix], [:os], %{os: :win}, []) == :ok
  end

  test "evaluating filters with skip" do
    assert Filters.eval([], [], %{}, []) == :ok
    assert Filters.eval([], [], %{skip: true}, []) == {:error, "due to skip tag"}
    assert Filters.eval([], [], %{skip: "skipped"}, []) == {:error, "skipped"}
    assert Filters.eval([], [:os], %{skip: "skipped"}, []) == {:error, "skipped"}
    assert Filters.eval([:skip], [], %{skip: true}, []) == :ok
    assert Filters.eval([:skip], [], %{skip: "skipped"}, []) == :ok
  end

  test "evaluating filters matches integers" do
    assert Filters.eval([int: "1"], [], %{int: 1}, []) == :ok
    assert Filters.eval([int: "1"], [int: 5], %{int: 1}, []) == :ok
    assert Filters.eval([int: "1"], [:int], %{int: 1}, []) == :ok
  end

  test "evaluating filter matches atoms" do
    assert Filters.eval([os: "win"], [], %{os: :win}, []) == :ok
    assert Filters.eval([os: "win"], [os: :unix], %{os: :win}, []) == :ok
    assert Filters.eval([os: "win"], [:os], %{os: :win}, []) == :ok
    assert Filters.eval([module: "Foo"], [:os], %{module: Foo}, []) == :ok
  end

  test "evaluating filter matches regexes" do
    assert Filters.eval([os: ~r"win"], [], %{os: :win}, []) == :ok

    expected = {:error, "due to os filter"}
    assert Filters.eval([os: ~r"mac"], [os: :unix], %{os: :unix}, []) == expected
  end

  test "evaluating filter uses special rules for line" do
    tests = [
      %ExUnit.Test{tags: %{line: 3, describe_line: 2}},
      %ExUnit.Test{tags: %{line: 5, describe_line: nil}},
      %ExUnit.Test{tags: %{line: 8, describe_line: 7}},
      %ExUnit.Test{tags: %{line: 10, describe_line: 7}},
      %ExUnit.Test{tags: %{line: 13, describe_line: 12}}
    ]

    assert Filters.eval([line: "3"], [:line], %{line: 3, describe_line: 2}, tests) == :ok
    assert Filters.eval([line: "4"], [:line], %{line: 3, describe_line: 2}, tests) == :ok
    assert Filters.eval([line: "5"], [:line], %{line: 5, describe_line: nil}, tests) == :ok
    assert Filters.eval([line: "6"], [:line], %{line: 5, describe_line: nil}, tests) == :ok
    assert Filters.eval([line: "2"], [:line], %{line: 3, describe_line: 2}, tests) == :ok
    assert Filters.eval([line: "7"], [:line], %{line: 8, describe_line: 7}, tests) == :ok
    assert Filters.eval([line: "7"], [:line], %{line: 10, describe_line: 7}, tests) == :ok

    expected = {:error, "due to line filter"}

    assert Filters.eval([line: "1"], [:line], %{line: 3, describe_line: 2}, tests) == expected

    assert Filters.eval([line: "7"], [:line], %{line: 3, describe_line: 2}, tests) == expected

    assert Filters.eval([line: "7"], [:line], %{line: 5, describe_line: nil}, tests) == expected
  end

  test "parsing filters" do
    assert Filters.parse(["run"]) == [:run]
    assert Filters.parse(["run:true"]) == [run: "true"]
    assert Filters.parse(["run:test"]) == [run: "test"]
    assert Filters.parse(["line:9"]) == [line: "9"]
  end

  test "file paths with line numbers" do
    expected = {"test/some/path.exs", [exclude: [:test], include: [line: "123"]]}
    assert Filters.parse_path("test/some/path.exs:123") == expected

    assert Filters.parse_path("test/some/path.exs") == {"test/some/path.exs", []}

    expected = {"test/some/path.exs:123notreallyalinenumber123", []}
    assert Filters.parse_path("test/some/path.exs:123notreallyalinenumber123") == expected

    expected = {"C:\\some\\path.exs", [exclude: [:test], include: [line: "123"]]}
    assert Filters.parse_path("C:\\some\\path.exs:123") == expected

    assert Filters.parse_path("C:\\some\\path.exs") == {"C:\\some\\path.exs", []}

    expected = {"C:\\some\\path.exs:123notreallyalinenumber123", []}
    assert Filters.parse_path("C:\\some\\path.exs:123notreallyalinenumber123") == expected
  end
end
