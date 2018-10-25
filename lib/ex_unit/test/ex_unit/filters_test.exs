Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.FiltersTest do
  use ExUnit.Case, async: true

  doctest ExUnit.Filters

  describe "filter precedence" do
    setup do
      [
        one: %{
          one: true,
          numeric: true,
          test: :"test one",
          test_type: :test
        },
        two: %{
          skip: true,
          numeric: true,
          test: :"test two",
          test_type: :test,
          two: true
        }
      ]
    end

    test "no filters", tags do
      assert ExUnit.Filters.eval([], [], tags.one, []) == :ok
      assert ExUnit.Filters.eval([], [], tags.two, []) == {:skipped, "due to skip tag"}
    end

    test "exclude tests", tags do
      assert ExUnit.Filters.eval([], [:two], tags.one, []) == :ok
      assert ExUnit.Filters.eval([], [:test], tags.one, []) == {:excluded, "due to test filter"}
      assert ExUnit.Filters.eval([], [:test], tags.two, []) == {:excluded, "due to test filter"}
      assert ExUnit.Filters.eval([], [:numeric], tags.one, []) == {:excluded, "due to numeric filter"}
      assert ExUnit.Filters.eval([], [:unknown], tags.one, []) == :ok
      assert ExUnit.Filters.eval([], [:unknown], tags.two, []) == {:skipped, "due to skip tag"}
    end

    test "explicitly exclude a test with the `:skip` tag", tags do
      # --exclude two
      assert ExUnit.Filters.eval([], [:two], tags.two, []) == {:excluded, "due to two filter"}

      assert ExUnit.Filters.eval([], [:two, :test], tags.two, []) ==
               {:excluded, "due to two filter"}
    end

    test "include tests", tags do
      # use --include, exclude is missing
      assert ExUnit.Filters.eval([:two], [], tags.one, []) == :ok
      assert ExUnit.Filters.eval([:unknown], [], tags.one, []) == :ok

      # --only
      assert ExUnit.Filters.eval([:one], [:test], tags.one, []) == :ok
      assert ExUnit.Filters.eval([:two], [:test], tags.one, []) ==
               {:excluded, "due to test filter"}
      assert ExUnit.Filters.eval([:unknown], [:test], tags.one, []) ==
               {:excluded, "due to test filter"}
      assert ExUnit.Filters.eval([:unknown], [:test], tags.two, []) ==
               {:excluded, "due to test filter"}
    end

    test "explicitly include a test with the `:skip` tag", tags do
      # --include two, exclude is missing
      assert ExUnit.Filters.eval([:two], [], tags.two, []) == {:skipped, "due to skip tag"}

      # --only two
      assert ExUnit.Filters.eval([:two], [:test], tags.two, []) == {:skipped, "due to skip tag"}
    end

    test "exception to the rule: include tests with the `:skip` tag", tags do
      # --include skip, exclude is missing
      assert ExUnit.Filters.eval([:skip], [], tags.two, []) == :ok
      assert ExUnit.Filters.eval([:skip], [], %{tags.two | skip: "skip me please"}, []) == :ok
      assert ExUnit.Filters.eval([skip: ~r/alpha/], [], %{tags.two | skip: "alpha"}, []) == :ok

      assert ExUnit.Filters.eval([skip: ~r/alpha/], [], %{tags.two | skip: "beta"}, []) ==
               {:skipped, "beta"}

      # --only skip
      assert ExUnit.Filters.eval([:skip], [:test], tags.two, []) == :ok

      assert ExUnit.Filters.eval([:skip], [:test], %{tags.two | skip: "skip me please"}, []) ==
               :ok

      assert ExUnit.Filters.eval([skip: ~r/alpha/], [:test], %{tags.two | skip: "alpha"}, []) ==
               :ok

      assert ExUnit.Filters.eval([skip: ~r/alpha/], [:test], %{tags.two | skip: "beta"}, []) ==
               {:excluded, "due to test filter"}
    end
  end

  test "evaluating filters" do
    assert ExUnit.Filters.eval([], [:os], %{}, []) == :ok
    assert ExUnit.Filters.eval([], [os: :win], %{os: :unix}, []) == :ok
    assert ExUnit.Filters.eval([], [:os], %{os: :unix}, []) == {:excluded, "due to os filter"}

    assert ExUnit.Filters.eval([], [os: :unix], %{os: :unix}, []) ==
             {:excluded, "due to os filter"}

    assert ExUnit.Filters.eval([os: :win], [], %{}, []) == :ok
    assert ExUnit.Filters.eval([os: :win], [], %{os: :unix}, []) == :ok
    assert ExUnit.Filters.eval([os: :win], [:os], %{}, []) == :ok
    assert ExUnit.Filters.eval([os: :win], [:os], %{os: :win}, []) == :ok

    assert ExUnit.Filters.eval([os: :win, os: :unix], [:os], %{os: :win}, []) == :ok
  end

  describe "evaluating filters with skip" do
    test "regular usage" do
      assert ExUnit.Filters.eval([], [], %{}, []) == :ok
      assert ExUnit.Filters.eval([], [], %{skip: true}, []) == {:skipped, "due to skip tag"}
      assert ExUnit.Filters.eval([], [], %{skip: "skipped"}, []) == {:skipped, "skipped"}
      assert ExUnit.Filters.eval([], [:os], %{skip: "skipped"}, []) == {:skipped, "skipped"}
    end

    test "exception to the rule: explicitly including tests with the `:skip` tag" do
      assert ExUnit.Filters.eval([:skip], [], %{skip: true}, []) == :ok
      assert ExUnit.Filters.eval([:skip], [], %{skip: "skipped"}, []) == :ok
      assert ExUnit.Filters.eval([:skip], [:test], %{skip: true}, []) == :ok
      assert ExUnit.Filters.eval([:skip], [:test], %{skip: "skipped"}, []) == :ok
      assert ExUnit.Filters.eval([skip: true], [:test], %{skip: true}, []) == :ok
      assert ExUnit.Filters.eval([skip: ~r/skip me/], [:test], %{skip: "skip me please"}, []) == :ok

      assert ExUnit.Filters.eval([skip: ~r/skip me/], [:test], %{skip: "don't skip!"}, []) ==
               {:skipped, "don't skip!"}
    end
  end

  test "evaluating filters matches integers" do
    assert ExUnit.Filters.eval([int: "1"], [], %{int: 1}, []) == :ok
    assert ExUnit.Filters.eval([int: "1"], [int: 5], %{int: 1}, []) == :ok
    assert ExUnit.Filters.eval([int: "1"], [:int], %{int: 1}, []) == :ok
  end

  test "evaluating filter matches atoms" do
    assert ExUnit.Filters.eval([os: "win"], [], %{os: :win}, []) == :ok
    assert ExUnit.Filters.eval([os: "win"], [os: :unix], %{os: :win}, []) == :ok
    assert ExUnit.Filters.eval([os: "win"], [:os], %{os: :win}, []) == :ok
    assert ExUnit.Filters.eval([module: "Foo"], [:os], %{module: Foo}, []) == :ok
  end

  test "evaluating filter matches regexes" do
    assert ExUnit.Filters.eval([os: ~r"win"], [], %{os: :win}, []) == :ok

    assert ExUnit.Filters.eval([os: ~r"mac"], [os: :unix], %{os: :unix}, []) ==
             {:excluded, "due to os filter"}
  end

  test "evaluating filter uses special rules for line" do
    tests = [
      %ExUnit.Test{tags: %{line: 3, describe_line: 2}},
      %ExUnit.Test{tags: %{line: 5, describe_line: nil}},
      %ExUnit.Test{tags: %{line: 8, describe_line: 7}},
      %ExUnit.Test{tags: %{line: 10, describe_line: 7}},
      %ExUnit.Test{tags: %{line: 13, describe_line: 12}}
    ]

    assert ExUnit.Filters.eval([line: "3"], [:line], %{line: 3, describe_line: 2}, tests) == :ok
    assert ExUnit.Filters.eval([line: "4"], [:line], %{line: 3, describe_line: 2}, tests) == :ok
    assert ExUnit.Filters.eval([line: "5"], [:line], %{line: 5, describe_line: nil}, tests) == :ok
    assert ExUnit.Filters.eval([line: "6"], [:line], %{line: 5, describe_line: nil}, tests) == :ok
    assert ExUnit.Filters.eval([line: "2"], [:line], %{line: 3, describe_line: 2}, tests) == :ok
    assert ExUnit.Filters.eval([line: "7"], [:line], %{line: 8, describe_line: 7}, tests) == :ok
    assert ExUnit.Filters.eval([line: "7"], [:line], %{line: 10, describe_line: 7}, tests) == :ok

    assert ExUnit.Filters.eval([line: "1"], [:line], %{line: 3, describe_line: 2}, tests) ==
             {:excluded, "due to line filter"}

    assert ExUnit.Filters.eval([line: "7"], [:line], %{line: 3, describe_line: 2}, tests) ==
             {:excluded, "due to line filter"}

    assert ExUnit.Filters.eval([line: "7"], [:line], %{line: 5, describe_line: nil}, tests) ==
             {:excluded, "due to line filter"}
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

    assert ExUnit.Filters.parse_path("test/some/path.exs") == {"test/some/path.exs", []}

    assert ExUnit.Filters.parse_path("test/some/path.exs:123notreallyalinenumber123") ==
             {"test/some/path.exs:123notreallyalinenumber123", []}

    assert ExUnit.Filters.parse_path("C:\\some\\path.exs:123") ==
             {"C:\\some\\path.exs", [exclude: [:test], include: [line: "123"]]}

    assert ExUnit.Filters.parse_path("C:\\some\\path.exs") == {"C:\\some\\path.exs", []}

    assert ExUnit.Filters.parse_path("C:\\some\\path.exs:123notreallyalinenumber123") ==
             {"C:\\some\\path.exs:123notreallyalinenumber123", []}
  end
end
