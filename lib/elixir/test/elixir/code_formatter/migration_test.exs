Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Formatter.MigrationTest do
  use ExUnit.Case, async: true

  import CodeFormatterHelpers

  @rewrite_unless [rewrite_unless: true]

  describe "rewrite_unless: true" do
    test "rewrites unless as an if with negated condition" do
      bad = "unless x, do: y"

      good = "if !x, do: y"

      assert_format bad, good, @rewrite_unless

      bad = """
      unless x do
        y
      else
        z
      end
      """

      good = """
      if !x do
        y
      else
        z
      end
      """

      assert_format bad, good, @rewrite_unless
    end

    test "rewrites pipelines with negated condition" do
      bad = "x |> unless(do: y)"

      good = "!x |> if(do: y)"

      assert_format bad, good, @rewrite_unless

      bad = "x |> foo() |> unless(do: y)"

      good = "x |> foo() |> Kernel.!() |> if(do: y)"

      assert_format bad, good, @rewrite_unless
    end

    test "rewrites in as not in" do
      assert_format "unless x in y, do: 1", "if x not in y, do: 1", @rewrite_unless
    end

    test "rewrites equality operators" do
      assert_format "unless x == y, do: 1", "if x != y, do: 1", @rewrite_unless
      assert_format "unless x === y, do: 1", "if x !== y, do: 1", @rewrite_unless
      assert_format "unless x != y, do: 1", "if x == y, do: 1", @rewrite_unless
      assert_format "unless x !== y, do: 1", "if x === y, do: 1", @rewrite_unless
    end

    test "rewrites boolean or is_* conditions with not" do
      assert_format "unless x > 0, do: 1", "if not (x > 0), do: 1", @rewrite_unless
      assert_format "unless is_atom(x), do: 1", "if not is_atom(x), do: 1", @rewrite_unless
    end

    test "removes ! or not in condition" do
      assert_format "unless not x, do: 1", "if x, do: 1", @rewrite_unless
      assert_format "unless !x, do: 1", "if x, do: 1", @rewrite_unless
    end

    test "does nothing without the rewrite_unless option" do
      assert_same "unless x, do: y"
      assert_same "unless x, do: y, else: z"
    end
  end
end
