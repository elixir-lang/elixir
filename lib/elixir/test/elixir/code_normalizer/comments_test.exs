Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Normalizer.IntegrationTest do
  use ExUnit.Case, async: true

  import CodeNormalizerHelpers

  describe "preserves comments formatting" do
    test "before and after expressions" do
      assert_same """
      # before comment
      :hello
      """

      assert_same """
      :hello
      # after comment
      """

      assert_same """
      # before comment
      :hello
      # after comment
      """
    end

    test "empty comment" do
      assert_same """
      #
      :foo
      """
    end

    test "before and after expressions with newlines" do
      assert_same """
      # before comment
      # second line

      :hello

      # middle comment 1

      #

      # middle comment 2

      :world

      # after comment
      # second line
      """
    end

    test "interpolation with comment outside before and after" do
      assert_same ~S"""
      # comment
      IO.puts("Hello #{world}")
      """

      assert_same ~S"""
      IO.puts("Hello #{world}")
      # comment
      """
    end
  end
end
