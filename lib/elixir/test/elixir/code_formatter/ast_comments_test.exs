Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Formatter.AstCommentsTest do
  use ExUnit.Case, async: true

  @string ~S"""
      %Foo{
        bar
        | # 1. one

          # 1. two
          # 1. three
          one: one,
          one: after_one,
          one:
            after_one do
              :ok
            end,

          # 2. one

          # 2. two
          # 2. three
          # two,

          # 3. one

          # 3. two
          # 3. three
          # final
          three: three

          # 4. one

          # 4. two
          # 4. three
          # four
      }
  """

  test "wip" do
    Code.format_string!(@string)
    |> IO.puts()
  end

  test "test" do
    Code.string_to_quoted!(@string, include_comments: true, unescape: false)
    |> IO.inspect()
  end
end
