Code.require_file "../test_helper.exs", __DIR__
Code.require_file "../fixtures/calendar/julian.exs", __DIR__

defmodule Date.RangeTest do
  use ExUnit.Case, async: true
  doctest Date.Range

  setup do
    {:ok, range: Date.range(~D[2000-01-01], ~D[2001-01-01])}
  end

  describe "Enum.member?/2" do
    test "for ascending range", %{range: range} do
      assert Enum.member?(range, ~D[2000-02-22])
      refute Enum.member?(range, ~D[2002-01-01])
    end

    test "for descending range", %{range: range} do
      assert Enum.member?(range, ~D[2000-02-22])
      refute Enum.member?(range, ~D[1999-01-01])
    end
  end

  describe "Enum.count/1" do
    test "counts days in range", %{range: range} do
      assert Enum.count(range) == 367
    end
  end

  describe "Enum.reduce/3" do
    test "acts as a normal reduce" do
      range = Date.range(~D[2000-01-01], ~D[2000-01-03])
      fun = fn (date, acc) -> acc ++ [date] end

      assert Enum.reduce(range, [], fun) == [~D[2000-01-01], ~D[2000-01-02], ~D[2000-01-03]]
    end
  end

  test "both dates must have matching calendars" do
    first = ~D[2000-01-01]
    last = Calendar.Julian.date(2001, 01, 01)

    assert_raise ArgumentError, "both dates must have matching calendars", fn ->
      Date.range(first, last)
    end
  end

  test "accepts equal but not Calendar.ISO calendars" do
    first = Calendar.Julian.date(2001, 01, 01)
    last = Calendar.Julian.date(2000, 01, 01)

    assert Date.range(first, last)
  end
end
