Code.require_file("../test_helper.exs", __DIR__)
Code.require_file("holocene.exs", __DIR__)

defmodule Date.RangeTest do
  use ExUnit.Case, async: true

  @asc_range Date.range(~D[2000-01-01], ~D[2001-01-01])
  @desc_range Date.range(~D[2001-01-01], ~D[2000-01-01])

  describe "Enum.member?/2" do
    test "for ascending range" do
      assert Enum.member?(@asc_range, ~D[2000-02-22])
      assert Enum.member?(@asc_range, ~D[2000-01-01])
      assert Enum.member?(@asc_range, ~D[2001-01-01])
      refute Enum.member?(@asc_range, ~D[2002-01-01])
      refute Enum.member?(@asc_range, Calendar.Holocene.date(12000, 1, 1))
    end

    test "for descending range" do
      assert Enum.member?(@desc_range, ~D[2000-02-22])
      assert Enum.member?(@desc_range, ~D[2000-01-01])
      assert Enum.member?(@desc_range, ~D[2001-01-01])
      refute Enum.member?(@desc_range, ~D[1999-01-01])
      refute Enum.member?(@asc_range, Calendar.Holocene.date(12000, 1, 1))
    end
  end

  describe "Enum.count/1" do
    test "for ascending range" do
      assert Enum.count(@asc_range) == 367
    end

    test "for descending range" do
      assert Enum.count(@desc_range) == 367
    end
  end

  describe "Enum.slice/3" do
    test "for ascending range" do
      assert Enum.slice(@asc_range, 3, 3) == [~D[2000-01-04], ~D[2000-01-05], ~D[2000-01-06]]
      assert Enum.slice(@asc_range, -3, 3) == [~D[2000-12-30], ~D[2000-12-31], ~D[2001-01-01]]
    end

    test "for descending range" do
      assert Enum.slice(@desc_range, 3, 3) == [~D[2000-12-29], ~D[2000-12-28], ~D[2000-12-27]]
      assert Enum.slice(@desc_range, -3, 3) == [~D[2000-01-03], ~D[2000-01-02], ~D[2000-01-01]]
    end
  end

  describe "Enum.reduce/3" do
    test "for ascending range" do
      range = Date.range(~D[2000-01-01], ~D[2000-01-03])
      assert Enum.to_list(range) == [~D[2000-01-01], ~D[2000-01-02], ~D[2000-01-03]]
    end

    test "for descending range" do
      range = Date.range(~D[2000-01-03], ~D[2000-01-01])
      assert Enum.to_list(range) == [~D[2000-01-03], ~D[2000-01-02], ~D[2000-01-01]]
    end
  end

  test "both dates must have matching calendars" do
    first = ~D[2000-01-01]
    last = Calendar.Holocene.date(12001, 1, 1)

    assert_raise ArgumentError, "both dates must have matching calendars", fn ->
      Date.range(first, last)
    end
  end

  test "accepts equal but non Calendar.ISO calendars" do
    first = Calendar.Holocene.date(12000, 1, 1)
    last = Calendar.Holocene.date(12001, 1, 1)
    range = Date.range(first, last)
    assert range
    assert first in range
    assert last in range
    assert Enum.count(range) == 367
  end
end
