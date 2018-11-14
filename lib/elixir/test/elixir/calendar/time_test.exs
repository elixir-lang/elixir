Code.require_file("../test_helper.exs", __DIR__)
Code.require_file("holocene.exs", __DIR__)
Code.require_file("fakes.exs", __DIR__)

defmodule TimeTest do
  use ExUnit.Case, async: true
  doctest Time

  test "to_string/1" do
    assert to_string(~T[23:00:07.005]) == "23:00:07.005"
  end

  test "Kernel.inspect/1" do
    assert inspect(~T[23:00:07.005]) == "~T[23:00:07.005]"
  end

  test "compare/2" do
    time0 = ~T[01:01:01.0]
    time1 = ~T[01:01:01.005]
    time2 = ~T[01:01:01.0050]
    time3 = ~T[23:01:01.0050]
    assert Time.compare(time0, time1) == :lt
    assert Time.compare(time1, time1) == :eq
    assert Time.compare(time1, time2) == :eq
    assert Time.compare(time1, time3) == :lt
    assert Time.compare(time3, time2) == :gt
  end

  test "truncate/2" do
    assert Time.truncate(~T[01:01:01.123456], :microsecond) == ~T[01:01:01.123456]

    assert Time.truncate(~T[01:01:01.0], :millisecond) == ~T[01:01:01.0]
    assert Time.truncate(~T[01:01:01.00], :millisecond) == ~T[01:01:01.00]
    assert Time.truncate(~T[01:01:01.1], :millisecond) == ~T[01:01:01.1]
    assert Time.truncate(~T[01:01:01.100], :millisecond) == ~T[01:01:01.100]
    assert Time.truncate(~T[01:01:01.999], :millisecond) == ~T[01:01:01.999]
    assert Time.truncate(~T[01:01:01.1000], :millisecond) == ~T[01:01:01.100]
    assert Time.truncate(~T[01:01:01.1001], :millisecond) == ~T[01:01:01.100]
    assert Time.truncate(~T[01:01:01.123456], :millisecond) == ~T[01:01:01.123]
    assert Time.truncate(~T[01:01:01.000123], :millisecond) == ~T[01:01:01.000]
    assert Time.truncate(~T[01:01:01.00012], :millisecond) == ~T[01:01:01.000]

    assert Time.truncate(~T[01:01:01.123456], :second) == ~T[01:01:01]
  end
end
