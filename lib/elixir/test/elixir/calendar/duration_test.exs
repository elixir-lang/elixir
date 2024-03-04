Code.require_file("../test_helper.exs", __DIR__)

defmodule Calendar.DurationTest do
  use ExUnit.Case, async: true
  doctest Calendar.Duration

  test "new!/1" do
    # TODO
    assert true
  end

  test "sigil_P" do
    assert ~P[3Y6M4DT12H30M5S] == %Calendar.Duration{
             year: 3,
             month: 6,
             week: 0,
             day: 4,
             hour: 12,
             minute: 30,
             second: 5,
             microsecond: 0
           }

    assert ~P[T1H30M] == %Calendar.Duration{
             year: 0,
             month: 0,
             week: 0,
             day: 0,
             hour: 1,
             minute: 30,
             second: 0,
             microsecond: 0
           }

    assert ~P[1DT2H] == %Calendar.Duration{
             year: 0,
             month: 0,
             week: 0,
             day: 1,
             hour: 2,
             minute: 0,
             second: 0,
             microsecond: 0
           }

    assert ~P[5Y2M3DT12H] == %Calendar.Duration{
             year: 5,
             month: 2,
             week: 0,
             day: 3,
             hour: 12,
             minute: 0,
             second: 0,
             microsecond: 0
           }

    assert ~P[10W3DT5H] == %Calendar.Duration{
             year: 0,
             month: 0,
             week: 10,
             day: 3,
             hour: 5,
             minute: 0,
             second: 0,
             microsecond: 0
           }

    assert ~P[T10M] == %Calendar.Duration{
             year: 0,
             month: 0,
             week: 0,
             day: 0,
             hour: 0,
             minute: 10,
             second: 0,
             microsecond: 0
           }

    assert ~P[1Y] == %Calendar.Duration{
             year: 1,
             month: 0,
             week: 0,
             day: 0,
             hour: 0,
             minute: 0,
             second: 0,
             microsecond: 0
           }

    assert ~P[3W] == %Calendar.Duration{
             year: 0,
             month: 0,
             week: 3,
             day: 0,
             hour: 0,
             minute: 0,
             second: 0,
             microsecond: 0
           }

    assert ~P[1DT12H] == %Calendar.Duration{
             year: 0,
             month: 0,
             week: 0,
             day: 1,
             hour: 12,
             minute: 0,
             second: 0,
             microsecond: 0
           }

    assert ~P[T2S] == %Calendar.Duration{
             year: 0,
             month: 0,
             week: 0,
             day: 0,
             hour: 0,
             minute: 0,
             second: 2,
             microsecond: 0
           }

    assert ~P[-3Y-6M-4DT-12H-30M-5S] == %Calendar.Duration{
             year: -3,
             month: -6,
             week: 0,
             day: -4,
             hour: -12,
             minute: -30,
             second: -5,
             microsecond: 0
           }
  end
end
