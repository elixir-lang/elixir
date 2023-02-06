Code.require_file("test_helper.exs", __DIR__)

defmodule CalendarTest do
  use ExUnit.Case, async: true
  doctest Calendar

  describe "strftime/3" do
    test "returns received string if there is no datetime formatting to be found in it" do
      assert Calendar.strftime(~N[2019-08-20 15:47:34.001], "same string") == "same string"
    end

    test "formats all time zones blank when receiving a NaiveDateTime" do
      assert Calendar.strftime(~N[2019-08-15 17:07:57.001], "%z%Z") == ""
    end

    test "raises error when trying to format a date with a map that has no date fields" do
      time_without_date = %{hour: 15, minute: 47, second: 34, microsecond: {0, 0}}

      assert_raise KeyError, fn -> Calendar.strftime(time_without_date, "%x") end
    end

    test "raises error when trying to format a time with a map that has no time fields" do
      date_without_time = %{year: 2019, month: 8, day: 20}

      assert_raise KeyError, fn -> Calendar.strftime(date_without_time, "%X") end
    end

    test "raises error when the format is invalid" do
      assert_raise ArgumentError, "invalid strftime format: %-", fn ->
        Calendar.strftime(~N[2019-08-20 15:47:34.001], "%-2-ç")
      end

      assert_raise ArgumentError, "invalid strftime format: %", fn ->
        Calendar.strftime(~N[2019-08-20 15:47:34.001], "%")
      end
    end

    test "raises error when the preferred_datetime calls itself" do
      assert_raise ArgumentError, fn ->
        Calendar.strftime(~N[2019-08-20 15:47:34.001], "%c", preferred_datetime: "%c")
      end
    end

    test "raises error when the preferred_date calls itself" do
      assert_raise ArgumentError, fn ->
        Calendar.strftime(~N[2019-08-20 15:47:34.001], "%x", preferred_date: "%x")
      end
    end

    test "raises error when the preferred_time calls itself" do
      assert_raise ArgumentError, fn ->
        Calendar.strftime(~N[2019-08-20 15:47:34.001], "%X", preferred_time: "%X")
      end
    end

    test "raises error when the preferred formats creates a circular chain" do
      assert_raise ArgumentError, fn ->
        Calendar.strftime(~N[2019-08-20 15:47:34.001], "%c",
          preferred_datetime: "%x",
          preferred_date: "%X",
          preferred_time: "%c"
        )
      end
    end

    test "with preferred formats are included multiple times on the same string" do
      assert Calendar.strftime(~N[2019-08-15 17:07:57.001], "%c %c %x %x %X %X") ==
               "2019-08-15 17:07:57 2019-08-15 17:07:57 2019-08-15 2019-08-15 17:07:57 17:07:57"
    end

    test "`-` removes padding" do
      assert Calendar.strftime(~D[2019-01-01], "%-j") == "1"
      assert Calendar.strftime(~T[17:07:57.001], "%-999M") == "7"
    end

    test "formats time zones correctly when receiving a DateTime" do
      datetime_with_zone = %DateTime{
        year: 2019,
        month: 8,
        day: 15,
        zone_abbr: "EEST",
        hour: 17,
        minute: 7,
        second: 57,
        microsecond: {0, 0},
        utc_offset: 7200,
        std_offset: 3600,
        time_zone: "UK"
      }

      assert Calendar.strftime(datetime_with_zone, "%z %Z") == "+0300 EEST"
    end

    test "formats AM and PM correctly on the %P and %p options" do
      am_time_almost_pm = ~U[2019-08-26 11:59:59.001Z]
      pm_time = ~U[2019-08-26 12:00:57.001Z]
      pm_time_almost_am = ~U[2019-08-26 23:59:57.001Z]
      am_time = ~U[2019-08-26 00:00:01.001Z]

      assert Calendar.strftime(am_time_almost_pm, "%P %p") == "am AM"
      assert Calendar.strftime(pm_time, "%P %p") == "pm PM"
      assert Calendar.strftime(pm_time_almost_am, "%P %p") == "pm PM"
      assert Calendar.strftime(am_time, "%P %p") == "am AM"
    end

    test "formats all weekdays correctly with %A and %a formats" do
      sunday = ~U[2019-08-25 11:59:59.001Z]
      monday = ~U[2019-08-26 11:59:59.001Z]
      tuesday = ~U[2019-08-27 11:59:59.001Z]
      wednesday = ~U[2019-08-28 11:59:59.001Z]
      thursday = ~U[2019-08-29 11:59:59.001Z]
      friday = ~U[2019-08-30 11:59:59.001Z]
      saturday = ~U[2019-08-31 11:59:59.001Z]

      assert Calendar.strftime(sunday, "%A %a") == "Sunday Sun"
      assert Calendar.strftime(monday, "%A %a") == "Monday Mon"
      assert Calendar.strftime(tuesday, "%A %a") == "Tuesday Tue"
      assert Calendar.strftime(wednesday, "%A %a") == "Wednesday Wed"
      assert Calendar.strftime(thursday, "%A %a") == "Thursday Thu"
      assert Calendar.strftime(friday, "%A %a") == "Friday Fri"
      assert Calendar.strftime(saturday, "%A %a") == "Saturday Sat"
    end

    test "formats all months correctly with the %B and %b formats" do
      assert Calendar.strftime(%{month: 1}, "%B %b") == "January Jan"
      assert Calendar.strftime(%{month: 2}, "%B %b") == "February Feb"
      assert Calendar.strftime(%{month: 3}, "%B %b") == "March Mar"
      assert Calendar.strftime(%{month: 4}, "%B %b") == "April Apr"
      assert Calendar.strftime(%{month: 5}, "%B %b") == "May May"
      assert Calendar.strftime(%{month: 6}, "%B %b") == "June Jun"
      assert Calendar.strftime(%{month: 7}, "%B %b") == "July Jul"
      assert Calendar.strftime(%{month: 8}, "%B %b") == "August Aug"
      assert Calendar.strftime(%{month: 9}, "%B %b") == "September Sep"
      assert Calendar.strftime(%{month: 10}, "%B %b") == "October Oct"
      assert Calendar.strftime(%{month: 11}, "%B %b") == "November Nov"
      assert Calendar.strftime(%{month: 12}, "%B %b") == "December Dec"
    end

    test "formats all weekdays correctly on %A with day_of_week_names option" do
      sunday = ~U[2019-08-25 11:59:59.001Z]
      monday = ~U[2019-08-26 11:59:59.001Z]
      tuesday = ~U[2019-08-27 11:59:59.001Z]
      wednesday = ~U[2019-08-28 11:59:59.001Z]
      thursday = ~U[2019-08-29 11:59:59.001Z]
      friday = ~U[2019-08-30 11:59:59.001Z]
      saturday = ~U[2019-08-31 11:59:59.001Z]

      day_of_week_names = fn day_of_week ->
        {"segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado",
         "domingo"}
        |> elem(day_of_week - 1)
      end

      assert Calendar.strftime(sunday, "%A", day_of_week_names: day_of_week_names) ==
               "domingo"

      assert Calendar.strftime(monday, "%A", day_of_week_names: day_of_week_names) ==
               "segunda-feira"

      assert Calendar.strftime(tuesday, "%A", day_of_week_names: day_of_week_names) ==
               "terça-feira"

      assert Calendar.strftime(wednesday, "%A", day_of_week_names: day_of_week_names) ==
               "quarta-feira"

      assert Calendar.strftime(thursday, "%A", day_of_week_names: day_of_week_names) ==
               "quinta-feira"

      assert Calendar.strftime(friday, "%A", day_of_week_names: day_of_week_names) ==
               "sexta-feira"

      assert Calendar.strftime(saturday, "%A", day_of_week_names: day_of_week_names) ==
               "sábado"
    end

    test "formats all months correctly on the %B with month_names option" do
      month_names = fn month ->
        {"январь", "февраль", "март", "апрель", "май", "июнь", "июль", "август", "сентябрь",
         "октябрь", "ноябрь", "декабрь"}
        |> elem(month - 1)
      end

      assert Calendar.strftime(%{month: 1}, "%B", month_names: month_names) == "январь"
      assert Calendar.strftime(%{month: 2}, "%B", month_names: month_names) == "февраль"
      assert Calendar.strftime(%{month: 3}, "%B", month_names: month_names) == "март"
      assert Calendar.strftime(%{month: 4}, "%B", month_names: month_names) == "апрель"
      assert Calendar.strftime(%{month: 5}, "%B", month_names: month_names) == "май"
      assert Calendar.strftime(%{month: 6}, "%B", month_names: month_names) == "июнь"
      assert Calendar.strftime(%{month: 7}, "%B", month_names: month_names) == "июль"
      assert Calendar.strftime(%{month: 8}, "%B", month_names: month_names) == "август"
      assert Calendar.strftime(%{month: 9}, "%B", month_names: month_names) == "сентябрь"
      assert Calendar.strftime(%{month: 10}, "%B", month_names: month_names) == "октябрь"
      assert Calendar.strftime(%{month: 11}, "%B", month_names: month_names) == "ноябрь"
      assert Calendar.strftime(%{month: 12}, "%B", month_names: month_names) == "декабрь"
    end

    test "formats all weekdays correctly on the %a format with abbreviated_day_of_week_names option" do
      sunday = ~U[2019-08-25 11:59:59.001Z]
      monday = ~U[2019-08-26 11:59:59.001Z]
      tuesday = ~U[2019-08-27 11:59:59.001Z]
      wednesday = ~U[2019-08-28 11:59:59.001Z]
      thursday = ~U[2019-08-29 11:59:59.001Z]
      friday = ~U[2019-08-30 11:59:59.001Z]
      saturday = ~U[2019-08-31 11:59:59.001Z]

      abbreviated_day_of_week_names = fn day_of_week ->
        {"seg", "ter", "qua", "qui", "sex", "sáb", "dom"}
        |> elem(day_of_week - 1)
      end

      assert Calendar.strftime(sunday, "%a",
               abbreviated_day_of_week_names: abbreviated_day_of_week_names
             ) == "dom"

      assert Calendar.strftime(monday, "%a",
               abbreviated_day_of_week_names: abbreviated_day_of_week_names
             ) == "seg"

      assert Calendar.strftime(tuesday, "%a",
               abbreviated_day_of_week_names: abbreviated_day_of_week_names
             ) == "ter"

      assert Calendar.strftime(wednesday, "%a",
               abbreviated_day_of_week_names: abbreviated_day_of_week_names
             ) == "qua"

      assert Calendar.strftime(thursday, "%a",
               abbreviated_day_of_week_names: abbreviated_day_of_week_names
             ) == "qui"

      assert Calendar.strftime(friday, "%a",
               abbreviated_day_of_week_names: abbreviated_day_of_week_names
             ) == "sex"

      assert Calendar.strftime(saturday, "%a",
               abbreviated_day_of_week_names: abbreviated_day_of_week_names
             ) == "sáb"
    end

    test "formats all months correctly on the %b format with abbreviated_month_names option" do
      abbreviated_month_names = fn month ->
        {"янв", "февр", "март", "апр", "май", "июнь", "июль", "авг", "сент", "окт", "нояб", "дек"}
        |> elem(month - 1)
      end

      assert Calendar.strftime(%{month: 1}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) ==
               "янв"

      assert Calendar.strftime(%{month: 2}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) ==
               "февр"

      assert Calendar.strftime(%{month: 3}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) ==
               "март"

      assert Calendar.strftime(%{month: 4}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) ==
               "апр"

      assert Calendar.strftime(%{month: 5}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) ==
               "май"

      assert Calendar.strftime(%{month: 6}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) ==
               "июнь"

      assert Calendar.strftime(%{month: 7}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) ==
               "июль"

      assert Calendar.strftime(%{month: 8}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) ==
               "авг"

      assert Calendar.strftime(%{month: 9}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) ==
               "сент"

      assert Calendar.strftime(%{month: 10}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) == "окт"

      assert Calendar.strftime(%{month: 11}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) == "нояб"

      assert Calendar.strftime(%{month: 12}, "%b",
               abbreviated_month_names: abbreviated_month_names
             ) == "дек"
    end

    test "formats ignores padding and width options on microseconds" do
      datetime = ~U[2019-08-15 17:07:57.001234Z]
      assert Calendar.strftime(datetime, "%f") == "001234"
      assert Calendar.strftime(datetime, "%f") == Calendar.strftime(datetime, "%_20f")
      assert Calendar.strftime(datetime, "%f") == Calendar.strftime(datetime, "%020f")
      assert Calendar.strftime(datetime, "%f") == Calendar.strftime(datetime, "%-f")
    end

    test "formats properly dates with different microsecond precisions" do
      assert Calendar.strftime(~U[2019-08-15 17:07:57.5Z], "%f") == "5"
      assert Calendar.strftime(~U[2019-08-15 17:07:57.45Z], "%f") == "45"
      assert Calendar.strftime(~U[2019-08-15 17:07:57.345Z], "%f") == "345"
      assert Calendar.strftime(~U[2019-08-15 17:07:57.2345Z], "%f") == "2345"
      assert Calendar.strftime(~U[2019-08-15 17:07:57.12345Z], "%f") == "12345"
      assert Calendar.strftime(~U[2019-08-15 17:07:57.012345Z], "%f") == "012345"
    end

    test "formats properly different microsecond precisions of zero" do
      assert Calendar.strftime(~N[2019-08-15 17:07:57.0], "%f") == "0"
      assert Calendar.strftime(~N[2019-08-15 17:07:57.00], "%f") == "00"
      assert Calendar.strftime(~N[2019-08-15 17:07:57.000], "%f") == "000"
      assert Calendar.strftime(~N[2019-08-15 17:07:57.0000], "%f") == "0000"
      assert Calendar.strftime(~N[2019-08-15 17:07:57.00000], "%f") == "00000"
      assert Calendar.strftime(~N[2019-08-15 17:07:57.000000], "%f") == "000000"
    end

    test "returns a single zero if there's no microseconds precision" do
      assert Calendar.strftime(~N[2019-08-15 17:07:57], "%f") == "0"
    end

    test "handles `0` both as padding and as part of a width" do
      assert Calendar.strftime(~N[2019-08-15 17:07:57], "%10A") == "  Thursday"
      assert Calendar.strftime(~N[2019-08-15 17:07:57], "%010A") == "00Thursday"
    end

    test "formats Epoch time with %s" do
      assert Calendar.strftime(~N[2019-08-15 17:07:57], "%s") == "1565888877"

      datetime = %DateTime{
        year: 2019,
        month: 8,
        day: 15,
        hour: 17,
        minute: 7,
        second: 57,
        microsecond: {0, 0},
        time_zone: "Europe/Berlin",
        zone_abbr: "CET",
        utc_offset: 3600,
        std_offset: 0
      }

      assert Calendar.strftime(datetime, "%s") == "1565885277"
    end

    test "formats datetime with all options and modifiers" do
      assert Calendar.strftime(
               ~U[2019-08-15 17:07:57.001Z],
               "%04% %a %A %b %B %-3c %d %f %H %I %j %m %_5M %p %P %q %S %u %x %X %y %Y %z %Z"
             ) ==
               "000% Thu Thursday Aug August 2019-08-15 17:07:57 15 001 17 05 227 08     7 PM pm 3 57 4 2019-08-15 17:07:57 19 2019 +0000 UTC"
    end

    test "formats according to custom configs" do
      assert Calendar.strftime(
               ~U[2019-08-15 17:07:57.001Z],
               "%A %a %p %B %b %c %x %X",
               am_pm_names: fn
                 :am -> "a"
                 :pm -> "p"
               end,
               month_names: fn month ->
                 {"Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto",
                  "Setembro", "Outubro", "Novembro", "Dezembro"}
                 |> elem(month - 1)
               end,
               day_of_week_names: fn day_of_week ->
                 {"понедельник", "вторник", "среда", "четверг", "пятница", "суббота",
                  "воскресенье"}
                 |> elem(day_of_week - 1)
               end,
               abbreviated_month_names: fn month ->
                 {"Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov",
                  "Dez"}
                 |> elem(month - 1)
               end,
               abbreviated_day_of_week_names: fn day_of_week ->
                 {"ПНД", "ВТР", "СРД", "ЧТВ", "ПТН", "СБТ", "ВСК"}
                 |> elem(day_of_week - 1)
               end,
               preferred_date: "%05Y-%m-%d",
               preferred_time: "%M:%_3H%S",
               preferred_datetime: "%%"
             ) == "четверг ЧТВ P Agosto Ago % 02019-08-15 07: 1757"
    end

    test "raises on unknown option according to custom configs" do
      assert_raise ArgumentError, "unknown option :unknown given to Calendar.strftime/3", fn ->
        Calendar.strftime(~D[2019-08-15], "%D", unknown: "option")
      end
    end

    test "zero padding for negative year" do
      assert Calendar.strftime(Date.new!(-1, 1, 1), "%Y") == "-0001"
      assert Calendar.strftime(Date.new!(-11, 1, 1), "%Y") == "-0011"
      assert Calendar.strftime(Date.new!(-111, 1, 1), "%Y") == "-0111"
      assert Calendar.strftime(Date.new!(-1111, 1, 1), "%Y") == "-1111"
    end
  end
end
