defmodule Logger.UtilsTest do
  use Logger.Case, async: true

  import Logger.Utils

  import Kernel, except: [inspect: 2]

  defp inspect(format, args) do
    format
    |> Logger.Utils.scan_inspect(args, 10)
    |> :io_lib.unscan_format()
  end

  describe "compute_mode/2" do
    test "starting async" do
      assert compute_mode(:async, 0, 15, 20, 7500, 10000) == :async
      assert compute_mode(:async, 10, 15, 20, 7500, 10000) == :async
      assert compute_mode(:async, 18, 15, 20, 7500, 10000) == :async
      assert compute_mode(:async, 20, 15, 20, 7500, 10000) == :sync
      assert compute_mode(:async, 30, 15, 20, 7500, 10000) == :sync
      assert compute_mode(:async, 8000, 15, 20, 7500, 10000) == :sync
      assert compute_mode(:async, 10000, 15, 20, 7500, 10000) == :discard
    end

    test "starting sync" do
      assert compute_mode(:sync, 0, 15, 20, 7500, 10000) == :async
      assert compute_mode(:sync, 10, 15, 20, 7500, 10000) == :async
      assert compute_mode(:sync, 18, 15, 20, 7500, 10000) == :sync
      assert compute_mode(:sync, 20, 15, 20, 7500, 10000) == :sync
      assert compute_mode(:sync, 30, 15, 20, 7500, 10000) == :sync
      assert compute_mode(:sync, 8000, 15, 20, 7500, 10000) == :sync
      assert compute_mode(:sync, 10000, 15, 20, 7500, 10000) == :discard
    end

    test "starting discard (with sync)" do
      assert compute_mode(:discard, 0, 15, 20, 7500, 10000) == :async
      assert compute_mode(:discard, 10, 15, 20, 7500, 10000) == :async
      assert compute_mode(:discard, 18, 15, 20, 7500, 10000) == :async
      assert compute_mode(:discard, 20, 15, 20, 7500, 10000) == :sync
      assert compute_mode(:discard, 30, 15, 20, 7500, 10000) == :sync
      assert compute_mode(:discard, 8000, 15, 20, 7500, 10000) == :discard
      assert compute_mode(:discard, 10000, 15, 20, 7500, 10000) == :discard
    end

    test "starting discard (without sync)" do
      assert compute_mode(:discard, 0, 75000, 100_000, 7500, 10000) == :async
      assert compute_mode(:discard, 10, 75000, 100_000, 7500, 10000) == :async
      assert compute_mode(:discard, 18, 75000, 100_000, 7500, 10000) == :async
      assert compute_mode(:discard, 20, 75000, 100_000, 7500, 10000) == :async
      assert compute_mode(:discard, 30, 75000, 100_000, 7500, 10000) == :async
      assert compute_mode(:discard, 8000, 75000, 100_000, 7500, 10000) == :discard
      assert compute_mode(:discard, 10000, 75000, 100_000, 7500, 10000) == :discard
    end
  end

  test "truncate/2" do
    # ASCII binaries
    assert truncate("foo", 4) == "foo"
    assert truncate("foo", 3) == "foo"
    assert truncate("foo", 2) == ["fo", " (truncated)"]

    # UTF-8 binaries
    assert truncate("olá", 2) == ["ol", " (truncated)"]
    assert truncate("olá", 3) == ["ol", " (truncated)"]
    assert truncate("olá", 4) == "olá"
    assert truncate("ááááá:", 10) == ["ááááá", " (truncated)"]
    assert truncate("áááááá:", 10) == ["ááááá", " (truncated)"]

    # Charlists
    assert truncate('olá', 2) == ['olá', " (truncated)"]
    assert truncate('olá', 3) == ['olá', " (truncated)"]
    assert truncate('olá', 4) == 'olá'

    # Chardata
    assert truncate('ol' ++ "á", 2) == ['ol' ++ "", " (truncated)"]
    assert truncate('ol' ++ "á", 3) == ['ol' ++ "", " (truncated)"]
    assert truncate('ol' ++ "á", 4) == 'ol' ++ "á"

    # :infinity
    long_string = String.duplicate("foo", 10000)
    assert truncate(long_string, :infinity) == long_string
  end

  test "inspect/2 formats" do
    assert inspect('~p', [1]) == {'~ts', [["1"]]}
    assert inspect("~p", [1]) == {'~ts', [["1"]]}
    assert inspect(:"~p", [1]) == {'~ts', [["1"]]}
  end

  test "inspect/2 sigils" do
    assert inspect('~10.10tp', [1]) == {'~ts', [["1"]]}
    assert inspect('~-10.10tp', [1]) == {'~ts', [["1"]]}

    assert inspect('~10.10lp', [1]) == {'~ts', [["1"]]}
    assert inspect('~10.10x~p~n', [1, 2, 3]) == {'~10.10x~ts~n', [1, 2, ["3"]]}
  end

  test "inspect/2 with modifier t has no effect (as it is the default)" do
    assert inspect('~tp', [1]) == {'~ts', [["1"]]}
    assert inspect('~tw', [1]) == {'~ts', [["1"]]}
  end

  test "inspect/2 with modifier l always prints lists" do
    assert inspect('~lp', ['abc']) == {'~ts', [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}
    assert inspect('~lw', ['abc']) == {'~ts', [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}
  end

  test "inspect/2 with modifier for width" do
    assert inspect('~5lp', ['abc']) ==
             {'~ts', [["[", "97", ",", "\n ", "98", ",", "\n ", "99", "]"]]}

    assert inspect('~5lw', ['abc']) == {'~ts', [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}
  end

  test "inspect/2 with modifier for limit" do
    assert inspect('~5lP', ['abc', 2]) ==
             {'~ts', [["[", "97", ",", "\n ", "98", ",", "\n ", "...", "]"]]}

    assert inspect('~5lW', ['abc', 2]) ==
             {'~ts', [["[", "97", ",", " ", "98", ",", " ", "...", "]"]]}
  end

  test "inspect/2 truncates binaries" do
    assert inspect('~ts', ["abcdeabcdeabcdeabcde"]) == {'~ts', ["abcdeabcde"]}

    assert inspect('~ts~ts~ts', ["abcdeabcde", "abcde", "abcde"]) ==
             {'~ts~ts~ts', ["abcdeabcde", "", ""]}
  end

  test "timestamp/1" do
    assert {{_, _, _}, {_, _, _, _}} = timestamp(true)
  end
end
