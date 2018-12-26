defmodule Logger.UtilsTest do
  use Logger.Case, async: true

  import Logger.Utils

  import Kernel, except: [inspect: 2]

  defp inspect(format, args, truncate \\ 10) do
    format
    |> Logger.Utils.scan_inspect(args, truncate)
    |> :io_lib.unscan_format()
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

  test "scan_inspect/3 formats" do
    assert inspect('~p', [1]) == {'~ts', [["1"]]}
    assert inspect("~p", [1]) == {'~ts', [["1"]]}
    assert inspect(:"~p", [1]) == {'~ts', [["1"]]}
  end

  test "scan_inspect/3 sigils" do
    assert inspect('~10.10tp', [1]) == {'~ts', [["1"]]}
    assert inspect('~-10.10tp', [1]) == {'~ts', [["1"]]}

    assert inspect('~10.10lp', [1]) == {'~ts', [["1"]]}
    assert inspect('~10.10x~p~n', [1, 2, 3]) == {'~10.10x~ts~n', [1, 2, ["3"]]}
  end

  test "scan_inspect/3 with modifier t has no effect (as it is the default)" do
    assert inspect('~tp', [1]) == {'~ts', [["1"]]}
    assert inspect('~tw', [1]) == {'~ts', [["1"]]}
  end

  test "scan_inspect/3 with modifier l always prints lists" do
    assert inspect('~lp', ['abc']) == {'~ts', [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}
    assert inspect('~lw', ['abc']) == {'~ts', [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}
  end

  test "scan_inspect/3 with modifier for width" do
    assert inspect('~5lp', ['abc']) ==
             {'~ts', [["[", "97", ",", "\n ", "98", ",", "\n ", "99", "]"]]}

    assert inspect('~5lw', ['abc']) == {'~ts', [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}
  end

  test "scan_inspect/3 with modifier for limit" do
    assert inspect('~5lP', ['abc', 2]) ==
             {'~ts', [["[", "97", ",", "\n ", "98", ",", "\n ", "...", "]"]]}

    assert inspect('~5lW', ['abc', 2]) ==
             {'~ts', [["[", "97", ",", " ", "98", ",", " ", "...", "]"]]}
  end

  test "scan_inspect/3 truncates binaries" do
    assert inspect('~ts', ["abcdeabcdeabcdeabcde"]) == {'~ts', ["abcdeabcde"]}

    assert inspect('~ts~ts~ts', ["abcdeabcde", "abcde", "abcde"]) ==
             {'~ts~ts~ts', ["abcdeabcde", "", ""]}
  end

  test "scan_inspect/3 with :infinity truncate" do
    long_string = String.duplicate("foo", 10000)
    assert inspect('~ts', [long_string], :infinity) == {'~ts', [long_string]}
  end

  test "timestamp/1" do
    assert {{_, _, _}, {_, _, _, _}} = timestamp(true)
  end
end
