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
    assert truncate("𠜎𠜱𠝹𠱓", 15) == ["𠜎𠜱𠝹", " (truncated)"]

    # Charlists
    assert truncate(~c"olá", 2) == [~c"olá", " (truncated)"]
    assert truncate(~c"olá", 3) == [~c"olá", " (truncated)"]
    assert truncate(~c"olá", 4) == ~c"olá"

    # Chardata
    assert truncate(~c"ol" ++ "á", 2) == [~c"ol" ++ "", " (truncated)"]
    assert truncate(~c"ol" ++ "á", 3) == [~c"ol" ++ "", " (truncated)"]
    assert truncate(~c"ol" ++ "á", 4) == ~c"ol" ++ "á"

    # :infinity
    long_string = String.duplicate("foo", 10000)
    assert truncate(long_string, :infinity) == long_string
  end

  test "scan_inspect/3 formats" do
    assert inspect(~c"~p", [1]) == {~c"~ts", [["1"]]}
    assert inspect("~p", [1]) == {~c"~ts", [["1"]]}
    assert inspect(:"~p", [1]) == {~c"~ts", [["1"]]}
  end

  test "scan_inspect/3 sigils" do
    assert inspect(~c"~10.10tp", [1]) == {~c"~ts", [["1"]]}
    assert inspect(~c"~-10.10tp", [1]) == {~c"~ts", [["1"]]}

    assert inspect(~c"~10.10lp", [1]) == {~c"~ts", [["1"]]}
    assert inspect(~c"~10.10x~p~n", [1, 2, 3]) == {~c"~10.10x~ts~n", [1, 2, ["3"]]}
  end

  test "scan_inspect/3 with modifier t has no effect (as it is the default)" do
    assert inspect(~c"~tp", [1]) == {~c"~ts", [["1"]]}
    assert inspect(~c"~tw", [1]) == {~c"~ts", [["1"]]}
  end

  test "scan_inspect/3 with modifier l always prints lists" do
    assert inspect(~c"~lp", [~c"abc"]) ==
             {~c"~ts", [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}

    assert inspect(~c"~lw", [~c"abc"]) ==
             {~c"~ts", [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}
  end

  test "scan_inspect/3 with modifier for width" do
    assert inspect(~c"~5lp", [~c"abc"]) ==
             {~c"~ts", [["[", "97", ",", "\n ", "98", ",", "\n ", "99", "]"]]}

    assert inspect(~c"~5lw", [~c"abc"]) ==
             {~c"~ts", [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}
  end

  test "scan_inspect/3 with modifier for limit" do
    assert inspect(~c"~5lP", [~c"abc", 2]) ==
             {~c"~ts", [["[", "97", ",", "\n ", "98", ",", "\n ", "...", "]"]]}

    assert inspect(~c"~5lW", [~c"abc", 2]) ==
             {~c"~ts", [["[", "97", ",", " ", "98", ",", " ", "...", "]"]]}
  end

  test "scan_inspect/3 truncates binaries" do
    assert inspect(~c"~ts", ["abcdeabcdeabcdeabcde"]) == {~c"~ts", ["abcdeabcde"]}

    assert inspect(~c"~ts~ts~ts", ["abcdeabcde", "abcde", "abcde"]) ==
             {~c"~ts~ts~ts", ["abcdeabcde", "", ""]}
  end

  test "scan_inspect/3 with :infinity truncate" do
    long_string = String.duplicate("foo", 10000)
    assert inspect(~c"~ts", [long_string], :infinity) == {~c"~ts", [long_string]}
  end

  test "timestamp/1" do
    assert {{_, _, _}, {_, _, _, _}} = timestamp(true)
  end
end
