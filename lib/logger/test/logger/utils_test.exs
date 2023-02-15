defmodule Logger.UtilsTest do
  use Logger.Case, async: true

  doctest Logger.Utils

  defp scan_inspect(format, args, truncate \\ 10) do
    format
    |> Logger.Utils.scan_inspect(args, truncate)
    |> :io_lib.unscan_format()
  end

  test "formats" do
    assert scan_inspect(~c"~p", [1]) == {~c"~ts", [["1"]]}
    assert scan_inspect("~p", [1]) == {~c"~ts", [["1"]]}
    assert scan_inspect(:"~p", [1]) == {~c"~ts", [["1"]]}
  end

  test "sigils" do
    assert scan_inspect(~c"~10.10tp", [1]) == {~c"~ts", [["1"]]}
    assert scan_inspect(~c"~-10.10tp", [1]) == {~c"~ts", [["1"]]}

    assert scan_inspect(~c"~10.10lp", [1]) == {~c"~ts", [["1"]]}
    assert scan_inspect(~c"~10.10x~p~n", [1, 2, 3]) == {~c"~10.10x~ts~n", [1, 2, ["3"]]}
  end

  test "with modifier t has no effect (as it is the default)" do
    assert scan_inspect(~c"~tp", [1]) == {~c"~ts", [["1"]]}
    assert scan_inspect(~c"~tw", [1]) == {~c"~ts", [["1"]]}
  end

  test "with modifier l always prints lists" do
    assert scan_inspect(~c"~lp", [~c"abc"]) ==
             {~c"~ts", [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}

    assert scan_inspect(~c"~lw", [~c"abc"]) ==
             {~c"~ts", [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}
  end

  test "with modifier for width" do
    assert scan_inspect(~c"~5lp", [~c"abc"]) ==
             {~c"~ts", [["[", "97", ",", "\n ", "98", ",", "\n ", "99", "]"]]}

    assert scan_inspect(~c"~5lw", [~c"abc"]) ==
             {~c"~ts", [["[", "97", ",", " ", "98", ",", " ", "99", "]"]]}
  end

  test "with modifier for limit" do
    assert scan_inspect(~c"~5lP", [~c"abc", 2]) ==
             {~c"~ts", [["[", "97", ",", "\n ", "98", ",", "\n ", "...", "]"]]}

    assert scan_inspect(~c"~5lW", [~c"abc", 2]) ==
             {~c"~ts", [["[", "97", ",", " ", "98", ",", " ", "...", "]"]]}
  end

  test "truncates binaries" do
    assert scan_inspect(~c"~ts", ["abcdeabcdeabcdeabcde"]) == {~c"~ts", ["abcdeabcde"]}

    assert scan_inspect(~c"~ts~ts~ts", ["abcdeabcde", "abcde", "abcde"]) ==
             {~c"~ts~ts~ts", ["abcdeabcde", "", ""]}
  end

  test "with :infinity truncate" do
    long_string = String.duplicate("foo", 10000)
    assert scan_inspect(~c"~ts", [long_string], :infinity) == {~c"~ts", [long_string]}
  end
end
