# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("test_helper.exs", __DIR__)

defmodule JSONTest do
  use ExUnit.Case, async: true

  defmodule Token do
    defstruct [:value]

    defimpl JSON.Encoder do
      def encode(token, encoder) do
        [?[, encoder.(token.value, encoder), ?]]
      end
    end
  end

  doctest JSON

  defmodule Malformed do
    defstruct [:output]

    defimpl JSON.Encoder do
      def encode(%{output: output}, _encoder), do: output
    end
  end

  describe "encode" do
    test "atoms" do
      assert JSON.encode!([nil, false, true, :another]) == "[null,false,true,\"another\"]"
    end

    test "binaries" do
      assert JSON.encode!("hello\0world\t✂️") == "\"hello\\u0000world\\t✂️\""
    end

    test "integers" do
      assert JSON.encode!(123_456) == "123456"
    end

    test "floats" do
      assert JSON.encode!(123.456) == "123.456"
    end

    test "maps" do
      assert JSON.encode!(%{1 => 2, 3.0 => 4.0, ~c"list" => ~c"list", key: :bar}) ==
               "{\"1\":2,\"3.0\":4.0,\"key\":\"bar\",\"list\":[108,105,115,116]}"
    end

    test "lists" do
      assert JSON.encode!([1, 1.0, "one", %{1 => 2, 3.0 => 4.0, key: :bar}]) ==
               "[1,1.0,\"one\",{\"1\":2,\"3.0\":4.0,\"key\":\"bar\"}]"
    end

    test "structs" do
      assert JSON.encode!(%Token{value: :example}) == "[\"example\"]"
      assert JSON.encode!(%Token{value: "hello\0world"}) == "[\"hello\\u0000world\"]"
    end

    test "calendar" do
      assert JSON.encode!(~D[2010-04-17]) == "\"2010-04-17\""
      assert JSON.encode!(~T[14:00:00.123]) == "\"14:00:00.123\""
      assert JSON.encode!(~N[2010-04-17 14:00:00.123]) == "\"2010-04-17T14:00:00.123\""
      assert JSON.encode!(~U[2010-04-17 14:00:00.123Z]) == "\"2010-04-17T14:00:00.123Z\""
      assert JSON.encode!(Duration.new!(month: 2, hour: 3)) == "\"P2MT3H\""
    end
  end

  describe "encode with pretty" do
    test "indents nested members" do
      assert JSON.encode!(%{a: [1, %{b: true}]}, pretty: true) ==
               "{\n  \"a\": [\n    1,\n    {\n      \"b\": true\n    }\n  ]\n}"
    end

    test "keeps empty containers on a single line" do
      assert JSON.encode!([[], %{}, [1]], pretty: true) == "[\n  [],\n  {},\n  [\n    1\n  ]\n]"
    end

    test "accepts a custom indent unit" do
      assert JSON.encode!(%{a: 1}, pretty: "\t") == "{\n\t\"a\": 1\n}"
      assert JSON.encode!(%{a: 1}, pretty: "    ") == "{\n    \"a\": 1\n}"
      assert JSON.encode!(%{a: 1}, pretty: "") == "{\n\"a\": 1\n}"
    end

    test "is compact by default" do
      assert JSON.encode!(%{a: [1]}) == "{\"a\":[1]}"
      assert JSON.encode!(%{a: [1]}, pretty: false) == "{\"a\":[1]}"
    end

    test "preserves string contents" do
      term = %{a: "1,2:3 {\"}]"}
      assert term |> JSON.encode!(pretty: true) |> JSON.decode!() == %{"a" => "1,2:3 {\"}]"}
      assert JSON.encode!(%{a: "1,2:3"}, pretty: true) == "{\n  \"a\": \"1,2:3\"\n}"
    end

    test "supports structs and calendar types" do
      assert JSON.encode!(%{date: ~D[2024-01-01]}, pretty: true) ==
               "{\n  \"date\": \"2024-01-01\"\n}"

      assert JSON.encode!(%{nil: nil}, pretty: true) == "{\n  \"nil\": null\n}"

      assert JSON.encode!(%{duration: Duration.new!(second: 1)}, pretty: true) ==
               "{\n  \"duration\": \"PT1S\"\n}"
    end

    test "supports iodata" do
      assert IO.iodata_to_binary(JSON.encode_to_iodata!(%{a: [1]}, pretty: true)) ==
               "{\n  \"a\": [\n    1\n  ]\n}"
    end

    test "validates the second argument" do
      assert_raise ArgumentError,
                   "expected :pretty to be a boolean or a binary, got: 2",
                   fn -> JSON.encode!(%{}, pretty: 2) end

      assert_raise ArgumentError,
                   "expected :pretty to be a binary of spaces and tabs, got: \"abc\"",
                   fn -> JSON.encode!(%{}, pretty: "abc") end

      assert_raise ArgumentError,
                   "unknown options [:indent], expected only :pretty",
                   fn -> JSON.encode!(%{}, indent: 4) end

      assert_raise ArgumentError,
                   "expected the second argument to be a function or a keyword list, got: [1, 2]",
                   fn -> JSON.encode!(%{}, [1, 2]) end
    end

    test "accepts any indent width" do
      assert JSON.encode!(%{a: 1}, pretty: String.duplicate(" ", 20)) ==
               "{\n#{String.duplicate(" ", 20)}\"a\": 1\n}"
    end

    test "handles scalars and empty containers" do
      assert JSON.encode!(nil, pretty: true) == "null"
      assert JSON.encode!("abc", pretty: true) == "\"abc\""
      assert JSON.encode!(%{}, pretty: true) == "{}"
      assert JSON.encode!([], pretty: true) == "[]"
    end

    test "keeps escaped characters intact" do
      assert JSON.encode!(%{a: "é"}, pretty: true) == "{\n  \"a\": \"é\"\n}"
      assert JSON.encode!(%{a: "\u0000"}, pretty: true) == "{\n  \"a\": \"\\u0000\"\n}"
    end

    test "raises on malformed encoder output" do
      assert_raise ArgumentError,
                   "expected the encoder to return valid JSON, got unexpected }",
                   fn -> JSON.encode!(struct(Malformed, output: "}]"), pretty: true) end

      assert_raise ArgumentError,
                   "expected the encoder to return valid JSON, got an unterminated string",
                   fn -> JSON.encode!(struct(Malformed, output: ~s("abc)), pretty: true) end
    end

    test "keeps custom encoders working" do
      assert JSON.encode!(%Token{value: 1}) == "[1]"
    end
  end

  describe "JSON.Encoder" do
    defp protocol_encode(term) do
      term
      |> JSON.Encoder.encode(&JSON.protocol_encode/2)
      |> IO.iodata_to_binary()
    end

    test "atoms" do
      assert protocol_encode(:another) == "\"another\""
      assert protocol_encode([nil, false, true, :another]) == "[null,false,true,\"another\"]"
    end

    test "binaries" do
      assert protocol_encode("hello\0world\t✂️") == "\"hello\\u0000world\\t✂️\""
    end

    test "integers" do
      assert protocol_encode(123_456) == "123456"
    end

    test "floats" do
      assert protocol_encode(123.456) == "123.456"
    end

    test "maps" do
      assert protocol_encode(%{1 => 2, 3.0 => 4.0, ~c"list" => ~c"list", key: :bar}) ==
               "{\"1\":2,\"3.0\":4.0,\"key\":\"bar\",\"list\":[108,105,115,116]}"
    end

    test "lists" do
      assert protocol_encode([1, 1.0, "one", %{1 => 2, 3.0 => 4.0, key: :bar}]) ==
               "[1,1.0,\"one\",{\"1\":2,\"3.0\":4.0,\"key\":\"bar\"}]"
    end

    test "structs" do
      assert protocol_encode(%Token{value: :example}) == "[\"example\"]"
      assert protocol_encode(%Token{value: "hello\0world"}) == "[\"hello\\u0000world\"]"
    end
  end

  test "encode_to_iodata" do
    list = JSON.encode_to_iodata!([1, 1.0, "one", %{1 => 2, 3.0 => 4.0, key: :bar}])
    assert is_list(list)
    assert IO.iodata_to_binary(list) == "[1,1.0,\"one\",{\"1\":2,\"3.0\":4.0,\"key\":\"bar\"}]"

    list =
      JSON.encode_to_iodata!([
        ~T[12:34:56.78],
        ~D[2024-12-31],
        ~N[2010-04-17 14:00:00.123],
        ~U[2010-04-17 14:00:00.123Z],
        Duration.new!(month: 2, hour: 3)
      ])

    assert IO.iodata_to_binary(list) ==
             ~s'["12:34:56.78","2024-12-31","2010-04-17T14:00:00.123","2010-04-17T14:00:00.123Z","P2MT3H"]'
  end

  test "deprecated" do
    assert JSON.encode!([:hello, "world"]) == "[\"hello\",\"world\"]"

    list = JSON.encode_to_iodata!([:hello, "world"])
    assert is_list(list)
    assert IO.iodata_to_binary(list) == "[\"hello\",\"world\"]"
  end

  describe "deriving" do
    defmodule WithOnly do
      @derive {JSON.Encoder, only: [:a, :b, :d]}
      # The encoded order depends on only
      defstruct Enum.shuffle([:a, :b, :c, :d])
    end

    test "with only" do
      assert ["{\"a\":", _, ",\"b\":", _, ",\"d\":", _, 125] =
               json = JSON.encode_to_iodata!(%WithOnly{a: :a, b: "b", c: make_ref(), d: [?d]})

      assert IO.iodata_to_binary(json) == "{\"a\":\"a\",\"b\":\"b\",\"d\":[100]}"
    end

    defmodule WithExcept do
      @derive {JSON.Encoder, except: [:c]}
      defstruct [:a, :b, :c, :d]
    end

    test "with except" do
      assert ["{\"a\":", _, ",\"b\":", _, ",\"d\":", _, 125] =
               json = JSON.encode_to_iodata!(%WithExcept{a: :a, b: "b", c: make_ref(), d: [?d]})

      assert IO.iodata_to_binary(json) == "{\"a\":\"a\",\"b\":\"b\",\"d\":[100]}"
    end

    defmodule WithEmpty do
      @derive {JSON.Encoder, only: []}
      defstruct [:a, :b]
    end

    test "with empty" do
      assert JSON.encode_to_iodata!(%WithEmpty{}) == "{}"
    end
  end

  describe "decode" do
    test "succeeds" do
      assert JSON.decode("[null,123,456.7,\"string\",{\"key\":\"value\"}]") ==
               {:ok, [nil, 123, 456.7, "string", %{"key" => "value"}]}

      assert JSON.decode!("[null,123,456.7,\"string\",{\"key\":\"value\"}]") ==
               [nil, 123, 456.7, "string", %{"key" => "value"}]
    end

    test "unexpected end" do
      assert JSON.decode("{") == {:error, {:unexpected_end, 1}}

      assert_raise JSON.DecodeError,
                   "unexpected end of JSON binary at position (byte offset) 1",
                   fn -> JSON.decode!("{") end
    end

    test "invalid byte" do
      assert JSON.decode(",") == {:error, {:invalid_byte, 0, ?,}}
      assert JSON.decode("123o") == {:error, {:invalid_byte, 3, ?o}}

      assert_raise JSON.DecodeError,
                   "invalid byte 111 at position (byte offset) 3",
                   fn -> JSON.decode!("123o") end
    end

    test "integer too large" do
      digits = String.duplicate("9", 310)
      assert JSON.decode(digits) == {:ok, String.to_integer(digits)}
      assert JSON.decode("-" <> digits) == {:ok, String.to_integer("-" <> digits)}
      assert JSON.decode(digits <> "9") == {:error, {:integer_too_large, 311}}
      assert JSON.decode("[1, -#{digits}9]") == {:error, {:integer_too_large, 311}}
      assert JSON.decode(digits <> "9", :ok, integer: & &1) == {digits <> "9", :ok, ""}

      assert_raise JSON.DecodeError,
                   "integer with 311 digits exceeds the limit of 310 digits",
                   fn -> JSON.decode!(digits <> "9") end
    end

    test "unexpected sequence" do
      assert JSON.decode("\"\\ud8aa\\udcxx\"") ==
               {:error, {:unexpected_sequence, 1, "\\ud8aa\\udcxx"}}

      assert_raise JSON.DecodeError,
                   "unexpected sequence \"\\\\ud8aa\\\\udcxx\" at position (byte offset) 1",
                   fn -> JSON.decode!("\"\\ud8aa\\udcxx\"") end
    end
  end
end
