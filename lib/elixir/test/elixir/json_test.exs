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
      json = JSON.encode_to_iodata!(%WithOnly{a: :a, b: "b", c: make_ref(), d: [?d]})
      assert is_list(json)
      assert IO.iodata_to_binary(json) == "{\"a\":\"a\",\"b\":\"b\",\"d\":[100]}"
    end

    defmodule WithExcept do
      @derive {JSON.Encoder, except: [:c]}
      defstruct [:a, :b, :c, :d]
    end

    test "with except" do
      json = JSON.encode_to_iodata!(%WithExcept{a: :a, b: "b", c: make_ref(), d: [?d]})
      assert is_list(json)
      assert IO.iodata_to_binary(json) == "{\"a\":\"a\",\"b\":\"b\",\"d\":[100]}"
    end

    defmodule WithEmpty do
      @derive {JSON.Encoder, only: []}
      defstruct [:a, :b]
    end

    test "with empty" do
      assert JSON.encode_to_iodata!(%WithEmpty{}) == "{}"
    end

    test "nested without formatting" do
      assert JSON.encode!(%{user: %WithOnly{a: 1, b: "nested", c: nil, d: [1, 2]}}) ==
               "{\"user\":{\"a\":1,\"b\":\"nested\",\"d\":[1,2]}}"
    end
  end

  describe "encode with indent" do
    test "atoms" do
      assert JSON.encode!(nil, indent: 2) == "null\n"
      assert JSON.encode!(true, indent: 2) == "true\n"
      assert JSON.encode!(false, indent: 2) == "false\n"
      assert JSON.encode!(:hello, indent: 2) == "\"hello\"\n"

      formatted = JSON.encode!(%{a: nil, b: false, c: true, d: :another}, indent: 2)

      assert {:ok, %{"a" => nil, "b" => false, "c" => true, "d" => "another"}} =
               JSON.decode(formatted)
    end

    test "binaries" do
      assert JSON.encode!("hello", indent: 2) == "\"hello\"\n"
      assert JSON.encode!("hello\0world\t✂️", indent: 2) == "\"hello\\u0000world\\t✂️\"\n"
    end

    test "integers" do
      assert JSON.encode!(123_456, indent: 2) == "123456\n"
    end

    test "floats" do
      assert JSON.encode!(123.456, indent: 2) == "123.456\n"
    end

    test "maps" do
      assert JSON.encode!(%{key: "value"}, indent: 2) ==
               "{\n  \"key\": \"value\"\n}\n"

      assert JSON.encode!(%{}, indent: 2) == "{}\n"
    end

    test "nested maps" do
      assert JSON.encode!(%{a: %{b: 1}}, indent: 2) ==
               "{\n  \"a\": {\n    \"b\": 1\n  }\n}\n"
    end

    test "maps with String.Chars keys" do
      assert JSON.encode!(%{1 => "one"}, indent: 2) ==
               "{\n  \"1\": \"one\"\n}\n"

      assert JSON.encode!(%{~c"list" => "value"}, indent: 2) ==
               "{\n  \"list\": \"value\"\n}\n"

      formatted = JSON.encode!(%{1 => 2, 3.0 => 4.0, ~c"list" => ~c"list", key: :bar}, indent: 2)
      assert {:ok, decoded} = JSON.decode(formatted)
      assert decoded == %{"1" => 2, "3.0" => 4.0, "key" => "bar", "list" => [108, 105, 115, 116]}
    end

    test "lists" do
      assert JSON.encode!([1, "two", 3], indent: 2) == "[1,\"two\",3]\n"

      assert JSON.encode!(%{items: [1, 2, 3]}, indent: 2) ==
               "{\n  \"items\": [1,2,3]\n}\n"
    end

    test "custom structs" do
      nested = %{wrapper: %Token{value: %{name: "test", nested: %{x: 1}}}}

      # Compact: everything inline
      assert JSON.encode!(nested) ==
               "{\"wrapper\":[{\"name\":\"test\",\"nested\":{\"x\":1}}]}"

      # Formatted: custom wrapper stays compact, nested maps get indented
      assert JSON.encode!(nested, indent: 2) ==
               "{\n  \"wrapper\": [{\n    \"name\": \"test\",\n    \"nested\": {\n      \"x\": 1\n    }\n  }]\n}\n"
    end

    test "calendar" do
      assert JSON.encode!(~D[2010-04-17], indent: 2) == "\"2010-04-17\"\n"
      assert JSON.encode!(~T[14:00:00.123], indent: 2) == "\"14:00:00.123\"\n"

      assert JSON.encode!(~N[2010-04-17 14:00:00.123], indent: 2) ==
               "\"2010-04-17T14:00:00.123\"\n"

      assert JSON.encode!(~U[2010-04-17 14:00:00.123Z], indent: 2) ==
               "\"2010-04-17T14:00:00.123Z\"\n"

      assert JSON.encode!(Duration.new!(month: 2, hour: 3), indent: 2) == "\"P2MT3H\"\n"

      assert JSON.encode!(%{date: ~D[2010-04-17]}, indent: 2) ==
               "{\n  \"date\": \"2010-04-17\"\n}\n"

      assert JSON.encode!(%{time: ~T[14:00:00.123]}, indent: 2) ==
               "{\n  \"time\": \"14:00:00.123\"\n}\n"

      assert JSON.encode!(%{naive: ~N[2010-04-17 14:00:00.123]}, indent: 2) ==
               "{\n  \"naive\": \"2010-04-17T14:00:00.123\"\n}\n"

      assert JSON.encode!(%{utc: ~U[2010-04-17 14:00:00.123Z]}, indent: 2) ==
               "{\n  \"utc\": \"2010-04-17T14:00:00.123Z\"\n}\n"

      assert JSON.encode!(%{duration: Duration.new!(month: 2, hour: 3)}, indent: 2) ==
               "{\n  \"duration\": \"P2MT3H\"\n}\n"
    end

    test "empty containers" do
      formatted = JSON.encode!(%{map: %{}, list: []}, indent: 2)
      assert {:ok, %{"map" => %{}, "list" => []}} = JSON.decode(formatted)
    end

    test "indent: 4" do
      assert JSON.encode!(%{key: "value"}, indent: 4) ==
               "{\n    \"key\": \"value\"\n}\n"

      assert JSON.encode!(%{a: %{b: 1}}, indent: 4) ==
               "{\n    \"a\": {\n        \"b\": 1\n    }\n}\n"
    end

    test "max option" do
      # Default max (100): array stays on one line
      assert JSON.encode!(%{items: [1, 2, 3, 4, 5]}, indent: 2) ==
               "{\n  \"items\": [1,2,3,4,5]\n}\n"

      # Narrow max: array wraps
      assert JSON.encode!(%{items: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]}, indent: 2, max: 20) ==
               "{\n  \"items\": [1,2,3,4,5,6,7,8,9,10,\n    11,12]\n}\n"
    end

    test "derived structs" do
      formatted =
        JSON.encode!(%JSONTest.WithOnly{a: :a, b: "b", c: make_ref(), d: [?d]}, indent: 2)

      assert formatted ==
               "{\n  \"a\": \"a\",\n  \"b\": \"b\",\n  \"d\": [100]\n}\n"

      nested =
        JSON.encode!(%{user: %JSONTest.WithOnly{a: 1, b: "nested", c: nil, d: [1, 2]}}, indent: 2)

      assert nested ==
               "{\n  \"user\": {\n    \"a\": 1,\n    \"b\": \"nested\",\n    \"d\": [1,2]\n  }\n}\n"

      nested_4 =
        JSON.encode!(%{user: %JSONTest.WithOnly{a: 1, b: "nested", c: nil, d: [1, 2]}},
          indent: 4
        )

      assert nested_4 ==
               "{\n    \"user\": {\n        \"a\": 1,\n        \"b\": \"nested\",\n        \"d\": [1,2]\n    }\n}\n"
    end

    test "custom encoder" do
      # A custom encoder that uppercases all string values
      encoder = fn
        value, encoder when is_binary(value) ->
          :json.encode_binary(String.upcase(value))

        value, encoder ->
          JSON.protocol_encode(value, encoder)
      end

      assert JSON.encode!(%{name: "elixir"}, encoder) == "{\"NAME\":\"ELIXIR\"}"
    end

    test "encode_to_iodata with indent" do
      data = JSON.encode_to_iodata!(%{key: "value"}, indent: 2)
      assert is_list(data)
      assert IO.iodata_to_binary(data) == "{\n  \"key\": \"value\"\n}\n"
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

    test "unexpected sequence" do
      assert JSON.decode("\"\\ud8aa\\udcxx\"") ==
               {:error, {:unexpected_sequence, 1, "\\ud8aa\\udcxx"}}

      assert_raise JSON.DecodeError,
                   "unexpected sequence \"\\\\ud8aa\\\\udcxx\" at position (byte offset) 1",
                   fn -> JSON.decode!("\"\\ud8aa\\udcxx\"") end
    end
  end
end
