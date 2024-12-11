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
      assert JSON.encode!(%{1 => 2, 3.0 => 4.0, key: :bar}) ==
               "{\"1\":2,\"3.0\":4.0,\"key\":\"bar\"}"
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
    end
  end

  describe "JSON.Encoder" do
    defp protocol_encode(term) do
      term
      |> JSON.Encoder.encode(&JSON.protocol_encode/2)
      |> IO.iodata_to_binary()
    end

    test "atoms" do
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
      assert protocol_encode(%{1 => 2, 3.0 => 4.0, key: :bar}) ==
               "{\"1\":2,\"3.0\":4.0,\"key\":\"bar\"}"
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
      defstruct [:a, :b, :c, :d]
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

    test "unexpected sequence" do
      assert JSON.decode("\"\\ud8aa\\udcxx\"") ==
               {:error, {:unexpected_sequence, 1, "\\ud8aa\\udcxx"}}

      assert_raise JSON.DecodeError,
                   "unexpected sequence \"\\\\ud8aa\\\\udcxx\" at position (byte offset) 1",
                   fn -> JSON.decode!("\"\\ud8aa\\udcxx\"") end
    end
  end
end
