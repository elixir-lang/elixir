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
  end

  describe "JSON.Encoder" do
    defp protocol_encode(term) do
      term
      |> JSON.Encoder.encode(&JSON.encode_value/2)
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
end
