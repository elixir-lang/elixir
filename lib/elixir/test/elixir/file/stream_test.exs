Code.require_file("../test_helper.exs", __DIR__)

defmodule File.StreamTest do
  use ExUnit.Case
  import PathHelpers

  setup do
    File.mkdir_p!(tmp_path())
    on_exit(fn -> File.rm_rf(tmp_path()) end)
    :ok
  end

  defp stream!(node, src, lines_or_bytes_or_modes \\ []) do
    :erpc.call(node, File, :stream!, [src, lines_or_bytes_or_modes])
  end

  defp stream!(node, src, lines_or_bytes, modes) do
    :erpc.call(node, File, :stream!, [src, lines_or_bytes, modes])
  end

  distributed_node = :"secondary@#{node() |> Atom.to_string() |> :binary.split("@") |> tl()}"

  for {type, node} <- [local: node(), distributed: distributed_node] do
    describe "#{type} node" do
      @describetag type
      @node node

      test "returns a struct" do
        src = fixture_path("file.txt")
        stream = stream!(@node, src)
        assert %File.Stream{} = stream
        assert stream.modes == [:raw, :read_ahead, :binary]
        assert stream.raw
        assert stream.line_or_bytes == :line

        stream = stream!(@node, src, read_ahead: false)
        assert %File.Stream{} = stream
        assert stream.modes == [:raw, :binary]
        assert stream.raw

        stream = stream!(@node, src, read_ahead: 5000)
        assert %File.Stream{} = stream
        assert stream.modes == [:raw, {:read_ahead, 5000}, :binary]
        assert stream.raw

        stream = stream!(@node, src, [:utf8], 10)
        assert %File.Stream{} = stream
        assert stream.modes == [{:encoding, :utf8}, :binary]
        refute stream.raw
        assert stream.line_or_bytes == 10
      end

      test "counts bytes/characters" do
        src = fixture_path("file.txt")
        stream = stream!(@node, src)
        assert Enum.count(stream) == 1

        stream = stream!(@node, src, [:utf8])
        assert Enum.count(stream) == 1

        stream = stream!(@node, src, 2)
        assert Enum.count(stream) == 2
      end

      test "reads and writes lines" do
        src = fixture_path("file.txt")
        dest = tmp_path("tmp_test.txt")

        try do
          stream = stream!(@node, src)

          File.open(dest, [:write], fn target ->
            Enum.each(stream, fn line ->
              IO.write(target, String.replace(line, "O", "A"))
            end)
          end)

          assert File.read(dest) == {:ok, "FAA\n"}
        after
          File.rm(dest)
        end
      end

      test "reads and writes bytes" do
        src = fixture_path("file.txt")
        dest = tmp_path("tmp_test.txt")

        try do
          stream = stream!(@node, src, 1)

          File.open(dest, [:write], fn target ->
            Enum.each(stream, fn <<char>> ->
              IO.write(target, <<char + 1>>)
            end)
          end)

          assert File.read(dest) == {:ok, "GPP\v"}
        after
          File.rm(dest)
        end
      end

      test "is collectable" do
        src = fixture_path("file.txt")
        dest = tmp_path("tmp_test.txt")

        try do
          refute File.exists?(dest)
          original = stream!(@node, dest)

          stream =
            stream!(@node, src)
            |> Stream.map(&String.replace(&1, "O", "A"))
            |> Enum.into(original)

          assert stream == original
          assert File.read(dest) == {:ok, "FAA\n"}
        after
          File.rm(dest)
        end
      end

      test "is collectable with append" do
        src = fixture_path("file.txt")
        dest = tmp_path("tmp_test.txt")

        try do
          refute File.exists?(dest)
          original = stream!(@node, dest, [:append])

          stream!(@node, src, [:append])
          |> Stream.map(&String.replace(&1, "O", "A"))
          |> Enum.into(original)

          stream!(@node, src, [:append])
          |> Enum.into(original)

          assert File.read(dest) == {:ok, "FAA\nFOO\n"}
        after
          File.rm(dest)
        end
      end

      test "supports byte offset" do
        src = fixture_path("file.txt")

        assert @node
               |> stream!(src, [{:read_offset, 0}])
               |> Enum.take(1) == ["FOO\n"]

        assert @node
               |> stream!(src, [{:read_offset, 1}])
               |> Enum.take(1) == ["OO\n"]

        assert @node
               |> stream!(src, [{:read_offset, 4}])
               |> Enum.take(1) == []

        assert @node |> stream!(src, 1, [{:read_offset, 1}]) |> Enum.count() == 3
        assert @node |> stream!(src, 1, [{:read_offset, 4}]) |> Enum.count() == 0
      end

      test "applies offset after trimming BOM" do
        src = fixture_path("utf8_bom.txt")

        assert @node
               |> stream!(src, [:trim_bom, {:read_offset, 4}])
               |> Enum.take(1) == ["сский\n"]

        assert @node |> stream!(src, 1, [:trim_bom, {:read_offset, 4}]) |> Enum.count() == 15
      end

      test "keeps BOM when raw" do
        src = fixture_path("utf8_bom.txt")

        assert @node
               |> stream!(src, [])
               |> Enum.take(1) == [<<239, 187, 191>> <> "Русский\n"]

        assert @node
               |> stream!(src, 1)
               |> Enum.take(5) == [<<239>>, <<187>>, <<191>>, <<208>>, <<160>>]

        assert @node |> stream!(src, []) |> Enum.count() == 2
        assert @node |> stream!(src, 1) |> Enum.count() == 22
      end

      test "trims BOM via option when raw" do
        src = fixture_path("utf8_bom.txt")

        assert @node
               |> stream!(src, [:trim_bom])
               |> Enum.take(1) == ["Русский\n"]

        assert @node
               |> stream!(src, [:trim_bom], 1)
               |> Enum.take(5) == [<<208>>, <<160>>, <<209>>, <<131>>, <<209>>]

        assert @node |> stream!(src, [:trim_bom]) |> Enum.count() == 2
        assert @node |> stream!(src, 1, [:trim_bom]) |> Enum.count() == 19
        assert @node |> stream!(src, 2, [:trim_bom]) |> Enum.count() == 10
      end

      test "keeps BOM with utf8 encoding" do
        src = fixture_path("utf8_bom.txt")

        assert @node
               |> stream!(src, [{:encoding, :utf8}])
               |> Enum.take(1) == [<<239, 187, 191>> <> "Русский\n"]

        assert @node
               |> stream!(src, 1, [{:encoding, :utf8}])
               |> Enum.take(9) == ["\uFEFF", "Р", "у", "с", "с", "к", "и", "й", "\n"]
      end

      test "trims BOM via option with utf8 encoding" do
        src = fixture_path("utf8_bom.txt")

        assert @node
               |> stream!(src, [{:encoding, :utf8}, :trim_bom])
               |> Enum.take(1) == ["Русский\n"]

        assert @node
               |> stream!(src, 1, [{:encoding, :utf8}, :trim_bom])
               |> Enum.take(8) == ["Р", "у", "с", "с", "к", "и", "й", "\n"]
      end

      test "keeps BOM with UTF16 BE" do
        src = fixture_path("utf16_be_bom.txt")

        assert @node
               |> stream!(src, [{:encoding, {:utf16, :big}}])
               |> Enum.take(1) == ["\uFEFFРусский\n"]
      end

      test "keeps BOM with UTF16 LE" do
        src = fixture_path("utf16_le_bom.txt")

        assert @node
               |> stream!(src, [{:encoding, {:utf16, :little}}])
               |> Enum.take(1) == ["\uFEFFРусский\n"]
      end

      test "trims BOM via option with utf16 BE encoding" do
        src = fixture_path("utf16_be_bom.txt")

        assert @node
               |> stream!(src, [{:encoding, {:utf16, :big}}, :trim_bom])
               |> Enum.take(1) == ["Русский\n"]

        assert @node
               |> stream!(src, 1, [{:encoding, {:utf16, :big}}, :trim_bom])
               |> Enum.take(8) == ["Р", "у", "с", "с", "к", "и", "й", "\n"]
      end

      test "trims BOM via option with utf16 LE encoding" do
        src = fixture_path("utf16_le_bom.txt")

        assert @node
               |> stream!(src, [{:encoding, {:utf16, :little}}, :trim_bom])
               |> Enum.take(1) == ["Русский\n"]

        assert @node
               |> stream!(src, 1, [{:encoding, {:utf16, :little}}, :trim_bom])
               |> Enum.take(8) == ["Р", "у", "с", "с", "к", "и", "й", "\n"]
      end

      test "reads and writes line by line in UTF-8" do
        src = fixture_path("file.txt")
        dest = tmp_path("tmp_test.txt")

        try do
          stream = stream!(@node, src)

          File.open(dest, [:write, :utf8], fn target ->
            Enum.each(stream, fn line ->
              IO.write(target, String.replace(line, "O", "A"))
            end)
          end)

          assert File.read(dest) == {:ok, "FAA\n"}
        after
          File.rm(dest)
        end
      end

      test "reads and writes character in UTF-8" do
        src = fixture_path("file.txt")
        dest = tmp_path("tmp_test.txt")

        try do
          stream = stream!(@node, src, 1, [:utf8])

          File.open(dest, [:write], fn target ->
            Enum.each(stream, fn <<char::utf8>> ->
              IO.write(target, <<char + 1::utf8>>)
            end)
          end)

          assert File.read(dest) == {:ok, "GPP\v"}
        after
          File.rm(dest)
        end
      end
    end
  end
end
