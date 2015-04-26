Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ComprehensionTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO
  require Integer

  defmodule PDict do
    defstruct []

    defimpl Collectable do
      def into(struct) do
        {struct,
         fn
           _, {:cont, x} -> Process.put(:into_cont, [x|Process.get(:into_cont)])
           _, :done -> Process.put(:into_done, true)
           _, :halt -> Process.put(:into_halt, true)
         end}
      end
    end
  end

  defp to_bin(x) do
    << x >>
  end

  defp nilly, do: nil

  ## Enum comprehensions (the common case)

  test "for comprehensions" do
    enum = 1..3
    assert for(x <- enum, do: x * 2) == [2, 4, 6]
  end

  test "for comprehensions with matching" do
    assert for({_, x} <- 1..3, do: x * 2) == []
  end

  test "for comprehensions with filters" do
    assert for(x <- 1..3, x > 1, x < 3, do: x * 2) == [4]
  end

  test "for comprehensions with nilly filters" do
    assert for(x <- 1..3, nilly, do: x * 2) == []
  end

  test "for comprehensions with errors on filters" do
    assert_raise ArgumentError, fn ->
      for(x <- 1..3, hd(x), do: x * 2)
    end
  end

  test "for comprehensions with variables in filters" do
    assert for(x <- 1..3, y = x + 1, y > 2, z = y, do: x * z) ==
           [6, 12]
  end

  test "for comprehensions with two enum generators" do
    assert (for x <- [1, 2, 3], y <- [4, 5, 6], do: x * y) ==
           [4, 5, 6, 8, 10, 12, 12, 15, 18]
  end

  test "for comprehensions with two enum generators and filters" do
    assert (for x <- [1, 2, 3], y <- [4, 5, 6], y / 2 == x, do: x * y) ==
           [8, 18]
  end

  test "for comprehensions generators precedence" do
    assert (for {_, _} = x <- [foo: :bar], do: x) ==
           [foo: :bar]
  end

  test "for comprehensions with binary, enum generators and filters" do
    assert (for x <- [1, 2, 3], << y <- <<4, 5, 6>> >>, y / 2 == x, do: x * y) ==
           [8, 18]
  end

  test "for comprehensions into list" do
    enum = 1..3
    assert for(x <- enum, into: [], do: x * 2) == [2, 4, 6]
  end

  test "for comprehensions into binary" do
    enum = 1..3
    assert for(x <- enum, into: "", do: to_bin(x * 2)) == <<2, 4, 6>>
  end

  test "for comprehensions where value is not used" do
    enum = 1..3

    assert capture_io(fn ->
      for(x <- enum, do: IO.puts x)
      nil
    end) == "1\n2\n3\n"
  end

  test "for comprehensions with into" do
    Process.put(:into_cont, [])
    Process.put(:into_done, false)
    Process.put(:into_halt, false)

    for x <- 1..3, into: %PDict{} do
      x * 2
    end

    assert Process.get(:into_cont) == [6, 4, 2]
    assert Process.get(:into_done)
    refute Process.get(:into_halt)
  end

  test "for comprehension with into leading to errors" do
    Process.put(:into_cont, [])
    Process.put(:into_done, false)
    Process.put(:into_halt, false)

    catch_error(
      for x <- 1..3, into: %PDict{} do
        if x > 2, do: raise("oops"), else: x
      end
    )

    assert Process.get(:into_cont) == [2, 1]
    refute Process.get(:into_done)
    assert Process.get(:into_halt)
  end

  test "for comprehension with into, generators and filters" do
    Process.put(:into_cont, [])

    for x <- 1..3, Integer.is_odd(x), << y <- "hello" >>, into: %PDict{} do
      x + y
    end

    assert IO.iodata_to_binary(Process.get(:into_cont)) == "roohkpmmfi"
  end

  ## List generators (inlined by the compiler)

  test "list for comprehensions" do
    list = [1, 2, 3]
    assert for(x <- list, do: x * 2) == [2, 4, 6]
  end

  test "list for comprehensions with matching" do
    assert for({_, x} <- [1, 2, a: 3, b: 4, c: 5], do: x * 2) == [6, 8, 10]
  end

  test "list for comprehension matched to '_' on last line of block" do
    assert (if true do
      _ = for x <- [1, 2, 3], do: x * 2
    end) == [2, 4, 6]
  end

  test "list for comprehensions with filters" do
    assert for(x <- [1, 2, 3], x > 1, x < 3, do: x * 2) == [4]
  end

  test "list for comprehensions with nilly filters" do
    assert for(x <- [1, 2, 3], nilly, do: x * 2) == []
  end

  test "list for comprehensions with errors on filters" do
    assert_raise ArgumentError, fn ->
      for(x <- [1, 2, 3], hd(x), do: x * 2)
    end
  end

  test "list for comprehensions with variables in filters" do
    assert for(x <- [1, 2, 3], y = x + 1, y > 2, z = y, do: x * z) ==
           [6, 12]
  end

  test "list for comprehensions into list" do
    enum = [1, 2, 3]
    assert for(x <- enum, into: [], do: x * 2) == [2, 4, 6]
  end

  test "list for comprehensions into binaries" do
    enum = [1, 2, 3]
    assert for(x <- enum, into: "", do: to_bin(x * 2)) == <<2, 4, 6>>
  end

  test "list for comprehensions where value is not used" do
    enum = [1, 2, 3]

    assert capture_io(fn ->
      for(x <- enum, do: IO.puts x)
      nil
    end) == "1\n2\n3\n"
  end

  ## Binary generators (inlined by the compiler)

  test "binary for comprehensions" do
    bin = <<1, 2, 3>>
    assert for(<< x <- bin >>, do: x * 2) == [2, 4, 6]
  end

  test "binary for comprehensions with inner binary" do
    bin = <<1, 2, 3>>
    assert for(<< <<x>> <- bin >>, do: x * 2) == [2, 4, 6]
  end

  test "binary for comprehensions with two generators" do
    assert (for << x <- <<1, 2, 3>> >>, << y <- <<4, 5, 6>> >>, y / 2 == x, do: x * y) ==
           [8, 18]
  end

  test "binary for comprehensions into list" do
    bin = <<1, 2, 3>>
    assert for(<< x <- bin >>, into: [], do: x * 2) == [2, 4, 6]
  end

  test "binary for comprehensions into binaries" do
    bin = <<1, 2, 3>>
    assert for(<< x <- bin >>, into: "", do: to_bin(x * 2)) == <<2, 4, 6>>
  end

  test "binary for comprehensions where value is not used" do
    bin = <<1, 2, 3>>

    assert capture_io(fn ->
      for(<<x <- bin>>, do: IO.puts x)
      nil
    end) == "1\n2\n3\n"
  end
end
