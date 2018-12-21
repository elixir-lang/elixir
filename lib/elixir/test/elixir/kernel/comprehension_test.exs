Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ComprehensionTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO
  require Integer

  defmodule Pdict do
    defstruct []

    defimpl Collectable do
      def into(struct) do
        fun = fn
          _, {:cont, x} -> Process.put(:into_cont, [x | Process.get(:into_cont)])
          _, :done -> Process.put(:into_done, true)
          _, :halt -> Process.put(:into_halt, true)
        end

        {struct, fun}
      end
    end
  end

  defp to_bin(x) do
    <<x>>
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

  test "for comprehensions with pin matching" do
    maps = [x: 1, y: 2, x: 3]
    assert for({:x, v} <- maps, do: v * 2) == [2, 6]
    x = :x
    assert for({^x, v} <- maps, do: v * 2) == [2, 6]
  end

  test "for comprehensions with guards" do
    assert for(x when x < 4 <- 1..10, do: x) == [1, 2, 3]
    assert for(x when x == 3 when x == 7 <- 1..10, do: x) == [3, 7]
  end

  test "for comprehensions with guards and filters" do
    assert for(
             {var, _}
             when is_atom(var) <- [{:foo, 1}, {2, :bar}],
             var = Atom.to_string(var),
             do: var
           ) == ["foo"]
  end

  test "for comprehensions with map key matching" do
    maps = [%{x: 1}, %{y: 2}, %{x: 3}]
    assert for(%{x: v} <- maps, do: v * 2) == [2, 6]
    x = :x
    assert for(%{^x => v} <- maps, do: v * 2) == [2, 6]
  end

  test "for comprehensions with filters" do
    assert for(x <- 1..3, x > 1, x < 3, do: x * 2) == [4]
  end

  test "for comprehensions with unique values" do
    list = [1, 1, 2, 3]
    assert for(x <- list, uniq: true, do: x * 2) == [2, 4, 6]
    assert for(x <- list, uniq: true, into: [], do: x * 2) == [2, 4, 6]
    assert for(x <- list, uniq: true, into: %{}, do: {x, 1}) == %{1 => 1, 2 => 1, 3 => 1}
    assert for(x <- list, uniq: true, into: "", do: to_bin(x * 2)) == <<2, 4, 6>>
    assert for(<<x <- "abcabc">>, uniq: true, into: "", do: to_bin(x)) == "abc"

    Process.put(:into_cont, [])
    Process.put(:into_done, false)
    Process.put(:into_halt, false)

    for x <- list, uniq: true, into: %Pdict{} do
      x * 2
    end

    assert Process.get(:into_cont) == [6, 4, 2]
    assert Process.get(:into_done)
    refute Process.get(:into_halt)

    assert_raise RuntimeError, "oops", fn ->
      for _ <- [1, 2, 3], uniq: true, into: %Pdict{}, do: raise("oops")
    end

    assert Process.get(:into_halt)
  end

  test "for comprehensions with nilly filters" do
    assert for(x <- 1..3, nilly(), do: x * 2) == []
  end

  test "for comprehensions with errors on filters" do
    assert_raise ArgumentError, fn ->
      for x <- 1..3, hd(x), do: x * 2
    end
  end

  test "for comprehensions with variables in filters" do
    assert for(x <- 1..3, y = x + 1, y > 2, z = y, do: x * z) == [6, 12]
  end

  test "for comprehensions with two enum generators" do
    assert for(
             x <- [1, 2, 3],
             y <- [4, 5, 6],
             do: x * y
           ) == [4, 5, 6, 8, 10, 12, 12, 15, 18]
  end

  test "for comprehensions with two enum generators and filters" do
    assert for(
             x <- [1, 2, 3],
             y <- [4, 5, 6],
             y / 2 == x,
             do: x * y
           ) == [8, 18]
  end

  test "for comprehensions generators precedence" do
    assert for({_, _} = x <- [foo: :bar], do: x) == [foo: :bar]
  end

  test "for comprehensions with binary, enum generators and filters" do
    assert for(x <- [1, 2, 3], <<(y <- <<4, 5, 6>>)>>, y / 2 == x, do: x * y) == [8, 18]
  end

  test "for comprehensions into list" do
    enum = 1..3
    assert for(x <- enum, into: [], do: x * 2) == [2, 4, 6]
  end

  test "for comprehensions into binary" do
    enum = 0..3

    assert (for x <- enum, into: "" do
              to_bin(x * 2)
            end) == <<0, 2, 4, 6>>

    assert (for x <- enum, into: "" do
              if Integer.is_even(x), do: <<x::size(2)>>, else: <<x::size(1)>>
            end) == <<0::size(2), 1::size(1), 2::size(2), 3::size(1)>>
  end

  test "for comprehensions into dynamic binary" do
    enum = 0..3
    into = ""

    assert (for x <- enum, into: into do
              to_bin(x * 2)
            end) == <<0, 2, 4, 6>>

    assert (for x <- enum, into: into do
              if Integer.is_even(x), do: <<x::size(2)>>, else: <<x::size(1)>>
            end) == <<0::size(2), 1::size(1), 2::size(2), 3::size(1)>>

    into = <<7::size(1)>>

    assert (for x <- enum, into: into do
              to_bin(x * 2)
            end) == <<7::size(1), 0, 2, 4, 6>>

    assert (for x <- enum, into: into do
              if Integer.is_even(x), do: <<x::size(2)>>, else: <<x::size(1)>>
            end) == <<7::size(1), 0::size(2), 1::size(1), 2::size(2), 3::size(1)>>
  end

  test "for comprehensions where value is not used" do
    enum = 1..3

    assert capture_io(fn ->
             for x <- enum, do: IO.puts(x)
             nil
           end) == "1\n2\n3\n"
  end

  test "for comprehensions with into" do
    Process.put(:into_cont, [])
    Process.put(:into_done, false)
    Process.put(:into_halt, false)

    for x <- 1..3, into: %Pdict{} do
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
      for x <- 1..3, into: %Pdict{} do
        if x > 2, do: raise("oops"), else: x
      end
    )

    assert Process.get(:into_cont) == [2, 1]
    refute Process.get(:into_done)
    assert Process.get(:into_halt)
  end

  test "for comprehension with into, generators and filters" do
    Process.put(:into_cont, [])

    for x <- 1..3, Integer.is_odd(x), <<y <- "hello">>, into: %Pdict{} do
      x + y
    end

    assert IO.iodata_to_binary(Process.get(:into_cont)) == "roohkpmmfi"
  end

  test "for comprehensions of map into map" do
    enum = %{a: 2, b: 3}
    assert for({k, v} <- enum, into: %{}, do: {k, v * v}) == %{a: 4, b: 9}
  end

  test "for comprehensions with reduce, generators and filters" do
    acc =
      for x <- 1..3, Integer.is_odd(x), <<y <- "hello">>, reduce: %{} do
        acc -> Map.update(acc, x, [y], &[y | &1])
      end

    assert acc == %{1 => 'olleh', 3 => 'olleh'}
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
    assert (if true_fun() do
              _ = for x <- [1, 2, 3], do: x * 2
            end) == [2, 4, 6]
  end

  defp true_fun(), do: true

  test "list for comprehensions with filters" do
    assert for(x <- [1, 2, 3], x > 1, x < 3, do: x * 2) == [4]
  end

  test "list for comprehensions with nilly filters" do
    assert for(x <- [1, 2, 3], nilly(), do: x * 2) == []
  end

  test "list for comprehensions with errors on filters" do
    assert_raise ArgumentError, fn ->
      for x <- [1, 2, 3], hd(x), do: x * 2
    end
  end

  test "list for comprehensions with variables in filters" do
    assert for(x <- [1, 2, 3], y = x + 1, y > 2, z = y, do: x * z) == [6, 12]
  end

  test "list for comprehensions into list" do
    enum = [1, 2, 3]
    assert for(x <- enum, into: [], do: x * 2) == [2, 4, 6]
  end

  test "list for comprehensions into binary" do
    enum = [0, 1, 2, 3]

    assert (for x <- enum, into: "" do
              to_bin(x * 2)
            end) == <<0, 2, 4, 6>>

    assert (for x <- enum, into: "" do
              if Integer.is_even(x), do: <<x::size(2)>>, else: <<x::size(1)>>
            end) == <<0::size(2), 1::size(1), 2::size(2), 3::size(1)>>
  end

  test "list for comprehensions into dynamic binary" do
    enum = [0, 1, 2, 3]
    into = ""

    assert (for x <- enum, into: into do
              to_bin(x * 2)
            end) == <<0, 2, 4, 6>>

    assert (for x <- enum, into: into do
              if Integer.is_even(x), do: <<x::size(2)>>, else: <<x::size(1)>>
            end) == <<0::size(2), 1::size(1), 2::size(2), 3::size(1)>>

    into = <<7::size(1)>>

    assert (for x <- enum, into: into do
              to_bin(x * 2)
            end) == <<7::size(1), 0, 2, 4, 6>>

    assert (for x <- enum, into: into do
              if Integer.is_even(x), do: <<x::size(2)>>, else: <<x::size(1)>>
            end) == <<7::size(1), 0::size(2), 1::size(1), 2::size(2), 3::size(1)>>
  end

  test "list for comprehensions where value is not used" do
    enum = [1, 2, 3]

    assert capture_io(fn ->
             for x <- enum, do: IO.puts(x)
             nil
           end) == "1\n2\n3\n"
  end

  test "list for comprehensions with reduce, generators and filters" do
    acc =
      for x <- [1, 2, 3], Integer.is_odd(x), <<y <- "hello">>, reduce: %{} do
        acc -> Map.update(acc, x, [y], &[y | &1])
      end

    assert acc == %{1 => 'olleh', 3 => 'olleh'}
  end

  ## Binary generators (inlined by the compiler)

  test "binary for comprehensions" do
    bin = <<1, 2, 3>>
    assert for(<<x <- bin>>, do: x * 2) == [2, 4, 6]
  end

  test "binary for comprehensions with inner binary" do
    bin = <<1, 2, 3>>
    assert for(<<(<<x>> <- bin)>>, do: x * 2) == [2, 4, 6]
  end

  test "binary for comprehensions with two generators" do
    assert for(<<(x <- <<1, 2, 3>>)>>, <<(y <- <<4, 5, 6>>)>>, y / 2 == x, do: x * y) == [8, 18]
  end

  test "binary for comprehensions into list" do
    bin = <<1, 2, 3>>
    assert for(<<x <- bin>>, into: [], do: x * 2) == [2, 4, 6]
  end

  test "binary for comprehensions into binary" do
    bin = <<0, 1, 2, 3>>

    assert (for <<x <- bin>>, into: "" do
              to_bin(x * 2)
            end) == <<0, 2, 4, 6>>

    assert (for <<x <- bin>>, into: "" do
              if Integer.is_even(x), do: <<x::size(2)>>, else: <<x::size(1)>>
            end) == <<0::size(2), 1::size(1), 2::size(2), 3::size(1)>>
  end

  test "binary for comprehensions into dynamic binary" do
    bin = <<0, 1, 2, 3>>
    into = ""

    assert (for <<x <- bin>>, into: into do
              to_bin(x * 2)
            end) == <<0, 2, 4, 6>>

    assert (for <<x <- bin>>, into: into do
              if Integer.is_even(x), do: <<x::size(2)>>, else: <<x::size(1)>>
            end) == <<0::size(2), 1::size(1), 2::size(2), 3::size(1)>>

    into = <<7::size(1)>>

    assert (for <<x <- bin>>, into: into do
              to_bin(x * 2)
            end) == <<7::size(1), 0, 2, 4, 6>>

    assert (for <<x <- bin>>, into: into do
              if Integer.is_even(x), do: <<x::size(2)>>, else: <<x::size(1)>>
            end) == <<7::size(1), 0::size(2), 1::size(1), 2::size(2), 3::size(1)>>
  end

  test "binary for comprehensions with literal matches" do
    # Integers
    bin = <<1, 2, 1, 3, 1, 4>>
    assert for(<<1, x <- bin>>, into: "", do: to_bin(x)) == <<2, 3, 4>>
    assert for(<<1, x <- bin>>, into: %{}, do: {x, x}) == %{2 => 2, 3 => 3, 4 => 4}

    bin = <<1, 2, 3, 1, 4>>
    assert for(<<1, x <- bin>>, into: "", do: to_bin(x)) == <<2>>
    assert for(<<1, x <- bin>>, into: %{}, do: {x, x}) == %{2 => 2}

    # Floats
    bin = <<1.0, 2, 1.0, 3, 1.0, 4>>
    assert for(<<1.0, x <- bin>>, into: "", do: to_bin(x)) == <<2, 3, 4>>
    assert for(<<1.0, x <- bin>>, into: %{}, do: {x, x}) == %{2 => 2, 3 => 3, 4 => 4}

    bin = <<1.0, 2, 3, 1.0, 4>>
    assert for(<<1.0, x <- bin>>, into: "", do: to_bin(x)) == <<2>>
    assert for(<<1.0, x <- bin>>, into: %{}, do: {x, x}) == %{2 => 2}

    # Binaries
    bin = <<"foo", 2, "foo", 3, "foo", 4>>
    assert for(<<"foo", x <- bin>>, into: "", do: to_bin(x)) == <<2, 3, 4>>
    assert for(<<"foo", x <- bin>>, into: %{}, do: {x, x}) == %{2 => 2, 3 => 3, 4 => 4}

    bin = <<"foo", 2, 3, "foo", 4>>
    assert for(<<"foo", x <- bin>>, into: "", do: to_bin(x)) == <<2>>
    assert for(<<"foo", x <- bin>>, into: %{}, do: {x, x}) == %{2 => 2}

    bin = <<"foo", 2, 3, 4, "foo", 5>>
    assert for(<<"foo", x <- bin>>, into: "", do: to_bin(x)) == <<2>>
    assert for(<<"foo", x <- bin>>, into: %{}, do: {x, x}) == %{2 => 2}
  end

  test "binary for comprehensions with variable size" do
    s = 16
    bin = <<1, 2, 3, 4, 5, 6>>
    assert for(<<x::size(s) <- bin>>, into: "", do: to_bin(div(x, 2))) == <<129, 130, 131>>

    # Aligned
    bin = <<8, 1, 16, 2, 3>>
    assert for(<<s, x::size(s) <- bin>>, into: "", do: <<x::size(s)>>) == <<1, 2, 3>>
    assert for(<<s, x::size(s) <- bin>>, into: %{}, do: {s, x}) == %{8 => 1, 16 => 515}

    # Unaligned
    bin = <<8, 1, 32, 2, 3>>
    assert for(<<s, x::size(s) <- bin>>, into: "", do: <<x::size(s)>>) == <<1>>
    assert for(<<s, x::size(s) <- bin>>, into: %{}, do: {s, x}) == %{8 => 1}
  end

  test "binary for comprehensions where value is not used" do
    bin = <<1, 2, 3>>

    assert capture_io(fn ->
             for <<x <- bin>>, do: IO.puts(x)
             nil
           end) == "1\n2\n3\n"
  end

  test "binary for comprehensions with reduce, generators and filters" do
    bin = <<1, 2, 3>>

    acc =
      for <<x <- bin>>, Integer.is_odd(x), <<y <- "hello">>, reduce: %{} do
        acc -> Map.update(acc, x, [y], &[y | &1])
      end

    assert acc == %{1 => 'olleh', 3 => 'olleh'}
  end
end
