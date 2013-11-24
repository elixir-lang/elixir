Code.require_file "../test_helper.exs", __DIR__

# This tests the proptests by running very simple proptests and making sure the
# right result comes out.
defmodule ExUnit.PropTest do
  import ExUnit.Prop

  use ExUnit.Case, async: true

  test "tautologies pass" do
    prop (fn -> true end), &(assert &1)
    prop (fn -> true end), &(assert &1 == true)
  end

  test "BadGenerator is raised if the generator keeps discarding" do
    assert_raise ExUnit.Prop.BadGenerator, 
      "Could not generate input value in 100 attempts",
      fn ->
        prop [max_discards: 100], fn -> discard end, fn _ -> :ok end
      end
  end

  test "if the seed is set random is deterministic" do
    prop [max_successes: 1, initial_seed: { 1, 2, 3 }],
         fn -> :random.uniform(0xFFFFFFFF) end,
         fn x -> assert x == 217968216 end
  end

  test "shrink reduces the failure input, linearly" do
    try do
      prop { fn -> [1, 2, 3, 4, 5] end, &[tl(&1)] },
           fn x -> assert x == [4, 5] end
      flunk
    catch
      :error, err = ExUnit.Prop.PropertyError[] ->
        assert err.input == [3, 4, 5]
    end
  end
  
  test "shrink reduces the failure input, non-linearly, in the correct order" do
    Process.put(:inputs, [])
    try do
      prop { fn -> [1, [[2, 3], [4, 5]], [6, 7]] end,
             fn i when is_list i -> i; _ -> [] end },
           fn x ->
             Process.put(:inputs, [x | Process.get(:inputs)])
             assert x == 1 or x == [4, 5]
           end
      flunk
    catch
      :error, err = ExUnit.Prop.PropertyError[] ->
        assert err.input == 2 
    end
    inputs = Process.get(:inputs) |> Enum.reverse()
    assert inputs ==
      [ [1, [[2, 3], [4, 5]], [6, 7]],
        1,
        [[2, 3], [4, 5]],
        [2, 3],
        2
      ]
  end

  test "get_size and with_size" do
    prop [max_size: 42], 
         fn ->
           assert get_size() == 42
           with_size(43, fn ->
             assert get_size() == 43
             with_size(&(&1*2), fn ->
               assert get_size() == 86
             end)
           end)
         end,
         fn _ -> :ok end
  end

  test "satisfy filters out invalid values" do
    prop fn -> x = :random.uniform(5); satisfy (x <= 3); x end,
         fn x -> assert x in [1, 2, 3] end
  end

  test "one_of generates only values in the collection" do
    prop fn -> one_of [1,2,3] end, fn x -> assert x in [1, 2, 3] end
    prop fn -> one_of 1..3 end, fn x -> assert x in [1, 2, 3] end
  end
  
  test "one_of raises exception on empty collection" do
    assert_raise ArgumentError, 
      "could not convert collection to nonempty list: []",
      fn ->
        prop fn -> one_of [] end, fn _ -> :ok end
      end
    assert_raise ArgumentError, 
      "could not convert collection to nonempty list: #HashDict<[]>",
      fn ->
        prop fn -> one_of HashDict.new end, fn _ -> :ok end
      end
  end
  
  test "list_of generates 0 upto size elements" do
    prop [max_size: 5],
         fn -> list_of(fn -> true end) end,
         fn l -> assert length(l) <= 5 end
  end
  
  test "list_of generates 1 upto size elements" do
    prop [max_size: 5],
         fn -> list_of1(fn -> true end) end,
         fn l -> assert length(l) > 0 and length(l) <= 5 end
  end
end
