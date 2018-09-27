Code.require_file("../test_helper.exs", __DIR__)

defmodule Module.LocalsTrackerTest do
  use ExUnit.Case, async: true

  alias Module.LocalsTracker, as: D

  setup do
    set = :ets.new(__MODULE__, [:set, :public])
    bag = :ets.new(__MODULE__, [:duplicate_bag, :public])
    [ref: {set, bag}]
  end

  ## Locals

  test "functions are reachable when connected through another one", config do
    D.add_local(config[:ref], {:public, 1}, {:private, 1}, line: 1)
    assert {:private, 1} in D.reachable_from(config[:ref], {:public, 1})
  end

  test "can yank and reattach nodes", config do
    D.add_local(config[:ref], {:foo, 1}, {:bar, 1}, line: 1)

    outfoo = D.yank(config[:ref], {:foo, 1})
    outbar = D.yank(config[:ref], {:bar, 1})

    D.reattach(config[:ref], {:bar, 1}, :defp, {:bar, 1}, outbar, line: 2)
    D.reattach(config[:ref], {:foo, 1}, :def, {:foo, 1}, outfoo, line: 3)
    assert {:bar, 1} in D.reachable_from(config[:ref], {:foo, 1})
  end

  @used [
    {{:public, 1}, :def, [], 0}
  ]

  test "unused private definitions are marked as so", config do
    D.add_local(config[:ref], {:public, 1}, {:private, 1}, line: 1)

    unused = D.collect_unused_locals(config[:ref], @used, [{{:private, 0}, :defp, [], 0}])
    assert unused == {[private: 0], [{[], {:unused_def, {:private, 0}, :defp}}]}

    unused = D.collect_unused_locals(config[:ref], @used, [{{:private, 1}, :defp, [], 0}])
    assert unused == {[], []}
  end

  @unused [
    {{:private, 3}, :defp, [], 3}
  ]

  test "private definitions with unused default arguments", config do
    unused = D.collect_unused_locals(config[:ref], @used, @unused)
    assert unused == {[private: 3], [{[], {:unused_def, {:private, 3}, :defp}}]}

    D.add_local(config[:ref], {:public, 1}, {:private, 3}, line: 1)
    unused = D.collect_unused_locals(config[:ref], @used, @unused)
    assert unused == {[], [{[], {:unused_args, {:private, 3}}}]}
  end

  test "private definitions with some unused default arguments", config do
    D.add_local(config[:ref], {:public, 1}, {:private, 1}, line: 1)
    unused = D.collect_unused_locals(config[:ref], @used, @unused)
    assert unused == {[private: 3], [{[], {:unused_args, {:private, 3}, 1}}]}
  end

  test "private definitions with all used default arguments", config do
    D.add_local(config[:ref], {:public, 1}, {:private, 0}, line: 1)
    unused = D.collect_unused_locals(config[:ref], @used, @unused)
    assert unused == {[private: 3], []}
  end

  ### Undefined functions

  test "undefined functions are marked as so", config do
    D.add_local(config[:ref], {:public, 1}, {:private, 1}, line: 1)

    undefined = D.collect_undefined_locals(config[:ref], @used)
    assert undefined == [{[line: 1], {:undefined_function, {:private, 1}}}]
  end

  ## Defaults

  test "defaults are connected to last clause only", config do
    D.add_defaults(config[:ref], :defp, {:foo, 4}, 2, line: 1)
    D.add_local(config[:ref], {:public, 1}, {:foo, 2}, line: 2)
    assert {:foo, 2} in D.reachable_from(config[:ref], {:public, 1})
    refute {:foo, 3} in D.reachable_from(config[:ref], {:public, 1})
    assert {:foo, 4} in D.reachable_from(config[:ref], {:public, 1})
  end

  ## Imports

  test "find import conflicts", config do
    entries = [{{:conflict, 1}, :def, [], []}]
    refute {[], {Module, {:conflict, 1}}} in D.collect_imports_conflicts(config[:ref], entries)

    D.add_local(config[:ref], {:public, 1}, {:foo, 2}, line: 1)
    D.add_import(config[:ref], {:foo, 2}, Module, {:conflict, 1})
    D.add_import(config[:ref], {:foo, 2}, Module, {:conflict, 1})
    assert {[], {Module, {:conflict, 1}}} in D.collect_imports_conflicts(config[:ref], entries)
  end

  defmodule NoPrivate do
    defmacrop foo(), do: bar()
    defp bar(), do: :baz
    def baz(), do: foo()
  end

  test "does not include unreachable locals" do
    assert NoPrivate.module_info(:functions) ==
             [__info__: 1, baz: 0, module_info: 0, module_info: 1]
  end
end
