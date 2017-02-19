Code.require_file "../test_helper.exs", __DIR__

defmodule Module.LocalsTrackerTest do
  use ExUnit.Case, async: true

  alias Module.LocalsTracker, as: D

  setup do
    {:ok, pid} = D.start_link
    {:ok, [pid: pid]}
  end

  ## Locals

  test "can add definitions", config do
    D.add_definition(config[:pid], :def, {:foo, 1})
    D.add_definition(config[:pid], :defp, {:bar, 1})
  end

  test "can add locals", config do
    D.add_definition(config[:pid], :def, {:foo, 1})
    D.add_local(config[:pid], {:foo, 1}, {:bar, 1})
  end

  test "public definitions are always reachable", config do
    D.add_definition(config[:pid], :def, {:public, 1})
    assert {:public, 1} in D.reachable(config[:pid])

    D.add_definition(config[:pid], :defmacro, {:public, 2})
    assert {:public, 2} in D.reachable(config[:pid])
  end

  test "private definitions are never reachable", config do
    D.add_definition(config[:pid], :defp, {:private, 1})
    refute {:private, 1} in D.reachable(config[:pid])

    D.add_definition(config[:pid], :defmacrop, {:private, 2})
    refute {:private, 2} in D.reachable(config[:pid])
  end

  test "private definitions are reachable when connected to local", config do
    D.add_definition(config[:pid], :defp, {:private, 1})
    refute {:private, 1} in D.reachable(config[:pid])

    D.add_local(config[:pid], {:private, 1})
    assert {:private, 1} in D.reachable(config[:pid])
  end

  test "private definitions are reachable when connected through a public one", config do
    D.add_definition(config[:pid], :defp, {:private, 1})
    refute {:private, 1} in D.reachable(config[:pid])

    D.add_definition(config[:pid], :def, {:public, 1})
    D.add_local(config[:pid], {:public, 1}, {:private, 1})
    assert {:private, 1} in D.reachable(config[:pid])
  end

  test "can yank and reattach nodes", config do
    D.add_definition(config[:pid], :def, {:foo, 1})
    D.add_local(config[:pid], {:foo, 1}, {:bar, 1})
    D.add_definition(config[:pid], :defp, {:bar, 1})

    {infoo, outfoo}   = D.yank(config[:pid], {:foo, 1})
    {inbar, outbar} = D.yank(config[:pid], {:bar, 1})

    D.reattach(config[:pid], :defp, {:bar, 1}, {inbar, outbar})
    D.reattach(config[:pid], :def, {:foo, 1}, {infoo, outfoo})
    assert {:bar, 1} in D.reachable(config[:pid])
  end

  @unused [
    {{:private, 1}, :defp, [], 0}
  ]

  test "unused private definitions are marked as so", config do
    D.add_definition(config[:pid], :def, {:public, 1})

    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == {[private: 1], [{[], {:unused_def, {:private, 1}, :defp}}]}

    D.add_local(config[:pid], {:public, 1}, {:private, 1})
    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == {[], []}
  end

  @unused [
    {{:private, 3}, :defp, [], 3}
  ]

  test "private definitions with unused default arguments", config do
    D.add_definition(config[:pid], :def, {:public, 1})

    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == {[private: 3], [{[], {:unused_def, {:private, 3}, :defp}}]}

    D.add_local(config[:pid], {:public, 1}, {:private, 3})
    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == {[], [{[], {:unused_args, {:private, 3}}}]}
  end

  test "private definitions with some unused default arguments", config do
    D.add_definition(config[:pid], :def, {:public, 1})
    D.add_local(config[:pid], {:public, 1}, {:private, 1})
    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == {[private: 3], [{[], {:unused_args, {:private, 3}, 1}}]}
  end

  test "private definitions with all used default arguments", config do
    D.add_definition(config[:pid], :def, {:public, 1})
    D.add_local(config[:pid], {:public, 1}, {:private, 0})
    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == {[private: 3], []}
  end

  ## Defaults

  test "can add defaults", config do
    D.add_definition(config[:pid], :def, {:foo, 4})
    D.add_defaults(config[:pid], :def, {:foo, 4}, 2)
  end

  test "defaults are reachable if public", config do
    D.add_definition(config[:pid], :def, {:foo, 4})
    D.add_defaults(config[:pid], :def, {:foo, 4}, 2)
    assert {:foo, 2} in D.reachable(config[:pid])
    assert {:foo, 3} in D.reachable(config[:pid])
  end

  test "defaults are not reachable if private", config do
    D.add_definition(config[:pid], :defp, {:foo, 4})
    D.add_defaults(config[:pid], :defp, {:foo, 4}, 2)
    refute {:foo, 2} in D.reachable(config[:pid])
    refute {:foo, 3} in D.reachable(config[:pid])
  end

  test "defaults are connected to last clause only", config do
    D.add_definition(config[:pid], :defp, {:foo, 4})
    D.add_defaults(config[:pid], :defp, {:foo, 4}, 2)
    D.add_local(config[:pid], {:foo, 2})
    assert {:foo, 2} in D.reachable(config[:pid])
    refute {:foo, 3} in D.reachable(config[:pid])
    assert {:foo, 4} in D.reachable(config[:pid])
  end

  ## Imports

  test "find imports from dispatch", config do
    D.add_import(config[:pid], nil, Module, {:concat, 1})
    assert Module in D.imports_with_dispatch(config[:pid], {:concat, 1})
    refute Module in D.imports_with_dispatch(config[:pid], {:unknown, 1})
  end

  test "find import conflicts", config do
    entries = [{{:conflict, 1}, :def, [], []}]

    refute {[], {[Module], :conflict, 1}} in D.collect_imports_conflicts(config[:pid], entries)

    # Calls outside local functions are not triggered
    D.add_import(config[:pid], nil, Module, {:conflict, 1})
    refute {[], {[Module], :conflict, 1}} in D.collect_imports_conflicts(config[:pid], entries)

    D.add_local(config[:pid], {:foo, 2})
    D.add_import(config[:pid], {:foo, 2}, Module, {:conflict, 1})
    assert {[], {[Module], :conflict, 1}} in D.collect_imports_conflicts(config[:pid], entries)
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
