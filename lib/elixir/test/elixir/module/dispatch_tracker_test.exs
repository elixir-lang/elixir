Code.require_file "../test_helper.exs", __DIR__

defmodule Module.DispatchTrackerTest do
  use ExUnit.Case, async: true

  alias Module.DispatchTracker, as: D

  setup do
    { :ok, [pid: D.start_link] }
  end

  teardown config do
    D.stop(config[:pid])
    :ok
  end

  ## Locals

  test "can add definitions", config do
    D.add_definition(config[:pid], :def, { :foo, 1 })
    D.add_definition(config[:pid], :defp, { :bar, 1 })
  end

  test "can add locals", config do
    D.add_definition(config[:pid], :def, { :foo, 1 })
    D.add_local(config[:pid], { :foo, 1 }, { :bar, 1 })
  end

  test "public definitions are always reachable", config do
    D.add_definition(config[:pid], :def, { :public, 1 })
    assert { :public, 1 } in D.reachable(config[:pid])

    D.add_definition(config[:pid], :defmacro, { :public, 2 })
    assert { :public, 2 } in D.reachable(config[:pid])
  end

  test "private definitions are never reachable", config do
    D.add_definition(config[:pid], :defp, { :private, 1 })
    refute { :private, 1 } in D.reachable(config[:pid])

    D.add_definition(config[:pid], :defmacrop, { :private, 2 })
    refute { :private, 2 } in D.reachable(config[:pid])
  end

  test "private definitions are reachable when connected to local", config do
    D.add_definition(config[:pid], :defp, { :private, 1 })
    refute { :private, 1 } in D.reachable(config[:pid])

    D.add_local(config[:pid], { :private, 1 })
    assert { :private, 1 } in D.reachable(config[:pid])
  end

  test "private definitions are reachable when connected through a public one", config do
    D.add_definition(config[:pid], :defp, { :private, 1 })
    refute { :private, 1 } in D.reachable(config[:pid])

    D.add_definition(config[:pid], :def, { :public, 1 })
    D.add_local(config[:pid], { :public, 1 }, { :private, 1 })
    assert { :private, 1 } in D.reachable(config[:pid])
  end

  @unused [
    { { :private, 1 }, :defp, 0 }
  ]

  test "unused private definitions are marked as so", config do
    D.add_definition(config[:pid], :def, { :public, 1 })

    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == [{ :unused_def, { :private, 1 }, :defp }]

    D.add_local(config[:pid], { :public, 1 }, { :private, 1 })
    unused = D.collect_unused_locals(config[:pid], @unused)
    refute unused == [{ :unused_def, { :private, 1 }, :defp }]
  end

  @unused [
    { { :private, 3 }, :defp, 3 }
  ]

  test "private definitions with unused default arguments", config do
    D.add_definition(config[:pid], :def, { :public, 1 })

    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == [{ :unused_def, { :private, 3 }, :defp }]

    D.add_local(config[:pid], { :public, 1 }, { :private, 3 })
    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == [{ :unused_args, { :private, 3 }}]
  end

  test "private definitions with some unused default arguments", config do
    D.add_definition(config[:pid], :def, { :public, 1 })
    D.add_local(config[:pid], { :public, 1 }, { :private, 1 })
    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == [{ :unused_args, { :private, 3 }, 1}]
  end

  test "private definitions with all used default arguments", config do
    D.add_definition(config[:pid], :def, { :public, 1 })
    D.add_local(config[:pid], { :public, 1 }, { :private, 0 })
    unused = D.collect_unused_locals(config[:pid], @unused)
    assert unused == []
  end

  ## Defaults

  test "can add defaults", config do
    D.add_definition(config[:pid], :def, { :foo, 4 })
    D.add_defaults(config[:pid], :def, { :foo, 4 }, 2)
  end

  test "defaults are reachable if public", config do
    D.add_definition(config[:pid], :def, { :foo, 4 })
    D.add_defaults(config[:pid], :def, { :foo, 4 }, 2)
    assert { :foo, 2 } in D.reachable(config[:pid])
    assert { :foo, 3 } in D.reachable(config[:pid])
  end

  test "defaults are not reachable if private", config do
    D.add_definition(config[:pid], :defp, { :foo, 4 })
    D.add_defaults(config[:pid], :defp, { :foo, 4 }, 2)
    refute { :foo, 2 } in D.reachable(config[:pid])
    refute { :foo, 3 } in D.reachable(config[:pid])
  end

  test "defaults are connected", config do
    D.add_definition(config[:pid], :defp, { :foo, 4 })
    D.add_defaults(config[:pid], :defp, { :foo, 4 }, 2)
    D.add_local(config[:pid], { :foo, 2 })
    assert { :foo, 2 } in D.reachable(config[:pid])
    assert { :foo, 3 } in D.reachable(config[:pid])
    assert { :foo, 4 } in D.reachable(config[:pid])
  end

  ## Aliases

  test "can add alias", config do
    D.add_alias(config[:pid], Module)
  end

  test "can retrieve aliases", config do
    D.add_alias(config[:pid], Module)
    assert Module in D.aliases(config[:pid])
  end

  ## Imports

  test "can add import", config do
    D.add_import(config[:pid], nil, Module, { :concat, 1 })
  end

  test "can retrieve imports", config do
    D.add_import(config[:pid], nil, Module, { :concat, 1 })
    assert Module in D.imports(config[:pid])
  end

  test "find imports from dispatch", config do
    D.add_import(config[:pid], nil, Module, { :concat, 1 })
    assert Module in D.imports_with_dispatch(config[:pid], { :concat, 1 })
    refute Module in D.imports_with_dispatch(config[:pid], { :unknown, 1 })
  end

  test "can add warnable", config do
    D.add_warnable(config[:pid], Module, true, 15)
    assert Module in D.imports(config[:pid])
  end

  test "find unused imports", config do
    D.add_warnable(config[:pid], Module, true, 15)
    assert { Module, 15 } in D.collect_unused_imports(config[:pid])

    D.add_warnable(config[:pid], Module, false, 15)
    refute { Module, 15 } in D.collect_unused_imports(config[:pid])
  end

  test "find import conflicts", config do
    refute { [Module], :conflict, 1 } in D.collect_imports_conflicts(config[:pid], [conflict: 1])

    D.add_import(config[:pid], nil, Module, { :conflict, 1 })
    assert { [Module], :conflict, 1 } in D.collect_imports_conflicts(config[:pid], [conflict: 1])
  end

  ## Remotes

  test "can add remote", config do
    D.add_remote(config[:pid], nil, Module, { :concat, 1 })
  end

  test "can retrieve remotes", config do
    D.add_remote(config[:pid], nil, Module, { :concat, 1 })
    assert Module in D.remotes(config[:pid])
  end

  test "find remotes from dispatch", config do
    D.add_remote(config[:pid], nil, Module, { :concat, 1 })
    assert Module in D.remotes_with_dispatch(config[:pid], { :concat, 1 })
    refute Module in D.remotes_with_dispatch(config[:pid], { :unknown, 1 })
  end

  test "remotes are not reachable", config do
    D.add_definition(config[:pid], :def, { :public, 1 })
    D.add_remote(config[:pid], { :public, 1 }, Module, { :concat, 1 })
    assert D.reachable(config[:pid]) == [{ :public, 1 }]
  end

  ## All

  test "can query dispatches", config do
    D.add_definition(config[:pid], :def, { :public, 1 })
    D.add_definition(config[:pid], :def, { :caller, 1 })
    D.add_local(config[:pid], { :caller, 1 }, { :public, 1 })
    D.add_remote(config[:pid], { :public, 1 }, Module, { :concat, 1 })
    D.add_import(config[:pid], { :public, 1 }, Kernel, { :if, 2 })

    assert D.dispatches_to(config[:pid], { :public, 1 }) == [{ :caller, 1 }]
    assert D.dispatches_to(config[:pid], { :import, :if, 2 }) == [{ :public, 1 }]
    assert D.dispatches_to(config[:pid], { :remote, :concat, 1 }) == [{ :public, 1 }]
    assert D.dispatches_to(config[:pid], Module) == [{ :remote, :concat, 1 }]
    assert D.dispatches_to(config[:pid], Kernel) == [{ :import, :if, 2 }]

    assert { :public, 1 } in D.dispatches_from(config[:pid], { :caller, 1 })
    assert { :remote, :concat, 1 } in D.dispatches_from(config[:pid], { :public, 1 })
    assert { :import, :if, 2 } in D.dispatches_from(config[:pid], { :public, 1 })
  end
end
