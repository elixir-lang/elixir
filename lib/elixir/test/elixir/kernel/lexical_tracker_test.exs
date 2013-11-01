Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.LexicalTrackerTest do
  use ExUnit.Case, async: true

  alias Kernel.LexicalTracker, as: D

  setup do
    { :ok, [pid: D.start_link] }
  end

  teardown config do
    D.stop(config[:pid])
    :ok
  end

  test "can add remote dispatches", config do
    D.remote_dispatch(config[:pid], String)
    assert D.remotes(config[:pid]) == [String]
  end

  test "can add imports", config do
    D.add_import(config[:pid], String, 1, true)
    assert D.remotes(config[:pid]) == [String]
  end

  test "can add aliases", config do
    D.add_alias(config[:pid], String, 1, true)
    assert D.remotes(config[:pid]) == [String]
  end

  test "unused imports", config do
    D.add_import(config[:pid], String, 1, true)
    assert D.collect_unused_imports(config[:pid]) == [{String,1}]
  end

  test "used imports are not unused", config do
    D.add_import(config[:pid], String, 1, true)
    D.import_dispatch(config[:pid], String)
    assert D.collect_unused_imports(config[:pid]) == []
  end

  test "imports with no warn are not unused", config do
    D.add_import(config[:pid], String, 1, false)
    assert D.collect_unused_imports(config[:pid]) == []
  end

  test "unused aliases", config do
    D.add_alias(config[:pid], String, 1, true)
    assert D.collect_unused_aliases(config[:pid]) == [{String,1}]
  end

  test "used aliases are not unused", config do
    D.add_alias(config[:pid], String, 1, true)
    D.alias_dispatch(config[:pid], String)
    assert D.collect_unused_aliases(config[:pid]) == []
  end

  test "aliases with no warn are not unused", config do
    D.add_alias(config[:pid], String, 1, false)
    assert D.collect_unused_aliases(config[:pid]) == []
  end
end
