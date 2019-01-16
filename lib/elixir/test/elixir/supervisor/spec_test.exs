Code.require_file("../test_helper.exs", __DIR__)

defmodule Supervisor.SpecTest do
  use ExUnit.Case, async: true

  import Supervisor.Spec

  test "worker/3" do
    assert worker(Foo, [1, 2, 3]) ==
             {Foo, {Foo, :start_link, [1, 2, 3]}, :permanent, 5000, :worker, [Foo]}

    opts = [
      id: :sample,
      function: :start,
      modules: :dynamic,
      restart: :temporary,
      shutdown: :brutal_kill
    ]

    assert worker(Foo, [1, 2, 3], opts) ==
             {:sample, {Foo, :start, [1, 2, 3]}, :temporary, :brutal_kill, :worker, :dynamic}
  end

  test "worker/3 with GenEvent" do
    mfa = {GenEvent, :start_link, [[name: :hello]]}

    assert worker(GenEvent, [[name: :hello]]) ==
             {GenEvent, mfa, :permanent, 5000, :worker, :dynamic}
  end

  test "supervisor/3" do
    assert supervisor(Foo, [1, 2, 3]) ==
             {Foo, {Foo, :start_link, [1, 2, 3]}, :permanent, :infinity, :supervisor, [Foo]}

    opts = [
      id: :sample,
      function: :start,
      modules: :dynamic,
      restart: :temporary,
      shutdown: :brutal_kill
    ]

    assert supervisor(Foo, [1, 2, 3], opts) ==
             {:sample, {Foo, :start, [1, 2, 3]}, :temporary, :brutal_kill, :supervisor, :dynamic}
  end

  test "supervise/2" do
    assert supervise([], strategy: :one_for_one) == {:ok, {{:one_for_one, 3, 5}, []}}

    children = [worker(GenEvent, [])]
    options = [strategy: :one_for_all, max_restarts: 1, max_seconds: 1]
    assert supervise(children, options) == {:ok, {{:one_for_all, 1, 1}, children}}
  end

  test "supervise/2 with duplicated IDs" do
    children = [worker(GenEvent, []), worker(GenEvent, [])]

    assert_raise ArgumentError, fn ->
      supervise(children, strategy: :one_for_one)
    end
  end
end
