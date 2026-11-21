# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("../test_helper.exs", __DIR__)

defmodule Module.ParallelCheckerTest do
  use ExUnit.Case, async: true

  alias Module.ParallelChecker

  test "stops pending workers when compilation aborts" do
    module_map = %{
      module: __MODULE__,
      file: __ENV__.file,
      anno: __ENV__.line,
      attributes: [],
      compile_opts: [],
      definitions: [],
      defines_behaviour: false,
      deprecated: [],
      impls: [],
      after_verify: []
    }

    ref =
      catch_throw(
        ParallelChecker.verify(fn ->
          {_, {checker, _}} = info = ParallelChecker.get()
          ParallelChecker.spawn(info, __MODULE__, module_map, %{}, [], false)
          %{checkers: [{__MODULE__, worker, _}]} = :sys.get_state(checker)
          throw(Process.monitor(worker))
        end)
      )

    assert_receive {:DOWN, ^ref, :process, _, :normal}
  end
end
