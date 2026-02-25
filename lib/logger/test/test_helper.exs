# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

{line_exclude, line_include} =
  if line = System.get_env("LINE"), do: {[:test], [line: line]}, else: {[], []}

Code.require_file("../../elixir/scripts/cover_record.exs", __DIR__)
CoverageRecorder.maybe_record("logger")

maybe_seed_opt = if seed = System.get_env("SEED"), do: [seed: String.to_integer(seed)], else: []

ex_unit_opts =
  [
    trace: !!System.get_env("TRACE"),
    include: line_include,
    exclude: line_exclude,
    assert_receive_timeout: String.to_integer(System.get_env("ELIXIR_ASSERT_TIMEOUT", "300"))
  ] ++ maybe_seed_opt

ExUnit.start(ex_unit_opts)

defmodule Logger.Case do
  use ExUnit.CaseTemplate
  import ExUnit.CaptureIO

  using _ do
    quote do
      import Logger.Case
    end
  end

  def msg(msg) do
    ~r/\d\d\:\d\d\:\d\d\.\d\d\d #{Regex.escape(msg)}/
  end

  def wait_for_handler(manager, handler) do
    if handler not in :gen_event.which_handlers(manager) do
      Process.sleep(10)
      wait_for_handler(manager, handler)
    end
  end

  def wait_for_logger() do
    try do
      :gen_event.which_handlers(Logger)
    catch
      :exit, _ ->
        Process.sleep(10)
        wait_for_logger()
    else
      _ ->
        :ok
    end
  end

  def capture_log(level \\ :debug, fun) do
    Logger.configure(level: level)

    capture_io(:user, fn ->
      fun.()
      Logger.flush()
    end)
  after
    Logger.configure(level: :debug)
  end
end
