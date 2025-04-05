# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Application.put_env(:elixir, :ansi_enabled, true)
Code.compiler_options(debug_info: true, infer_signatures: [:elixir])

Code.eval_file("../../scripts/path_helpers.exs", __DIR__)

defmodule CodeFormatterHelpers do
  defmacro assert_same(good, opts \\ []) do
    quote bind_quoted: [good: good, opts: opts] do
      assert IO.iodata_to_binary(Code.format_string!(good, opts)) == String.trim(good)
    end
  end

  defmacro assert_format(bad, good, opts \\ []) do
    quote bind_quoted: [bad: bad, good: good, opts: opts] do
      result = String.trim(good)
      assert IO.iodata_to_binary(Code.format_string!(bad, opts)) == result
      assert IO.iodata_to_binary(Code.format_string!(good, opts)) == result
    end
  end
end

assert_timeout = String.to_integer(System.get_env("ELIXIR_ASSERT_TIMEOUT") || "500")
epmd_exclude = if match?({:win32, _}, :os.type()), do: [epmd: true], else: []
os_exclude = if PathHelpers.windows?(), do: [unix: true], else: [windows: true]

{line_exclude, line_include} =
  if line = System.get_env("LINE"), do: {[:test], [line: line]}, else: {[], []}

distributed_exclude =
  if Code.ensure_loaded?(:peer) and Node.alive?() do
    {:ok, _pid, node} = :peer.start(%{name: :secondary})
    true = :erpc.call(node, :code, :set_path, [:code.get_path()])
    {:ok, _} = :erpc.call(node, :application, :ensure_all_started, [:elixir])
    []
  else
    [distributed: true]
  end

source_exclude =
  if :deterministic in :compile.env_compiler_options() do
    [:requires_source]
  else
    []
  end

ExUnit.start(
  trace: !!System.get_env("TRACE"),
  assert_receive_timeout: assert_timeout,
  exclude: epmd_exclude ++ os_exclude ++ line_exclude ++ distributed_exclude ++ source_exclude,
  include: line_include
)
