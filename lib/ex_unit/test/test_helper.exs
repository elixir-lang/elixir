# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

{line_exclude, line_include} =
  if line = System.get_env("LINE"), do: {[:test], [line: line]}, else: {[], []}

Code.require_file("../../elixir/scripts/cover_record.exs", __DIR__)
CoverageRecorder.maybe_record("ex_unit")

ExUnit.start(
  trace: !!System.get_env("TRACE"),
  include: line_include,
  exclude: line_exclude,
  assert_receive_timeout: String.to_integer(System.get_env("ELIXIR_ASSERT_TIMEOUT", "300"))
)
