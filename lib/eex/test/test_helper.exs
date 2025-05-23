# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

{line_exclude, line_include} =
  if line = System.get_env("LINE"), do: {[:test], [line: line]}, else: {[], []}

Code.require_file("../../elixir/scripts/cover_record.exs", __DIR__)
CoverageRecorder.maybe_record("eex")

ExUnit.start(
  trace: !!System.get_env("TRACE"),
  include: line_include,
  exclude: line_exclude
)
