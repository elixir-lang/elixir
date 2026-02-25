# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

{line_exclude, line_include} =
  if line = System.get_env("LINE"), do: {[:test], [line: line]}, else: {[], []}

Code.require_file("../../elixir/scripts/cover_record.exs", __DIR__)
CoverageRecorder.maybe_record("ex_unit")

maybe_seed_opt = if seed = System.get_env("SEED"), do: [seed: String.to_integer(seed)], else: []

ex_unit_opts =
  [
    trace: !!System.get_env("TRACE"),
    include: line_include,
    exclude: line_exclude,
    assert_receive_timeout: String.to_integer(System.get_env("ELIXIR_ASSERT_TIMEOUT", "300"))
  ] ++ maybe_seed_opt

ExUnit.start(ex_unit_opts)
