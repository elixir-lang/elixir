# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

ExUnit.start()

defmodule ChangelogTest do
  use ExUnit.Case, async: true
  doctest_file(Path.expand("../../../../CHANGELOG.md", __DIR__))
end
