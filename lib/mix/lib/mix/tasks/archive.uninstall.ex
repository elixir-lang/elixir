# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Archive.Uninstall do
  use Mix.Task

  @shortdoc "Uninstalls archives"

  @moduledoc """
  Uninstalls local archives.

      $ mix archive.uninstall archive.ez

  ## Command line options
    * `--force` - forces uninstallation without a shell prompt; primarily
      intended for automation
  """

  @switches [
    force: :boolean
  ]

  @impl true
  def run(argv) do
    Mix.Local.Installer.uninstall(Mix.path_for(:archives), "archive", argv, @switches)
  end
end
