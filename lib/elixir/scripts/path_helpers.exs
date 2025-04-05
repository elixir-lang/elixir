# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

# Beam files compiled on demand
path = Path.expand("../tmp/beams", __DIR__)
File.rm_rf!(path)
File.mkdir_p!(path)
Code.prepend_path(path)

defmodule PathHelpers do
  def fixture_path() do
    Path.expand("../test/elixir/fixtures", __DIR__)
  end

  def tmp_path() do
    Path.expand("../tmp", __DIR__)
  end

  def fixture_path(extra) do
    Path.join(fixture_path(), extra)
  end

  def tmp_path(extra) do
    Path.join(tmp_path(), extra)
  end

  def elixir(args, executable_extension \\ "") do
    run_cmd(elixir_executable(executable_extension), args)
  end

  def elixir_executable(extension \\ "") do
    executable_path("elixir", extension)
  end

  def elixirc(args, executable_extension \\ "") do
    run_cmd(elixirc_executable(executable_extension), args)
  end

  def elixirc_executable(extension \\ "") do
    executable_path("elixirc", extension)
  end

  def iex(args, executable_extension \\ "") do
    run_cmd(iex_executable(executable_extension), args)
  end

  def iex_executable(extension \\ "") do
    executable_path("iex", extension)
  end

  def write_beam({:module, name, bin, _} = res) do
    File.mkdir_p!(unquote(path))
    beam_path = Path.join(unquote(path), Atom.to_string(name) <> ".beam")
    File.write!(beam_path, bin)
    res
  end

  defp run_cmd(executable, args) do
    ~c"#{executable} #{IO.chardata_to_string(args)}#{redirect_std_err_on_win()}"
    |> :os.cmd()
    |> :unicode.characters_to_binary()
  end

  defp executable_path(name, extension) do
    Path.expand("../../../bin/#{name}#{extension}", __DIR__)
  end

  if match?({:win32, _}, :os.type()) do
    def windows?, do: true
    def executable_extension, do: ".bat"
    def redirect_std_err_on_win, do: " 2>&1"
  else
    def windows?, do: false
    def executable_extension, do: ""
    def redirect_std_err_on_win, do: ""
  end
end
