# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

# Beam files compiled on demand
path = Path.expand("../../tmp/beams", __DIR__)
File.rm_rf!(path)
File.mkdir_p!(path)
Code.prepend_path(path)

Application.put_env(:elixir, :ansi_enabled, true)
Code.compiler_options(debug_info: true, infer_signatures: [:elixir])

defmodule PathHelpers do
  def fixture_path() do
    Path.expand("fixtures", __DIR__)
  end

  def tmp_path() do
    Path.expand("../../tmp", __DIR__)
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

    :code.purge(name)
    :code.delete(name)

    res
  end

  defp run_cmd(executable, args) do
    ~c"#{executable} #{IO.chardata_to_string(args)}#{redirect_std_err_on_win()}"
    |> :os.cmd()
    |> :unicode.characters_to_binary()
  end

  defp executable_path(name, extension) do
    Path.expand("../../../../bin/#{name}#{extension}", __DIR__)
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

Code.require_file("../../scripts/cover_record.exs", __DIR__)

cover_exclude =
  if CoverageRecorder.maybe_record("elixir") do
    [:require_ast]
  else
    []
  end

# OTP 28.1+
re_import_exclude =
  if Code.ensure_loaded?(:re) and function_exported?(:re, :import, 1) do
    []
  else
    [:re_import]
  end

maybe_seed_opt = if seed = System.get_env("SEED"), do: [seed: String.to_integer(seed)], else: []

ex_unit_opts =
  [
    trace: !!System.get_env("TRACE"),
    exclude:
      epmd_exclude ++
        os_exclude ++
        line_exclude ++
        distributed_exclude ++ source_exclude ++ cover_exclude ++ re_import_exclude,
    include: line_include,
    assert_receive_timeout: String.to_integer(System.get_env("ELIXIR_ASSERT_TIMEOUT", "300"))
  ] ++ maybe_seed_opt

ExUnit.start(ex_unit_opts)
