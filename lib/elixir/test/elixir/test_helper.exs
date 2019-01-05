# Beam files compiled on demand
path = Path.expand("../../tmp/beams", __DIR__)
File.rm_rf!(path)
File.mkdir_p!(path)
Code.prepend_path(path)

Code.compiler_options(debug_info: true)

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

  def elixir(args) do
    run_cmd(elixir_executable(), args)
  end

  def elixir_executable do
    executable_path("elixir")
  end

  def elixirc(args) do
    run_cmd(elixirc_executable(), args)
  end

  def elixirc_executable do
    executable_path("elixirc")
  end

  def write_beam({:module, name, bin, _} = res) do
    File.mkdir_p!(unquote(path))
    beam_path = Path.join(unquote(path), Atom.to_string(name) <> ".beam")
    File.write!(beam_path, bin)
    res
  end

  defp run_cmd(executable, args) do
    '#{executable} #{IO.chardata_to_string(args)}#{redirect_std_err_on_win()}'
    |> :os.cmd()
    |> :binary.list_to_bin()
  end

  defp executable_path(name) do
    Path.expand("../../../../bin/#{name}#{executable_extension()}", __DIR__)
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

assert_timeout = String.to_integer(System.get_env("ELIXIR_ASSERT_TIMEOUT") || "500")
exclude = if PathHelpers.windows?(), do: [unix: true], else: [windows: true]

ExUnit.start(
  trace: "--trace" in System.argv(),
  assert_receive_timeout: assert_timeout,
  exclude: exclude
)
