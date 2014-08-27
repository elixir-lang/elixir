ExUnit.start [trace: "--trace" in System.argv]

# Beam files compiled on demand
path = Path.expand("../../tmp/beams", __DIR__)
File.rm_rf!(path)
File.mkdir_p!(path)
Code.prepend_path(path)

Code.compiler_options debug_info: true

defmodule PathHelpers do
  def fixture_path() do
    Path.expand("fixtures", __DIR__)
  end

  def tmp_fixture_path(extra) do
    src = fixture_path(extra)
    dest = tmp_path(extra)
    File.cp_r(src,dest)
    dest
  end

  def tmp_path() do
    Path.expand("../../tmp", __DIR__)
  end

  def fixture_path(extra) do
    Path.join(fixture_path, extra)
  end

  def tmp_path(extra) do
    Path.join(tmp_path, extra)
  end

  def elixir(args) do
    runcmd(elixir_executable, args)
  end

  def elixir_executable do
    executable_path("elixir")
  end

  def elixirc(args) do
    runcmd(elixirc_executable, args)
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

  defp runcmd(executable,args) do
    :os.cmd :binary.bin_to_list("#{executable} #{IO.chardata_to_string(args)}#{redirect_std_err_on_win}")
  end

  defp executable_path(name) do
    Path.expand("../../../../bin/#{name}#{executable_extension}", __DIR__)
  end

  if match? {:win32, _}, :os.type do
    def is_win?, do: true
    def executable_extension, do: ".bat"
    def redirect_std_err_on_win, do: " 2>&1"
  else
    def is_win?, do: false
    def executable_extension, do: ""
    def redirect_std_err_on_win, do: ""
  end
end

defmodule CompileAssertion do
  import ExUnit.Assertions

  def assert_compile_fail(exception, string) do
    case format_rescue(string) do
      {^exception, _} -> :ok
      error ->
        raise ExUnit.AssertionError,
          left: inspect(elem(error, 0)),
          right: inspect(exception),
          message: "Expected match"
    end
  end

  def assert_compile_fail(exception, message, string) do
    case format_rescue(string) do
      {^exception, ^message} -> :ok
      error ->
        raise ExUnit.AssertionError,
          left: "#{inspect elem(error, 0)}[message: #{inspect elem(error, 1)}]",
          right: "#{inspect exception}[message: #{inspect message}]",
          message: "Expected match"
    end
  end

  defp format_rescue(expr) do
    result = try do
      :elixir.eval(to_char_list(expr), [])
      nil
    rescue
      error -> {error.__struct__, Exception.message(error)}
    end

    result || flunk(message: "Expected expression to fail")
  end
end
