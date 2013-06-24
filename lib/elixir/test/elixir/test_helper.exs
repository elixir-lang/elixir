# Configure ExUnit, no options supported yet.
ExUnit.start []

defmodule PathHelpers do
  def fixture_path() do
    Path.expand("../fixtures", __FILE__)
  end

  def tmp_path() do
    Path.expand("../../tmp", __FILE__)
  end

  def fixture_path(extra) do
    Path.join(fixture_path, extra)
  end

  def tmp_path(extra) do
    Path.join(tmp_path, extra)
  end

  def elixirc(args) do
    runcmd(elixirc_executable,args)
  end
  
  def elixir(args) do
    runcmd(elixir_executable,args)
  end
  
  if match? { :win32, _ }, :os.type do
    def executable_extension, do: ".bat"
    defp runcmd(executable,args), do: :os.cmd binary_to_list("#{executable} #{:unicode.characters_to_binary(args)} 2>&1")
  else
    def executable_extension, do: ""
    defp runcmd(executable,args), do: :os.cmd binary_to_list("#{executable} #{:unicode.characters_to_binary(args)}")
  end

  def elixir_executable do
    Path.expand("../../../../../bin/elixir#{executable_extension}", __FILE__)
  end

  def elixirc_executable do
    Path.expand("../../../../../bin/elixirc#{executable_extension}", __FILE__)
  end
end

defmodule CompileAssertion do
  def assert_compile_fail(exception, string) do
    case format_rescue(string) do
      { ^exception, _ } -> :ok
      error ->
        raise ExUnit.ExpectationError,
          expected: inspect(exception),
          actual: inspect(elem(error, 0)),
          reason: "match"
    end
  end

  def assert_compile_fail(exception, message, string) do
    case format_rescue(string) do
      { ^exception, ^message } -> :ok
      error ->
        raise ExUnit.ExpectationError,
          expected: "#{inspect exception}[message: #{inspect message}]",
          actual: "#{inspect elem(error, 0)}[message: #{inspect elem(error, 1)}]",
          reason: "match"
    end
  end

  defp format_rescue(expr) do
    result = try do
      :elixir.eval(to_char_list(expr), [])
      nil
    rescue
      error -> { error.__record__(:name), error.message }
    end

    result || raise(ExUnit.AssertionError, message: "Expected expression to fail")
  end
end
