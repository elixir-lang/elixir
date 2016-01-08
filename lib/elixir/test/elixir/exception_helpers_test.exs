Code.require_file "test_helper.exs", __DIR__

defmodule ExceptionHelpersTest do
  use ExUnit.Case, async: true

  test "finds functions of the same name with different arity" do
    assert_found &IO.puts/0, [puts: 1, puts: 2]
  end

  test "finds similar functions when no name matches" do
    assert_found &IO.put/1, [puts: 1, puts: 2]
    assert_found &Enum.man/0, [min: 1, max: 1, map: 2]
    assert_found &String.capitalise/1, [capitalize: 1]
  end

  test "undefined function exception message" do
    assert %UndefinedFunctionError{module: File, function: :open, arity: 0}
      |> Exception.message =~ """
      undefined function File.open/0

          \e[1mPerhaps you meant one of:\e[0m

             File.open/1
             File.open/3
             File.open/2
      """
  end

  test "undefined module exception message" do
    assert %UndefinedFunctionError{module: Filr, function: :open, arity: 1}
      |> Exception.message =~ """
      undefined function Filr.open/1 (module Filr is not available)

          \e[1mPerhaps you meant one of:\e[0m

             File
      """
  end

  test "works with Erlang modules" do
    assert ":file" in Exception.Helpers.find_modules(:filr, :open, 1)
  end

  test "will find unloaded modules" do
    assert "OptionParser" in Exception.Helpers.find_modules(OptionParse, :foo, 1)
  end

  def assert_found(function, expected) do
    info = :erlang.fun_info(function)
    result = Exception.Helpers.find_functions(info[:module], info[:name], info[:arity])

    assert result == expected
  end
end
