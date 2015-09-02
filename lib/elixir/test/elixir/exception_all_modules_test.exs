Code.require_file "test_helper.exs", __DIR__

defmodule ExceptionAllTest.Modules do
  defmacro tests_all_modules() do
    for module <- all_error_modules() do
      quote do
        # test "test #{unquote(module)} - no options" do
        #   assert_raise unquote(module),
        #     fn -> raise unquote(module)
        #   end
        # end

        # test "test #{unquote(module)} - unknown option" do
        #   assert_raise unquote(module),
        #     fn ->
        #       raise unquote(module), [unknown_option: ""] # ignore unknown options
        #   end
        # end

        # test "test #{unquote(module)} - message:" do
        #   assert_raise unquote(module),
        #     "give me this message",
        #     fn ->
        #       raise unquote(module), [message: "give me this message"]
        #   end
        # end

        # test "test #{unquote(module)} - description:" do
        #   assert_raise unquote(module),
        #     "error description",
        #     fn ->
        #       raise unquote(module), [description: "error description"]
        #   end
        # end

      end
    end
  end

  # TODO: load this list dinamically 
  def all_error_modules() do
    [
      Elixir.ArgumentError,
      Elixir.ArithmeticError, #Wrong message for ArithmeticError. Expected "give me this message", got "bad argument in arithmetic expression"
      Elixir.BadArityError,
      Elixir.BadFunctionError,
      Elixir.BadMapError,
      Elixir.BadStructError,
      Elixir.CaseClauseError,
      Elixir.Code.LoadError,
      Elixir.CompileError,
      Elixir.CondClauseError,
      Elixir.Enum.EmptyError,
      Elixir.Enum.OutOfBoundsError,
      Elixir.ErlangError,
      Elixir.File.CopyError,
      Elixir.File.Error,
      Elixir.FunctionClauseError,
      Elixir.Inspect.Error,
      Elixir.IO.StreamError,
      Elixir.KeyError,
      Elixir.MatchError,
      Elixir.Protocol.UndefinedError,
      Elixir.Regex.CompileError,
      Elixir.RuntimeError,
      Elixir.SyntaxError,
      Elixir.SystemLimitError,
      Elixir.TokenMissingError,
      Elixir.TryClauseError,
      Elixir.UndefinedFunctionError,
      Elixir.UnicodeConversionError, #Expected exception UnicodeConversionError but got KeyError (key :encoded not found in: [])
      Elixir.Version.InvalidRequirementError,
      Elixir.Version.InvalidVersionError,
    ]
  end

end

defmodule ExceptionAllTest do
  use ExUnit.Case, async: true

  import ExceptionAllTest.Modules

  tests_all_modules()

end
