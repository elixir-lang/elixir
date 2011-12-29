module Elixir::ErrorsTest do
  use ExUnit::Case

  module __MODULE__ :: UnproperMacro do
    defmacro unproper(args), do: args
  end

  def test_invalid_token do
    "nofile:1: invalid token: \end" = format_catch '\end'
  end

  def test_syntax_error do
    "nofile:1: syntax error before: '}'" = format_catch 'case 1 do }'
  end

  def test_syntax_error_with_no_token do
    "nofile:1: syntax error" = format_catch 'case 1 do'
  end

  def test_bad_form do
    "nofile:2: function bar/0 undefined" = format_catch 'module Foo do\ndef foo, do: bar\nend'
  end

  def test_invalid_scope_for_module do
    "nofile:3: invalid scope for: module" = format_catch 'module Foo do\ndef foo do\nmodule Bar, do: 1\nend\nend'
  end

  def test_invalid_scope_for_function do
    "nofile:1: invalid scope for: def" = format_catch 'def Foo, do: 2'
    "nofile:3: invalid scope for: defmacro" = format_catch '\n\ndefmacro Foo, do: 2'
  end

  def test_invalid_case_args do
    "nofile:1: invalid args for: case" = format_catch 'case 1, 2, 3'
  end

  def test_invalid_quote_args do
    "nofile:1: invalid args for: quote" = format_catch 'quote 1'
  end

  def test_invalid_fn_args do
    "nofile:1: invalid args for: fn" = format_catch 'fn 1'
  end

  def test_invalid_try_args do
    "nofile:1: invalid args for: try" = format_catch 'try 1 do\n2\nend'
  end

  def test_unproper_macro do
    "nofile:2: key value blocks not supported by: ::Elixir::ErrorsTest::UnproperMacro.unproper/1" =
      format_catch 'module Foo do\nElixir::ErrorsTest::UnproperMacro.unproper do\nmatch: 1\nmatch: 2\nend\nend'
  end

  def test_def_defmacro_clause_change do
    "nofile:3: defmacro foo/1 already defined as def" =
      format_catch 'module Foo do\ndef foo(1), do: 1\ndefmacro foo(x), do: x\nend'
  end

  def test_visibility_clause_change do
    "nofile:4: function foo/1 already defined with visibility public" =
      format_catch 'module Foo do\ndef foo(1), do: 1\nprivate\ndef foo(x), do: x\nend'
  end

  def test_clause_change do
    "nofile:4: function foo/1 clause does not match with previous one" =
      format_catch 'module Foo do\ndef foo(1), do: 1\ndef bar(x), do: x\ndef foo(x), do: x\nend'
  end

  def test_internal_function_overriden do
    "nofile:1: function __macros__/0 is internal and should not be overriden" =
      format_catch 'module Foo do\ndef __macros__, do: []\nend'
  end

  def test_interpolation_error do
    "nofile:1: syntax error before: ')'" = format_catch '"foo\#{case 1 do )}bar"'
  end

  private

  def format_catch(expr) do
    try do
      Erlang.elixir.eval(expr)
      error({ :bad_assertion, "Expected function given to format_catch to fail" })
    catch: { kind, error, _ }
      Elixir::Formatter.format_catch(kind, error)
    end
  end
end