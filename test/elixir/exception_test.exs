Code.require_file "../test_helper", __FILE__

defmodule Kernel::ExceptionTest do
  use ExUnit::Case

  def test_is_exception do
    true  = is_exception(RuntimeError.new)
    false = is_exception({ :foo, :bar })
  end

  def test_format_stacktrace_with_no_file_or_line do
    "::Foo.bar(1, 2, 3)" = Exception.format_stacktrace({::Foo, :bar, [1, 2, 3], []})
    "::Foo.bar()" = Exception.format_stacktrace({::Foo, :bar, [], []})
    "::Foo.bar/1" = Exception.format_stacktrace({::Foo, :bar, 1, []})
  end

  def test_format_stacktrace_with_file_and_line do
    "file.ex:10: ::Foo.bar()" = Exception.format_stacktrace({::Foo, :bar, [], [file: 'file.ex', line: 10]})
    "file.ex:10: ::Foo.bar(1, 2, 3)" = Exception.format_stacktrace({::Foo, :bar, [1, 2, 3], [file: 'file.ex', line: 10]})
    "file.ex:10: ::Foo.bar/1" = Exception.format_stacktrace({::Foo, :bar, 1, [file: 'file.ex', line: 10]})
  end

  def test_format_stacktrace_with_file_no_line do
    "file.ex: ::Foo.bar()" = Exception.format_stacktrace({::Foo, :bar, [], [file: 'file.ex']})
    "file.ex: ::Foo.bar(1, 2, 3)" = Exception.format_stacktrace({::Foo, :bar, [1, 2, 3], [file: 'file.ex']})
    "file.ex: ::Foo.bar/1" = Exception.format_stacktrace({::Foo, :bar, 1, [file: 'file.ex']})
  end

  def test_format_module_function_arity do
    "::Foo.bar/1"   = Exception.format_module_fun_arity ::Foo, :bar, 1
    "::Foo.bar()"   = Exception.format_module_fun_arity ::Foo, :bar, []
    "foo:bar(1, 2)" = Exception.format_module_fun_arity :foo,  :bar, [1,2]
  end

  def test_runtime_error_message do
    "runtime error" = RuntimeError.new.message
    "exception"     = RuntimeError.new(message: "exception").message
  end

  def test_argument_error_message do
    "argument error" = ArgumentError.new.message
    "exception"      = ArgumentError.new(message: "exception").message
  end

  def test_undefined_function_message do
    "undefined function" = UndefinedFunctionError.new.message
    "undefined function: ::Foo.bar/1" = UndefinedFunctionError.new(module: ::Foo, function: :bar, arity: 1).message
    "undefined function: ::Foo.bar/0" = UndefinedFunctionError.new(module: ::Foo, function: :bar, arity: []).message
    "undefined function: foo:bar/0"   = UndefinedFunctionError.new(module: :foo,  function: :bar, arity: []).message
  end

  def test_function_clause_message do
    "no function clause matches" = FunctionClauseError.new.message
    "no function clause matching: ::Foo.bar/1" = FunctionClauseError.new(module: ::Foo, function: :bar, arity: 1).message
    "no function clause matching: ::Foo.bar()" = FunctionClauseError.new(module: ::Foo, function: :bar, arity: []).message
    "no function clause matching: foo:bar()"   = FunctionClauseError.new(module: :foo,  function: :bar, arity: []).message
  end

  def test_erlang_error_message do
    "erlang error: :sample" = ErlangError.new(original: :sample).message
  end
end