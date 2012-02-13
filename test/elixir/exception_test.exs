Code.require_file "../test_helper", __FILE__

defmodule Kernel::ExceptionTest do
  use ExUnit::Case

  def test_is_exception do
    assert is_exception(RuntimeError.new)
    refute is_exception({ :foo, :bar })
  end

  def test_format_stacktrace_with_no_file_or_line do
    assert_equal "::Foo.bar(1, 2, 3)", Exception.format_stacktrace({::Foo, :bar, [1, 2, 3], []})
    assert_equal "::Foo.bar()", Exception.format_stacktrace({::Foo, :bar, [], []})
    assert_equal "::Foo.bar/1", Exception.format_stacktrace({::Foo, :bar, 1, []})
  end

  def test_format_stacktrace_with_file_and_line do
    assert_equal "file.ex:10: ::Foo.bar()", Exception.format_stacktrace({::Foo, :bar, [], [file: 'file.ex', line: 10]})
    assert_equal "file.ex:10: ::Foo.bar(1, 2, 3)", Exception.format_stacktrace({::Foo, :bar, [1, 2, 3], [file: 'file.ex', line: 10]})
    assert_equal "file.ex:10: ::Foo.bar/1", Exception.format_stacktrace({::Foo, :bar, 1, [file: 'file.ex', line: 10]})
  end

  def test_format_stacktrace_with_file_no_line do
    assert_equal "file.ex: ::Foo.bar()", Exception.format_stacktrace({::Foo, :bar, [], [file: 'file.ex']})
    assert_equal "file.ex: ::Foo.bar()", Exception.format_stacktrace({::Foo, :bar, [], [file: 'file.ex', line: 0]})
    assert_equal "file.ex: ::Foo.bar(1, 2, 3)", Exception.format_stacktrace({::Foo, :bar, [1, 2, 3], [file: 'file.ex']})
    assert_equal "file.ex: ::Foo.bar/1", Exception.format_stacktrace({::Foo, :bar, 1, [file: 'file.ex']})
  end

  def test_format_module_function_arity do
    assert_equal "::Foo.bar/1", Exception.format_module_fun_arity ::Foo, :bar, 1
    assert_equal "::Foo.bar()", Exception.format_module_fun_arity ::Foo, :bar, []
    assert_equal "foo:bar(1, 2)", Exception.format_module_fun_arity :foo,  :bar, [1,2]
  end

  def test_runtime_error_message do
    assert_equal "runtime error", RuntimeError.new.message
    assert_equal "exception", RuntimeError.new(message: "exception").message
  end

  def test_argument_error_message do
    assert_equal "argument error", ArgumentError.new.message
    assert_equal "exception", ArgumentError.new(message: "exception").message
  end

  def test_undefined_function_message do
    assert_equal "undefined function", UndefinedFunctionError.new.message
    assert_equal "undefined function: ::Foo.bar/1", UndefinedFunctionError.new(module: ::Foo, function: :bar, arity: 1).message
    assert_equal "undefined function: ::Foo.bar/0", UndefinedFunctionError.new(module: ::Foo, function: :bar, arity: []).message
    assert_equal "undefined function: foo:bar/0", UndefinedFunctionError.new(module: :foo,  function: :bar, arity: []).message
  end

  def test_function_clause_message do
    assert_equal "no function clause matches", FunctionClauseError.new.message
    assert_equal "no function clause matching: ::Foo.bar/1", FunctionClauseError.new(module: ::Foo, function: :bar, arity: 1).message
    assert_equal "no function clause matching: ::Foo.bar()", FunctionClauseError.new(module: ::Foo, function: :bar, arity: []).message
    assert_equal "no function clause matching: foo:bar()", FunctionClauseError.new(module: :foo,  function: :bar, arity: []).message
  end

  def test_erlang_error_message do
    assert_equal "erlang error: :sample", ErlangError.new(original: :sample).message
  end
end
