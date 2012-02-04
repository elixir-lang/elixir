Code.require_file "../test_helper", __FILE__

defmodule Kernel::ExceptionTest do
  use ExUnit::Case

  def test_is_exception do
    true  = is_exception(RuntimeError.new)
    false = is_exception({ :foo, :bar })
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
    "undefined function ::Foo.bar/1" = UndefinedFunctionError.new(module: ::Foo, function: :bar, arity: 1).message
    "undefined function ::Foo.bar/0" = UndefinedFunctionError.new(module: ::Foo, function: :bar, arity: []).message
    "undefined function foo:bar/0"   = UndefinedFunctionError.new(module: :foo,  function: :bar, arity: []).message
  end

  def test_erlang_error_message do
    "erlang error: :sample" = ErlangError.new(original: :sample).message
  end
end