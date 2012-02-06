Code.require_file "../../test_helper", __FILE__

defmodule Kernel::RescueTest do
  use ExUnit::Case

  def test_rescue_with_underscore_no_exception do
    true = try do
      RescueUndefinedModule.go
    rescue: _
      true
    end
  end

  def test_rescue_with_higher_precedence_than_catch do
    true = try do
      RescueUndefinedModule.go
    catch: _, _
      false
    rescue: _
      true
    end
  end

  def test_rescue_runtime_error do
    true = try do
      raise "an exception"
    rescue: RuntimeError
      true
    catch: :error, _
      false
    end

    false = try do
      raise "an exception"
    rescue: AnotherError
      true
    catch: :error, _
      false
    end
  end

  def test_rescue_named_runtime_error do
    "an exception" = try do
      raise "an exception"
    rescue: x in [RuntimeError]
      x.message
    catch: :error, _
      false
    end
  end

  def test_rescue_named_with_underscore do
    "an exception" = try do
      raise "an exception"
    rescue: x in _
      x.message
    end
  end

  def test_rescue_defined_variable do
    expected = RuntimeError

    true = try do
      raise "an exception"
    rescue: ^expected
      true
    catch: :error, _
      false
    end
  end

  def test_rescue_named_defined_variable do
    expected = RuntimeError

    "an exception" = try do
      raise RuntimeError, message: "an exception"
    rescue: x in [expected, AnotherError]
      x.message
    catch: :error, _
      false
    end
  end

  def test_wrap_custom_erlang_error do
    "erlang error: :sample" = try do
      error(:sample)
    rescue: x in [RuntimeError, ErlangError]
      x.message
    end
  end

  def test_undefined_function_error do
    "undefined function: ::DoNotExist.for_sure/0" = try do
      DoNotExist.for_sure()
    rescue: x in [UndefinedFunctionError]
      x.message
    end
  end

  def test_function_clause_error do
    "no function clause matching: ::Kernel::RescueTest.zero(1)" = try do
      zero(1)
    rescue: x in [FunctionClauseError]
      x.message
    end
  end

  def test_badarg_error do
    "argument error" = try do
      error(:badarg)
    rescue: x in [ArgumentError]
      x.message
    end
  end

  def test_tuple_badarg_error do
    "argument error: [1,2,3]" = try do
      error({ :badarg, [1,2,3] })
    rescue: x in [ArgumentError]
      x.message
    end
  end

  def test_badarith_error do
    "bad argument in arithmetic expression" = try do
      error(:badarith)
    rescue: x in [ArithmeticError]
      x.message
    end
  end

  def test_badarity_error do
    fun    = fn(x, do: x)
    string = "bad arity error: #{inspect(fun)} called with [1,2]"

    ^string = try do
      fun.(1,2)
    rescue: x in [BadArityError]
      x.message
    end
  end

  def test_badfun_error do
    x = :example
    "bad function: :example" = try do
      x.(2)
    rescue: x in [BadFunctionError]
      x.message
    end
  end

  def test_badmatch_error do
    x = :example
    "no match of right hand side value: :other" = try do
      ^x = :other
    rescue: x in [MatchError]
      x.message
    end
  end

  def test_case_clause_error do
    x = :example
    "no case clause matching: :other" = try do
      case :other do
      match: ^x
      end
    rescue: x in [CaseClauseError]
      x.message
    end
  end

  def test_undefined_function_error_from_expected_variable do
    expected = UndefinedFunctionError
    "undefined function: ::DoNotExist.for_sure/0" = try do
      DoNotExist.for_sure()
    rescue: x in [expected]
      x.message
    end
  end

  def test_undefined_function_error_as_erlang_error do
    "undefined function: ::DoNotExist.for_sure/0" = try do
      DoNotExist.for_sure()
    rescue: x in [ErlangError]
      x.message
    end
  end

  defp zero(0), do: 0
end