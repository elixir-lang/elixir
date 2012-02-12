Code.require_file "../../test_helper", __FILE__

defmodule Kernel::RescueTest do
  use ExUnit::Case

  def test_rescue_with_underscore_no_exception do
    result = try do
      RescueUndefinedModule.go
    rescue: _
      true
    end

    assert result
  end

  def test_rescue_with_higher_precedence_than_catch do
    result = try do
      RescueUndefinedModule.go
    catch: _, _
      false
    rescue: _
      true
    end

    assert result
  end

  def test_rescue_runtime_error do
    result = try do
      raise "an exception"
    rescue: RuntimeError
      true
    catch: :error, _
      false
    end

    assert result

    result = try do
      raise "an exception"
    rescue: AnotherError
      true
    catch: :error, _
      false
    end

    refute result
  end

  def test_rescue_named_runtime_error do
    result = try do
      raise "an exception"
    rescue: x in [RuntimeError]
      x.message
    catch: :error, _
      false
    end

    assert_equal "an exception", result
  end

  def test_rescue_named_with_underscore do
    result = try do
      raise "an exception"
    rescue: x in _
      x.message
    end

    assert_equal "an exception", result
  end

  def test_rescue_defined_variable do
    expected = RuntimeError

    result = try do
      raise "an exception"
    rescue: ^expected
      true
    catch: :error, _
      false
    end

    assert result
  end

  def test_rescue_named_defined_variable do
    expected = RuntimeError

    result = try do
      raise RuntimeError, message: "an exception"
    rescue: x in [expected, AnotherError]
      x.message
    catch: :error, _
      false
    end

    assert_equal "an exception", result
  end

  def test_wrap_custom_erlang_error do
    result = try do
      error(:sample)
    rescue: x in [RuntimeError, ErlangError]
      x.message
    end

    assert_equal "erlang error: :sample", result
  end

  def test_undefined_function_error do
    result = try do
      DoNotExist.for_sure()
    rescue: x in [UndefinedFunctionError]
      x.message
    end

    assert_equal "undefined function: ::DoNotExist.for_sure/0", result
  end

  def test_function_clause_error do
    result = try do
      zero(1)
    rescue: x in [FunctionClauseError]
      x.message
    end

    assert_equal "no function clause matching: ::Kernel::RescueTest.zero(1)", result
  end

  def test_badarg_error do
    result = try do
      error(:badarg)
    rescue: x in [ArgumentError]
      x.message
    end

    assert_equal "argument error", result
  end

  def test_tuple_badarg_error do
    result = try do
      error({ :badarg, [1,2,3] })
    rescue: x in [ArgumentError]
      x.message
    end

    assert_equal "argument error: [1,2,3]", result
  end

  def test_badarith_error do
    result = try do
      error(:badarith)
    rescue: x in [ArithmeticError]
      x.message
    end

    assert_equal "bad argument in arithmetic expression", result
  end

  def test_badarity_error do
    fun    = fn(x, do: x)
    string = "bad arity error: #{inspect(fun)} called with [1,2]"

    result = try do
      fun.(1,2)
    rescue: x in [BadArityError]
      x.message
    end

    assert_equal string, result
  end

  def test_badfun_error do
    x = :example
    result = try do
      x.(2)
    rescue: x in [BadFunctionError]
      x.message
    end

    assert_equal "bad function: :example", result
  end

  def test_badmatch_error do
    x = :example
    result = try do
      ^x = :other
    rescue: x in [MatchError]
      x.message
    end

    assert_equal "no match of right hand side value: :other", result
  end

  def test_case_clause_error do
    x = :example
    result = try do
      case :other do
      match: ^x
      end
    rescue: x in [CaseClauseError]
      x.message
    end

    assert_equal "no case clause matching: :other", result
  end

  def test_undefined_function_error_from_expected_variable do
    expected = UndefinedFunctionError
    result = try do
      DoNotExist.for_sure()
    rescue: x in [expected]
      x.message
    end

    assert_equal "undefined function: ::DoNotExist.for_sure/0", result
  end

  def test_undefined_function_error_as_erlang_error do
    result = try do
      DoNotExist.for_sure()
    rescue: x in [ErlangError]
      x.message
    end

    assert_equal "undefined function: ::DoNotExist.for_sure/0", result
  end

  defp zero(0), do: 0
end
