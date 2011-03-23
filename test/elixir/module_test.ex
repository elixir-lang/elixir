Code.require File.expand_path("../test_helper", __FILE__)
Code.require "fixtures/default_args"

object ModuleTest
  proto ExUnit::Case

  def simple_default_args_test
    1 = DefaultArgs.arity0
    1 = DefaultArgs.arity0(1)
    2 = DefaultArgs.arity0(2)

    { 1, 2 } = DefaultArgs.arity1(1)
    { 1, 2 } = DefaultArgs.arity1(1, 2)
    { 1, 1 } = DefaultArgs.arity1(1, 1)
  end

  def many_default_args_test
    { 1, 2 } = DefaultArgs.many0
    { 1, 2 } = DefaultArgs.many0(1)
    { 2, 2 } = DefaultArgs.many0(2)
    { 1, 2 } = DefaultArgs.many0(1, 2)
    { 2, 3 } = DefaultArgs.many0(2, 3)

    { 0, 1, 2 } = DefaultArgs.many1(0)
    { 0, 1, 2 } = DefaultArgs.many1(0, 1)
    { 0, 2, 2 } = DefaultArgs.many1(0, 2)
    { 0, 1, 2 } = DefaultArgs.many1(0, 1, 2)
    { 0, 2, 3 } = DefaultArgs.many1(0, 2, 3)
  end

  def clash_default_args_test
    [1,2,3]  = DefaultArgs.clash([])
    { 1, 2 } = DefaultArgs.clash()
    { 2, 2 } = DefaultArgs.clash(2)
    { 2, 1 } = DefaultArgs.clash(2, 1)
  end

  def atom_default_args_test
    1 = DefaultArgs.atom('foo)
    2 = DefaultArgs.atom('bar)
  end

  def operators_signature_test
    operators(2, {'atom, 2})
  end

  private

  def operators(1 + 1, 'atom/2)
  end
end
