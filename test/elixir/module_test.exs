Code.require_file "../test_helper", __FILE__
Code.require_file "../fixtures/default_args", __FILE__

module ModuleTest
  mixin ExUnit::Case

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

  def send_test
    { 1, 2 } = DefaultArgs.send 'many0
    { 1, 2 } = DefaultArgs.send 'many0, [1]
    { 2, 2 } = DefaultArgs.send 'many0, [2]
    { 1, 2 } = DefaultArgs.send 'many0, [1,2]
    { 2, 3 } = DefaultArgs.send 'many0, [2,3]
  end

  def respond_to_test
    true = OrderedDict.respond_to? 'from_list, 1
    true = OrderedDict.respond_to? '__module_name__, 0
    true = OrderedDict.respond_to? 'respond_to?, 2
    false = OrderedDict.respond_to? 'whatever, 0
    false = OrderedDict.respond_to? '__elixir_exported__, 1
  end

  def mixin_methods_test
    assert_included {'from_list, 1}, OrderedDict.__mixin_methods__
    assert_included {'respond_to?, 2}, OrderedDict.__mixin_methods__
    assert_included {'__module_name__, 0}, OrderedDict.__mixin_methods__
    assert_not_included {'whatever, 0}, OrderedDict.__mixin_methods__
    assert_not_included {'elixir_exported, 1}, OrderedDict.__mixin_methods__
  end

  def local_methods_test
    assert_included {'from_list, 1}, OrderedDict.__local_methods__
    assert_not_included {'respond_to?, 2}, OrderedDict.__local_methods__
    assert_included {'__module_name__, 0}, OrderedDict.__local_methods__
    assert_not_included {'whatever, 0}, OrderedDict.__local_methods__
    assert_not_included {'elixir_exported, 1}, OrderedDict.__local_methods__
  end

  private

  def operators(1 + 1, 'atom/2)
  end
end
