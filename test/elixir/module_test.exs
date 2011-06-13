Code.require_file "../test_helper", __FILE__
Code.require_file "../fixtures/default_args", __FILE__

module ModuleTest
  mixin ExUnit::Case

  module A
    def foo
      1 + bar + baz
    end

    def bar
      1
    end

    private

    def baz
      1
    end
  end

  module B
    mixin ModuleTest::A

    def bar
      2
    end

    def baz
      3
    end
  end

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
    false = OrderedDict.respond_to? 'mixin, 1
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

  def builtin_not_allowed_test
    assert_error {'builtin_not_allowed, { 'set_ivar, 1 } }, do 1.set_ivar('foo, 'bar)
  end

  module Example
  end

  def inspect_test
    "ModuleTest::Example" = ModuleTest::Example.inspect

    with_ivars = #ModuleTest::Example().set_ivar('foo, 'bar).set_ivar('bar, 'baz)
    "<#ModuleTest::Example {'bar: 'baz, 'foo: 'bar}>" = with_ivars.inspect
  end

  def to_s_test
    "ModuleTest::Example" = ModuleTest::Example.to_s
    with_ivars = #ModuleTest::Example().set_ivar('foo, 'bar).set_ivar('bar, 'baz)
    "<#ModuleTest::Example {'bar: 'baz, 'foo: 'bar}>" = with_ivars.to_s
  end

  def get_and_set_ivars_test
    dict = { 'a: 1, 'b: 2 }
    ~dict = #ModuleTest::Example().set_ivars(dict).get_ivars
  end

  def update_ivar_test
    [1] = #ModuleTest::Example().set_ivar('foo, []).update_ivar('foo, _.push(1)).get_ivar('foo)
  end

  def remove_ivar_test
    nil = #ModuleTest::Example().set_ivar('foo, 'bar).remove_ivar('foo).get_ivar('foo)
  end

  def local_and_remote_calls_test
    3 = ModuleTest::A.foo
    4 = ModuleTest::B.foo
  end

  % def method_missing_test
  %   recorder = Recorder.new
  %   [{'hello, ['the, "world"]}] = recorder.hello('the, "world").calls
  %   processor = recorder.filter( -> (x) x rem 2 == 0 ).map( -> (x) x * 10 )
  %   [20, 40] = processor.play([1, 2, 3, 4])
  % end

  private

  def operators(1 + 1, 'atom/2)
  end
end
