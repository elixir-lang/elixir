Code.require_file "../test_helper", __FILE__
Code.require_file "../fixtures/like_cat", __FILE__
Code.require_file "../fixtures/like_dog", __FILE__
Code.require_file "../fixtures/recorder", __FILE__

module ObjectTest
  mixin ExUnit::Case

  def nomethod_error_test
    object = Object.new
    error = { 'nomethod, {'say, 0, object} }
    assert_error error, -> object.say
  end

  def nomethod_error_when_calling_new_on_module_test
    assert_error { 'nomethod, { 'new, 0, LikeDog } }, do
      LikeDog.new
    end

    assert_error { 'nomethod, { 'new, 2, LikeDog } }, do
      LikeDog.new(1,2)
    end
  end

  def runtime_mixin_test
    object = Object.new
    cat = object.mixin(LikeCat)
    "meow" = cat.say
    dog = cat.mixin(LikeDog)
    "woof" = dog.say
  end

  def runtime_proto_test
    object = Object.new
    cat_parent = object.proto(LikeCat)
    "meow" = cat_parent.new.say
    dog_parent = cat_parent.proto(LikeDog)
    "woof" = dog_parent.new.say
  end

  def only_module_mixin_proto_test
    assert_error {'notamodule, { 'mixin, 1 } }, do Object.mixin(1)
    assert_error {'notamodule, { 'proto, 1 } }, do Object.proto(1)
  end

  def builtin_not_allowed_test
    assert_error {'builtinnotallowed, { 'new, 1 } },      do 1.new
    assert_error {'builtinnotallowed, { 'set_ivar, 1 } }, do 1.set_ivar('foo, 'bar)
    assert_error {'builtinnotallowed, { 'mixin, 1 } },    do 1.mixin(LikeCat)
    assert_error {'builtinnotallowed, { 'proto, 1 } },    do 1.proto(LikeCat)
  end

  def parent_test
    'List = [].__parent_name__

    constant = List
    ~constant = [].__parent__

    nil = Object.__parent_name__
    nil = Object.__parent__

    object = Object
    parent = Object.new
    child = parent.new

    ~object = parent.__parent__
    'Object = parent.__parent_name__

    ~parent = child.__parent__
    nil = child.__parent_name__
  end

  def inspect_test
    object = Object.new
    "<Object {}>" = object.inspect

    with_ivars = Object.new.set_ivar('foo, 'bar).set_ivar('bar, 'baz)
    "<Object {'bar: 'baz, 'foo: 'bar}>" = with_ivars.inspect
  end

  def method_missing_test
    recorder = Recorder.new

    [{'hello, ['the, "world"]}] = recorder.hello('the, "world").calls

    processor = recorder.filter( -> (x) x rem 2 == 0 ).map( -> (x) x * 10 )
    [20, 40] = processor.play([1, 2, 3, 4])
  end

  def constant_method_missing_test
    {'foo,[1,2,3]} = Recorder.foo(1,2,3)

    assert_error { 'nomethod, {'foo,3,Object} }, do
      Object.foo(1,2,3)
    end
  end

  def set_ivars_syntax_Test
    @('a, 1)
    1 = @a

    dict = { 'a: 1, 'b: 2 }
    @(dict)
    ~dict = self.get_ivars
  end

  def get_and_set_ivars_test
    dict = { 'a: 1, 'b: 2 }
    ~dict = Object.new.set_ivars(dict).get_ivars
  end

  def does_not_include_module_methods_test
    false = 1.__mixin_methods__.include?({ 'attr_reader, 1 })
  end

  def __name__test
    nil = 1.__name__
  end

  def update_ivar_test
    [1] = Object.new.set_ivar('foo, []).update_ivar('foo, _.push(1)).get_ivar('foo)

    object = Object.new.update_ivar('list, [1], -> (_) self.error "never called").update_ivar('list, _.push(2))
    [1,2] = object.get_ivar('list)

    self.assert_error 'function_clause, do
      Object.new.update_ivar('foo, 2)
    end
  end
end
