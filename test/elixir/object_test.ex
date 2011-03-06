Code.require File.expand_path("../test_helper", __FILE__)
Code.require "fixtures/like_cat"
Code.require "fixtures/like_dog"
Code.require "fixtures/recorder"

object ObjectTest
  proto ExUnit::Case

  def nomethod_error_test
    object = Object.new
    error = { 'nomethod, {object, 'say, 0} }
    self.assert_error error, -> object.say
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
    self.assert_error {'notamodule, { 1, 'mixin } }, do Object.mixin(1)
    self.assert_error {'notamodule, { 1, 'proto } }, do Object.proto(1)
  end

  def builtin_not_allowed_test
    self.assert_error {'builtinnotallowed, { 1, 'new } },      do 1.new
    self.assert_error {'builtinnotallowed, { 1, 'set_ivar } }, do 1.set_ivar('foo, 'bar)
    self.assert_error {'builtinnotallowed, { 1, 'mixin } },    do 1.mixin(LikeCat)
    self.assert_error {'builtinnotallowed, { 1, 'proto } },    do 1.proto(LikeCat)
  end

  def parent_test
    'List = [].__parent_name__

    constant = List
    constant = [].__parent__

    [] = Object.__parent_name__
    [] = Object.__parent__

    object = Object
    parent = Object.new
    child = parent.new

    object = parent.__parent__
    'Object = parent.__parent_name__

    parent = child.__parent__
    [] = child.__parent_name__
  end

  def inspect_test
    object = Object.new
    "<Object {}>" = object.inspect

    with_ivars = Object.new.set_ivar('foo, 'bar).set_ivar('bar, 'baz)
    "<Object {'bar: 'baz, 'foo: 'bar}>" = with_ivars.inspect

    "<ObjectTest {}>" = self.inspect
  end

  def method_missing_test
    recorder = Recorder.new

    [{'hello, ['the, "world"]}] = recorder.hello('the, "world").calls

    processor = recorder.filter( -> (x) x rem 2 == 0 ).map( -> (x) x * 10 )
    [20, 40] = processor.play([1, 2, 3, 4])
  end

  def ivar_append_test
    [1] = Object.new.ivar_append('foo, 1).get_ivar('foo)

    object = Object.new.ivar_append('list, 1).ivar_append('list, 2)
    [1,2] = object.get_ivar('list)

    self.assert_error 'badarg, do
      Object.new.set_ivar('foo, 1).ivar_append('foo, 2)
    end
  end
end
