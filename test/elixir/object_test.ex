Code.require "fixtures/like_cat"
Code.require "fixtures/like_dog"

object ObjectTest
  proto ExUnit::Case

  def nomethod_error_test
    object = Object.new
    error = { 'nomethod, {object, 'say, 0} }
    self.assert_raise error, -> object.say
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
    self.assert_raise {'notamodule, { 1, 'mixin } }, do Object.mixin(1)
    self.assert_raise {'notamodule, { 1, 'proto } }, do Object.proto(1)
  end

  def builtin_not_allowed_test
    self.assert_raise {'builtinnotallowed, { 1, 'new } },      do 1.new
    self.assert_raise {'builtinnotallowed, { 1, 'set_ivar } }, do 1.set_ivar('foo, 'bar)
    self.assert_raise {'builtinnotallowed, { 1, 'mixin } },    do 1.mixin(LikeCat)
    self.assert_raise {'builtinnotallowed, { 1, 'proto } },    do 1.proto(LikeCat)
  end
end