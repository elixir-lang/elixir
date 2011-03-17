Code.require File.expand_path("../test_helper", __FILE__)

object AtomTest
  proto ExUnit::Case

  def method_atoms_test
    '+
    '-
    '/
    '*
    '<-
    '[]
    '$1
  end

  def to_constant_test
    atom = Atom
    atom = 'Atom.to_constant

    self.assert_error { 'noconstant, 'Foo }, do
      'Foo.to_constant
    end
  end

  def inspect_test
    "'foo" = 'foo.inspect
    "'@foo" = '@foo.inspect
    "'foo?" = 'foo?.inspect
    "'foo!" = 'foo!.inspect
    "'\"f!o\"" = '"f!o".inspect
    "'\"foo bar\"" = '"foo bar".inspect
  end

  def tuple_shortcut_test
    {'a, 2} = 'a/2
  end

  def to_s_test
    "foo" = 'foo.to_s
    "@foo" = '@foo.to_s
    "foo?" = 'foo?.to_s
    "foo!" = 'foo!.to_s
    "f!o" = '"f!o".to_s
    "foo bar" = '"foo bar".to_s
  end
end