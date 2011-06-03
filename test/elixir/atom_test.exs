Code.require_file "../test_helper", __FILE__

module AtomTest
  mixin ExUnit::Case

  def method_atoms_test
    '+
    '-
    '/
    '*
    '<-
    '[]
    '$1
    'Foo::Bar
  end

  def exists_test
    'some_unknown_atom1
    true  = Atom.exists?("some_unknown_atom1")
    false = Atom.exists?("some_unknown_atom2")
  end

  def to_constant_test
    atom = Atom
    ~atom = 'Atom.to_constant

    self.assert_error { 'noconstant, 'ThisConstantDoesNotExist }, do
      'ThisConstantDoesNotExist.to_constant
    end
  end

  def inspect_test
    "'foo" = 'foo.inspect
    "'@foo" = '@foo.inspect
    "'foo?" = 'foo?.inspect
    "'foo!" = 'foo!.inspect
    "'\"f!o\"" = '"f!o".inspect
    "'\"foo bar\"" = '"foo bar".inspect
    "'Foo::Bar" = 'Foo::Bar.inspect
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