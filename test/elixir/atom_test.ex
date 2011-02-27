object AtomTest
  proto ExUnit::Case

  def inspect_test
    "'foo" = 'foo.inspect
    "'@foo" = '@foo.inspect
    "'foo?" = 'foo?.inspect
    "'foo!" = 'foo!.inspect
    "'\"f!o\"" = '"f!o".inspect
    "'\"foo bar\"" = '"foo bar".inspect
  end
end