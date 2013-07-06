ExUnit.start []

defmodule KernelTest do
  use ExUnit.Case, async: true

  doctest Access.List
  doctest Binary.Chars.List
  doctest Inspect.Atom
  doctest Inspect.BitString
  doctest Inspect.List
  doctest Inspect.Number
  doctest Inspect.Regex
  doctest Inspect.Tuple
  doctest Bitwise
  doctest Code
  doctest Dict
  doctest Enum
  doctest IO.ANSI
  doctest Keyword
  doctest Kernel
  doctest Kernel.SpecialForms
  doctest List
  doctest Macro
  doctest Module
  doctest OptionParser
  doctest Path
  doctest Regex
  doctest Set
  doctest Stream
  doctest String
  doctest Inspect.Algebra
end
