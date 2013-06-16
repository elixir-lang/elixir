ExUnit.start []

defmodule KernelTest do
  use ExUnit.Case, async: true
  doctest Kernel
  doctest Kernel.SpecialForms
  doctest Access.List
  doctest Binary.Chars.List
  doctest Binary.Inspect.Atom
  doctest Binary.Inspect.BitString
  doctest Binary.Inspect.List
  doctest Binary.Inspect.Tuple
  doctest Binary.Inspect.Number
  doctest Binary.Inspect.Regex
  doctest Bitwise
  doctest Code
  doctest Dict
  doctest Enum
  doctest IO.ANSI
  doctest Regex
  doctest String
  doctest OptionParser
  doctest Path
  doctest Module
  doctest Macro
  doctest List
  doctest Keyword
end
