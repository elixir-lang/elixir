ExUnit.start []

defmodule KernelTest do
  use ExUnit.Case, async: true

  doctest Access.List
  doctest Binary.Chars.List
  doctest Binary.Inspect.Atom
  doctest Binary.Inspect.BitString
  doctest Binary.Inspect.List
  doctest Binary.Inspect.Number
  doctest Binary.Inspect.Regex
  doctest Binary.Inspect.Tuple
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
  doctest String
end
