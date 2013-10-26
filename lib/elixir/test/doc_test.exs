ExUnit.start []

defmodule KernelTest do
  use ExUnit.Case, async: true

  doctest Access.List
  doctest String.Chars.List
  doctest Bitwise
  doctest Code
  doctest Enum
  doctest Exception
  doctest Float
  doctest IO.ANSI
  doctest Inspect.Algebra
  doctest Integer
  doctest Keyword
  doctest Kernel
  doctest Kernel.SpecialForms
  doctest List
  doctest Macro
  doctest Module
  doctest OptionParser
  doctest Path
  doctest Protocol.Consolidation
  doctest Regex
  doctest Set
  doctest Stream
  doctest String
  doctest Tuple
  doctest Version
end
