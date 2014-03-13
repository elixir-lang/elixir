ExUnit.start []

defmodule KernelTest do
  use ExUnit.Case, async: true

  doctest Access
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
  doctest Node
  doctest OptionParser
  doctest Path
  doctest Protocol.Consolidation
  doctest Range
  doctest Regex
  doctest Stream
  doctest String
  doctest StringIO
  doctest Collectable
  doctest Tuple
  doctest URI
  doctest Version
end
