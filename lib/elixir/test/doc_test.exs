ExUnit.start []

defmodule KernelTest do
  use ExUnit.Case, async: true

  doctest Access
  doctest Base
  doctest Bitwise
  doctest Code
  doctest Collectable
  doctest EEx
  doctest Enum
  doctest Exception
  doctest Float
  doctest Inspect.Algebra
  doctest Integer
  doctest IO.ANSI
  doctest Kernel
  doctest Kernel.SpecialForms
  doctest Keyword
  doctest List
  doctest Macro
  doctest Module
  doctest Node
  doctest OptionParser
  doctest Path
  doctest Protocol.Consolidation
  doctest Range
  doctest Record
  doctest Regex
  doctest Stream
  doctest String
  doctest StringIO
  doctest Tuple
  doctest URI
  doctest Version
end
