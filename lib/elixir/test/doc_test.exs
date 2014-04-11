ExUnit.start []

defmodule KernelTest do
  use ExUnit.Case, async: true

  doctest Access
  doctest Base
  doctest Bitwise
  doctest Code
  doctest Collectable
  doctest Enum
  doctest Exception
  doctest Float
  doctest Inspect.Algebra
  doctest Inspect.Atom
  doctest Inspect.BitString
  doctest Inspect.Float
  doctest Inspect.Integer
  doctest Inspect.List
  doctest Inspect.Regex
  doctest Inspect.Tuple
  doctest Integer
  doctest IO.ANSI
  doctest Kernel
  doctest Kernel.SpecialForms
  doctest Keyword
  doctest List
  doctest Macro
  doctest Map
  doctest Module
  doctest Node
  doctest OptionParser
  doctest Path
  doctest Process
  doctest Protocol.Consolidation
  doctest Range
  doctest Record
  doctest Regex
  doctest Stream
  doctest String
  doctest String.Chars.List
  doctest StringIO
  doctest Tuple
  doctest URI
  doctest Version
end
