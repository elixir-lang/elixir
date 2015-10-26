ExUnit.start []

defmodule KernelTest do
  use ExUnit.Case, async: true

  doctest Access
  doctest Atom
  doctest Base
  doctest Bitwise, import: true
  doctest Code
  doctest Collectable
  doctest Enum
  doctest Exception
  doctest Float
  doctest Inspect
  doctest Inspect.Algebra
  doctest Integer
  doctest IO
  doctest IO.ANSI
  doctest Kernel
  doctest Kernel.SpecialForms
  doctest Keyword
  doctest List
  doctest Macro
  doctest Map
  doctest MapSet
  doctest Module
  doctest Node
  doctest OptionParser
  doctest Path
  doctest Process
  doctest Protocol
  doctest Range
  doctest Record
  doctest Regex
  doctest Stream
  doctest String
  doctest String.Chars
  doctest StringIO
  doctest Tuple
  doctest URI
  doctest Version
end
