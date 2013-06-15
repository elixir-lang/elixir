ExUnit.start []

defmodule ExUnitTest do
  use ExUnit.Case, async: true

  doctest ExUnit.CaptureIO, import: true
end
