ExUnit.start []

defmodule KernelTest do
  use ExUnit.Case, async: true
  doctest Kernel
end
