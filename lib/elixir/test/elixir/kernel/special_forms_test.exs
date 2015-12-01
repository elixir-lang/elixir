Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.SpecialFormsTest do
  use ExUnit.Case, async: true

  doctest Kernel.SpecialForms
end
