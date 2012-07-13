Code.require_file "../test_helper", __FILE__

defmodule GuardTest do
  use ExUnit.Case

  test :in do 
   assert 1 in [1,2,3] == true
   assert 4 in [1,2,3] == false
   assert [] in [1,2,3] == false
  end
end