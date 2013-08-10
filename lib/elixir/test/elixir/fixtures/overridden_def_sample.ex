defmodule OverriddenDefSample do

def foo(x, 1) do
  x + 1 
end

def bar(y, z) do
  z + y
end

def foo(x, 2) do
  x * 2 
end

end
