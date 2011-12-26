module List

def wrap(list) when is_list(list) do
  list
end

def wrap(other) do
  [other]
end

def reverse(list) do
  Erlang.lists.reverse(list)
end

def map([h|t], f) do
  [f.(h)|map(t,f)]
end

def map([], f) when is_function(f, 1) do
  []
end

def mapfoldl([h|t], acc, f) do  
  { result, acc } = f.(h, acc)
  { rest, acc }   = mapfoldl(f, acc, t)
  { [result|rest], acc }
end

def mapfoldl([], acc, f) when is_function(f, 2) do
  { [], acc }
end
