module List

def append(list) do
  Erlang.lists.append(list)
end

def append([h], right) do
  [h|right]
end

def append(left, right) do
  left ++ right
end

def map([h|t], f) do
  [f.(h)|map(t,f)]
end

def map([], f) when is_function(f, 1) do
  []
end

def mapfoldl([h|t], acc, f) do
  { result, acc } = f.(h, acc)
  { rest, acc }   = mapfoldl(t, acc, f)
  { [result|rest], acc }
end

def mapfoldl([], acc, f) when is_function(f, 2) do
  { [], acc }
end

def reverse(list) do
  Erlang.lists.reverse(list)
end

def uniq(list) do
  uniq(list, [])
end

def wrap(list) when is_list(list) do
  list
end

def wrap(other) do
  [other]
end

private

def uniq([h|t], acc) do
  case Erlang.lists.member(h, acc) do
  match: true
    uniq(t, acc)
  match: false
    uniq(t, [h|acc])
  end
end

def uniq([], acc) do
  Erlang.lists.reverse(acc)
end