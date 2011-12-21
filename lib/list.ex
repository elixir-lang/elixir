ns List

def wrap: [list] do
  case is_list(list) do
  match: true
    list
  match: false
    [list]
  end
end

def reverse: [list], do: Erlang.lists.reverse(list)