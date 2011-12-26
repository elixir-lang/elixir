module List

def wrap(list) when is_list(list), do: list
def wrap(other), do: [other]

def reverse(list), do: Erlang.lists.reverse(list)