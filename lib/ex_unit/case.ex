module ExUnit::Case

defmacro prepare do
  quote do
    ExUnit::Server.add_case(__MODULE__)
    
    def __tests__ do
      ExUnit::Case.tests_for(__MODULE__)
    end
  end
end

def tests_for(mod) do
  exports = mod.module_info(:exports)
  tests_for exports, []
end

private

def tests_for([{function,0}|t], acc) do
  list = atom_to_list(function)
  case list do
  match: 'test_' ++ _
    tests_for t, [function|acc]
  else:
    tests_for t, acc
  end
end

def tests_for([_|t], acc), do: tests_for t, acc
def tests_for([], acc),    do: List.reverse(acc)
