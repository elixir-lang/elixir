defmodule ExUnit::Case do
  defmacro __using__(module, opts // []) do
    if Orddict.get(opts, :sync, false) do
      ExUnit::Server.add_sync_case(module)
    else:
      ExUnit::Server.add_case(module)
    end

    quote do
      import ExUnit::Assertions

      def __tests__ do
        ExUnit::Case.tests_for(__MODULE__)
      end
    end
  end

  def tests_for(mod) do
    exports = mod.__info__(:exports)
    tests_for exports, []
  end

  ## Private

  defp tests_for([%{function,0}|t], acc) do
    list = atom_to_list(function)
    case list do
    match: 'test_' ++ _
      tests_for t, [function|acc]
    else:
      tests_for t, acc
    end
  end

  defp tests_for([_|t], acc), do: tests_for t, acc
  defp tests_for([], acc),    do: List.reverse(acc)
end