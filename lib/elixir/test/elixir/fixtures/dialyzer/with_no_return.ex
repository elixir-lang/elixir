defmodule Dialyzer.WithNoReturn do
  def with_no_return(list) do
    no_return = fn -> throw(:no_return) end

    with [] <- list do
      :ok
    else
      # note: throwing here directly wouldn't be caught in the first place,
      # calling a no_return function is what could cause an issue.
      _ -> no_return.()
    end
  end
end
