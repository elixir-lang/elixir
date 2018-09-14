defmodule Dialyzer.Try do
  def rescue_error do
    try do
      :erlang.error(:badarg)
    rescue
      e in ErlangError -> {:ok, e}
    end
  end
end
