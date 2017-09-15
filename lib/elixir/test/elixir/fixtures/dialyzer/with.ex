defmodule Dialyzer.With do
  def with_else do
    with :ok <- ok_or_error_with_atom(),
         :ok <- ok_or_error_with_string() do
      :ok
    else
      {:error, msg} when is_atom(msg) -> :error
      {:error, _msg} -> :error
    end
  end

  @spec ok_or_error_with_atom() :: :ok | {:error, atom()}
  defp ok_or_error_with_atom do
    Enum.random([:ok, {:error, :err}])
  end

  @spec ok_or_error_with_string() :: :ok | {:error, String.t}
  defp ok_or_error_with_string do
    Enum.random([:ok, {:error, "err"}])
  end
end
