defmodule Dialyzer.With do
  def with_else do
    with :ok <- ok_or_error(),
         :ok <- ok_or_other_error() do
      :ok
    else
      :error ->
        :error
      :other_error ->
        :other_error
    end
  end

  @spec ok_or_error() :: :ok | :error
  defp ok_or_error do
    Enum.random([:ok, :error])
  end

  @spec ok_or_other_error() :: :ok | :other_error
  defp ok_or_other_error do
    Enum.random([:ok, :other_error])
  end
end
