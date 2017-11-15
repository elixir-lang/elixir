defmodule Dialyzer.With do
  def with_else do
    with :ok <- ok_or_error(),
         :ok <- ok_or_other_error(),
         :ok <- ok_or_tuple_error(),
         :ok <- ok_or_tuple_list_error() do
      :ok
    else
      :error ->
        :error

      :other_error ->
        :other_error

      {:error, msg} when is_list(msg) or is_tuple(msg) ->
        :error

      {:error, msg} when is_list(msg) when is_tuple(msg) ->
        :error

      {:error, _msg} ->
        :error
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

  @spec ok_or_tuple_error() :: :ok | {:error, :err}
  defp ok_or_tuple_error do
    Enum.random([:ok, {:error, :err}])
  end

  @spec ok_or_tuple_list_error() :: :ok | {:error, [:err]}
  defp ok_or_tuple_list_error do
    Enum.random([:ok, {:error, [:err]}])
  end
end
