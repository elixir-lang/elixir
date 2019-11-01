defmodule Logger.Filter do
  @moduledoc false

  @doc """
  Filter messages logged via `Logger` module when not logging OTP reports
  """
  def elixir_domain(%{meta: meta}, action) when action in [:stop, :ignore] do
    case meta do
      %{domain: [:elixir | _]} -> action
      _ -> inverse_action(action)
    end
  end

  @doc """
  Filter out logs if current process opted out of log reports
  """
  def process_disabled(_log, _extra) do
    if Logger.enabled?(self()) do
      :ignore
    else
      :stop
    end
  end

  defp inverse_action(:ignore), do: :stop
  defp inverse_action(:stop), do: :ignore
end
