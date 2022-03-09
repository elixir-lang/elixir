defmodule Logger.Filter do
  @moduledoc false

  @doc """
  Filter messages logged via `Logger` module when not logging OTP reports.
  """
  def filter_elixir_domain(%{meta: meta}, _extra) do
    case meta do
      %{domain: [:elixir | _]} -> :ignore
      _ -> :stop
    end
  end

  @doc """
  Filter out logs if current process opted out of certain levels.
  """
  def process_level(%{level: level}, _extra) do
    if Logger.process_allowed?(level, self()) do
      :ignore
    else
      :stop
    end
  end

  @doc """
  A filter that waits until Logger exits and then removes itself.
  """
  def silence_logger_exit(
        %{
          msg:
            {:report,
             %{
               label: {:application_controller, :exit},
               report: [application: :logger, exited: :stopped] ++ _
             }}
        },
        _extra
      ) do
    :stop
  end

  def silence_logger_exit(_message, _extra) do
    :ignore
  end
end
