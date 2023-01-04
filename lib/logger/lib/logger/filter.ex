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
    process_level = Logger.get_process_level(self())

    if process_level != nil and :logger.compare_levels(level, process_level) == :lt do
      :stop
    else
      :ignore
    end
  end

  @doc """
  Filter Logger exits and then removes itself.
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
