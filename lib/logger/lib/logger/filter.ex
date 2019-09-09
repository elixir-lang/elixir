defmodule Logger.Filter do
  def sasl(sasl_enabled?) do
    filter =
      fn
        %{meta: %{domain: [:otp, :sasl | _]}}, false -> :stop
        %{meta: %{domain: [:supervisor_report | _]}}, false -> :stop
        log, _sasl_enabled -> log
      end

    {filter, sasl_enabled?}
  end

  def process_disabled() do
    {fn log, _extra ->
      if Logger.enabled?(self()) do
        log
      else
        :stop
      end
    end, nil}
  end
end
