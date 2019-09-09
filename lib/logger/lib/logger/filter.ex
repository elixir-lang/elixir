defmodule Logger.Filter do
  @moduledoc false

  @doc """
  Filter out SASL reports if `sasl_enabled?` is `false`
  """
  def sasl(sasl_enabled?) do
    filter =
      fn
        %{meta: %{domain: [:otp, :sasl | _]}}, false -> :stop
        %{meta: %{domain: [:supervisor_report | _]}}, false -> :stop
        log, _sasl_enabled -> log
      end

    {filter, sasl_enabled?}
  end

  @doc """
  Filter out logs if current process opted out of log reports
  """
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
