defmodule Logger.Filter do
  @moduledoc false

  @doc """
  Implement the default translation and handling of reports.
  """
  def translator(%{domain: [:elixir | _]}, %{otp: false}), do: :stop
  def translator(%{meta: %{domain: [:otp, :sasl | _]}}, %{sasl: false}), do: :stop
  def translator(%{meta: %{domain: [:supervisor_report | _]}}, %{sasl: false}), do: :stop
  def translator(%{msg: {:string, _}}, _config), do: :ignore

  def translator(%{msg: msg, level: level} = event, %{translators: translators}) do
    %{level: min_level} = :logger.get_primary_config()

    try do
      case msg do
        {:report, %{label: label, report: report} = complete}
        when map_size(complete) == 2 ->
          translate(translators, min_level, level, :report, {label, report})

        {:report, %{label: {:error_logger, _}, format: format, args: args}} ->
          translate(translators, min_level, level, :format, {format, args})

        {:report, report} ->
          translate(translators, min_level, level, :report, {:logger, report})

        {format, args} ->
          translate(translators, min_level, level, :format, {format, args})
      end
    rescue
      e ->
        chardata = [
          "Failure while translating Erlang's logger event\n",
          Exception.format(:error, e, __STACKTRACE__)
        ]

        %{event | msg: {:string, chardata}}
    else
      :none -> :ignore
      :skip -> :stop
      {:ok, chardata} -> %{event | msg: {:string, chardata}}
      {:ok, char, meta} -> %{event | msg: {:string, char}, meta: Enum.into(meta, event.meta)}
    end
  end

  defp translate([{mod, fun} | t], min_level, level, kind, data) do
    with :none <- apply(mod, fun, [min_level, level, kind, data]) do
      translate(t, min_level, level, kind, data)
    end
  end

  defp translate([], _min_level, _level, _kind, _data) do
    :none
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
    :logger.remove_primary_filter(:silence_logger_exit)
    :stop
  end

  def silence_logger_exit(_message, _extra) do
    :ignore
  end
end
