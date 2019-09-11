defmodule Logger.Filter do
  @moduledoc false

  @doc """
  Filter out logs if current process opted out of log reports
  """
  def process_disabled() do
    {fn _log, _extra ->
       if Logger.enabled?(self()) do
         :ignore
       else
         :stop
       end
     end, nil}
  end
end
