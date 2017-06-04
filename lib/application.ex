defmodule Application do
  @moduledoc "Application startup and shutdown functionality"

  defdelegate [load: 1, load: 2,
               get_application: 0, get_application: 1,
               loaded_applications: 0, permit: 2, 
               stop: 1, takeover: 2, unload: 1], to: :application

  defdelegate [running_applications: 0, running_applications: 1], to: :application, as: :which_applications
  
  @doc """
  `Application.start/1` and `Application.start/2` are used to start applications. They are similar to `:application.start/1` and
  `:application.start/2`, however, they support starting dependencies.
  
  Allowed options (`start/2`):

  * `:dependenices` (boolean): should dependencies be recursively started if required. Returns `{:error, {:not_started, required_app}}` if 
    `false`. Default: `true`
  * `:type` (atom): restart type (:temporary, :transient, :permanent). Default: `temporary`
    Excerpt from Erlang/OTP documentation:
     * If a permanent application terminates, all other applications and the entire Erlang node are also terminated.
     * If a transient application terminates with Reason == normal, this is reported but no other applications are terminated. 
       If a transient application terminates abnormally, all other applications and the entire Erlang node are also terminated.
     * If a temporary application terminates, this is reported but no other applications are terminated.

  """
  def start(application), do: start(application, [dependencies: true])
  def start(application, options) do
    unless options[:type], do: options = Keyword.put(options, :type, :temporary)

    case {options[:dependencies], :application.start(application, options[:type])} do
      {true, {:error, {:not_started, dep}}} ->
        start(dep, options)
        start(application, options)
      {_, {:error, {:already_started, _}}} -> :ok
      {_, other} -> other
    end
  end
  
end