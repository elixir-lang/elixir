# By using this module, you get default GenServer callbacks
# for handle_call, handle_info, handle_cast, terminate and
# code_change. init still needs to be implemented by the
# developer. This module also tags the behavior as :gen_server.
defmodule GenServer::Behavior do
  defmacro __using__(_, _) do
    quote do
      @behavior :gen_server
      defforward [handle_call: 3, handle_info: 2, handle_cast: 2, terminate: 2, code_change: 3], to: unquote(__MODULE__)
    end
  end

  def handle_call(_module, _request, _from, state) do
    { :reply, :undef, state }
  end

  def handle_info(_module, _msg, state) do
    { :noreply, state }
  end

  def handle_cast(_module, _msg, state) do
    { :noreply, state }
  end

  def terminate(module, reason, state) do
    IO.puts "[FATAL] #{module} crashed:\n#{inspect reason}"
    IO.puts "[FATAL] #{module} snapshot:\n#{inspect state}"
    :ok
  end

  def code_change(_module, _old, state, _extra) do
    { :ok, state }
  end
end