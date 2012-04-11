# By using this module, you get default GenServer callbacks
# for handle_call, handle_info, handle_cast, terminate and
# code_change. init still needs to be implemented by the
# developer. This module also tags the behavior as :gen_server.
defmodule GenServer.Behavior do
  defmacro __using__(_, _) do
    quote do
      @behavior :gen_server

      @overridable true
      def handle_call(_request, _from, state) do
        { :reply, :undef, state }
      end

      @overridable true
      def handle_info(_msg, state) do
        { :noreply, state }
      end

      @overridable true
      def handle_cast(_msg, state) do
        { :noreply, state }
      end

      @overridable true
      def terminate(reason, state) do
        IO.puts "[FATAL] #{__MODULE__} crashed:\n#{inspect reason}"
        IO.puts "[FATAL] #{__MODULE__} snapshot:\n#{inspect state}"
        :ok
      end

      @overridable true
      def code_change(_old, state, _extra) do
        { :ok, state }
      end
    end
  end
end
