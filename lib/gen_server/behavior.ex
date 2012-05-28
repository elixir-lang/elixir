defmodule GenServer.Behavior do
  @doc """
  By using this module, you get default GenServer callbacks
  for `handle_call`, `handle_info`, `handle_cast`, `terminate`
  and `code_change`. `init` still needs to be implemented by the
  developer. Since these functions are defined as overridable,
  they can be partially customized and have a global clause
  that simply invokes `super`. See `ExUnit.Server` for some
  code examples.

  This module also tags the behavior as :gen_server. For more
  information on gen_server, please refer to the Erlang
  documentation:

  http://www.erlang.org/doc/man/gen_server.html
  http://www.erlang.org/doc/design_principles/gen_server_concepts.html

  """
  defmacro __using__(_) do
    quote do
      @behavior :gen_server

      def handle_call(_request, _from, state) do
        { :reply, :undef, state }
      end

      def handle_info(_msg, state) do
        { :noreply, state }
      end

      def handle_cast(_msg, state) do
        { :noreply, state }
      end

      def terminate(reason, state) do
        :error_logger.error_report('#{inspect __MODULE__} crashed:\n#{inspect reason}')
        :error_logger.error_report('#{inspect __MODULE__} snapshot:\n#{inspect state}')
        :ok
      end

      def code_change(_old, state, _extra) do
        { :ok, state }
      end

      defoverridable [handle_call: 3, handle_info: 2, handle_cast: 2, terminate: 2, code_change: 3]
    end
  end
end