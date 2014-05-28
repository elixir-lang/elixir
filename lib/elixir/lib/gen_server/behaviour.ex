defmodule GenServer.Behaviour do
  @moduledoc false

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour :gen_server

      @doc false
      def init(args) do
        {:ok, args}
      end

      @doc false
      def handle_call(request, _from, state) do
        {:stop, {:bad_call, request}, state}
      end

      @doc false
      def handle_info(_msg, state) do
        {:noreply, state}
      end

      @doc false
      def handle_cast(msg, state) do
        {:stop, {:bad_cast, msg}, state}
      end

      @doc false
      def terminate(_reason, _state) do
        :ok
      end

      @doc false
      def code_change(_old, state, _extra) do
        {:ok, state}
      end

      defoverridable [init: 1, handle_call: 3, handle_info: 2,
        handle_cast: 2, terminate: 2, code_change: 3]
    end
  end
end
