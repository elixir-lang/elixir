defmodule GenEvent.Behaviour do
  @moduledoc false

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour :gen_event

      @doc false
      def init(args) do
        {:ok, args}
      end

      @doc false
      def handle_event(_event, state) do
        {:ok, state}
      end

      @doc false
      def handle_call(_request, state) do
        {:ok, :ok, state}
      end

      @doc false
      def handle_info(_msg, state) do
        {:ok, state}
      end

      @doc false
      def terminate(reason, state) do
        :ok
      end

      @doc false
      def code_change(_old, state, _extra) do
        {:ok, state}
      end

      defoverridable [init: 1, 
                      handle_event: 2,
                      handle_call: 2, handle_info: 2,
                      terminate: 2, code_change: 3]
    end
  end
end
