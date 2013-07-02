defmodule GenEvent.Behaviour do
  @moduledoc """
  This module is a convenience for defining GenEvent callbacks in Elixir.

  GenEvent is an OTP behaviour that encapsulates event handling functionality.

  ## Example

  Below is an example of a GenEvent that stores notifications
  until they are fetched:

      defmodule MyEventHandler do
        use GenEvent.Behaviour

        # Callbacks

        def init(_) do
          { :ok, [] }
        end

        def handle_event({:notification, x}, notifications) do
          { :ok, [x|notifications] }
        end

        def handle_call(:notifications, notifications) do
          {:ok, Enum.reverse(notifications), []}
        end

      end

      { :ok, pid } = :gen_event.start_link
      #=> {:ok,#PID<0.42.0>}

      :gen_event.add_handler(pid, MyEventHandler, [])
      #=> :ok

      :gen_event.notify(pid, {:notification, 1})
      #=> :ok

      :gen_event.notify(pid, {:notification, 2})
      #=> :ok
      
      :gen_event.call(pid, MyEventHandler, :notifications)
      #=> [1, 2]

      :gen_event.call(pid, MyEventHandler, :notifications)
      #=> []

  Notice we never call the server callbacks directly, they are called
  by OTP whenever we interact with the server. 

  Starting and sending messages to the GenEvent is done
  via Erlang's `:gen_event` module. For more information,
  please refer to the following:

  * http://www.erlang.org/doc/man/gen_event.html
  * http://learnyousomeerlang.com/event-handlers
  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour :gen_event

      @doc false
      def init(args) do
        { :ok, args }
      end

      @doc false
      def handle_event(_event, state) do
        { :ok, state }
      end

      @doc false
      def handle_call(_request, state) do
        { :ok, :ok, state }
      end

      @doc false
      def handle_info(_msg, state) do
        { :ok, state }
      end

      @doc false
      def terminate(reason, state) do
        :ok
      end

      @doc false
      def code_change(_old, state, _extra) do
        { :ok, state }
      end

      defoverridable [init: 1, 
                      handle_event: 2,
                      handle_call: 2, handle_info: 2,
                      terminate: 2, code_change: 3]
    end
  end
end
