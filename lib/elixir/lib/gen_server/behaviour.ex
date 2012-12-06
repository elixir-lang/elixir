defmodule GenServer.Behaviour do
  @moduledoc """
  This module is a convenience to define GenServer callbacks in Elixir.

  A server is responsible to react to messages received from client
  and a GenServer is an OTP behaviour that encapsulates common server
  functionalities.

  ## Example

  Bellow follows an example of a GenServer that push and pop items
  into a stack:

      defmodule MyServer do
        use GenServer.Behaviour

        # Callbacks

        def handle_call(:pop, _from, [h|t]) do
          { :reply, h, t }
        end

        def handle_call(_request, _from, _config) do
          # Call the default implementation from GenServer.Behaviour
          super
        end

        def handle_cast({ :push, item }, config) do
          { :noreply, [item|config] }
        end

        def handle_cast(_request, _config) do
          super
        end
      end

      { :ok, pid } = :gen_server.start_link(MyServer, [:hello], [])

      :gen_server.call(pid, :pop)
      #=> :hello

      :gen_server.cast(pid, { :push, :world })
      #=> :ok

      :gen_server.call(pid, :pop)
      #=> :world

  Notice we never call the server callbacks directly, they are called
  by OTP whenever we interact with the server. **cast** messages are
  asynchronous while **call** ones are synchronous.  In the case of
  GenServer's, there are 8 different values a callback such as
  `handle_call` or `handle_cast` can return:

      { :reply, reply, new_state }
      { :reply, reply, new_state, timeout }
      { :reply, reply, new_state, :hibernate }
      { :noreply, new_state }
      { :noreply, new_state, timeout }
      { :noreply, new_state, :hibernate }
      { :stop, reason, new_state }
      { :stop, reason, reply, new_state }

  There are 6 callbacks required to be implemented in a GenServer. The
  `GenServer.Behaviour` module defines all of them automatically, but
  allows us to customize the ones we need. The list of callbacks are:

  * `init(args)` - invoked when the server is started;
  * `handle_call(msg, from, state)` - invoked to handle call messages;
  * `handle_cast(msg, state)` - invoked to handle cast messages;
  * `handle_info(msg, state)` - handle all other messages which are
     normally received by processes;
  * `terminate(reason, state)` - called when the server is about to
     terminate, useful for cleaning up;
  * `code_change(old_vsn, state, extra)` - called when the application
    code is being upgraded live (hot code swap);

  Starting and sending messages to the gen_server is done
  via Erlang's `:gen_server` module. For more information,
  please refer to the following:

  http://www.erlang.org/doc/man/gen_server.html
  http://www.erlang.org/doc/design_principles/gen_server_concepts.html
  http://learnyousomeerlang.com/clients-and-servers
  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behavior :gen_server

      def init(args) do
        { :ok, args }
      end

      def handle_call(_request, _from, state) do
        { :noreply, state }
      end

      def handle_info(_msg, state) do
        { :noreply, state }
      end

      def handle_cast(_msg, state) do
        { :noreply, state }
      end

      def terminate(reason, state) do
        :ok
      end

      def code_change(_old, state, _extra) do
        { :ok, state }
      end

      defoverridable [init: 1, handle_call: 3, handle_info: 2,
        handle_cast: 2, terminate: 2, code_change: 3]
    end
  end
end