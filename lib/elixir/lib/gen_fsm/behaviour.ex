defmodule GenFSM.Behaviour do
  @moduledoc """
  This module is a convenience for defining GenFSM callbacks in Elixir.

  A finite state machine (FSM) is responsible for reacting to events received;
  GenFSM is an OTP behaviour that encapsulates common FSM
  functionalities.

  ## Example

  Below is an example of a GenFSM that runs a very simple minded
  coffee vending machine (CVM). The CVM treats all coins the same. If
  you press the request button then the CVM will brew coffee, if you
  have paid enough coins; if not, it will wait until you have inserted
  enough coins and then it will instantly brew the coffee, since you
  had already pressed the request button. As we told you - a very
  simple minded CVM! And greedy too. If you insert more coins than you
  need it will gladly eat them until you press the request button.

  We will leave it to the service-minded reader to improve the way the CVM
  works - we hereby declare a full disclaimer for any lawsuits that the
  behaviour of the CVM in its original state might encur.


      defmodule MyFsm do
        use GenFSM.Behaviour

          # keeping track of what is going on inside the CVM.
          # 3 is the target price for a cup of coffee
          defrecord StateData, coins: 0, price: 3

          #
          # API functions
          #

          def start_link() do
            :gen_fsm.start_link({:local, :cvm}, __MODULE__, [], [])
          end

          def insert_coin() do
            :gen_fsm.send_event(:cvm, :coin)
          end

          def request_coffee() do
            :gen_fsm.send_event(:cvm, :request_coffee)
          end

          #
          # Callbacks
          #

          def init(_args) do
            { :ok, :short_paid, StateData.new }
          end


          def short_paid(:coin, state_data = StateData[coins: c, price: p]) 
           when c + 1 < p do
            { :next_state, :short_paid, state_data.coins(c + 1) }
          end

          def short_paid(:coin, state_data) do
            { :next_state, :paid_in_full, state_data.update_coins(&1 + 1) }
          end

          def short_paid(:request_coffee, state_data) do
            { :next_state, :requested_short_paid, state_data }
          end


          def requested_short_paid(:request_coffee, state_data) do
            {:next_state, :requested_short_paid, state_data }
          end

          def requested_short_paid(:coin, state_data=StateData[coins: c, price: p]) 
           when c+1 < p do
            { :next_state, :requested_short_paid, state_data.coins(c + 1) }
          end

          def requested_short_paid(:coin, _state_data) do
            IO.puts "Here's your coffee!"
            { :next_state, :short_paid, StateData.new }
          end


          def paid_in_full(:coin, state_data) do
            { :next_state, :paid_in_full, state_data.update_coins(&1 + 1) }
          end

          def paid_in_full(:request_coffee, _state_data) do
            IO.puts "Here's your coffee!"
            { :next_state, :short_paid, StateData.new }
          end
        end


      { :ok, _pid } = MyFsm.start_link()

      MyFsm.insert_coin
      #=> :ok
      MyFsm.insert_coin
      #=> :ok

      MyFsm.request_coffee
      #=> :ok

      MyFsm.insert_coin
      #=> :ok
      #=> Here's your coffee!

  Notice we never call the GenFSM callbacks directly; they are called by
  OTP whenever we interact with the server throught the API. `send_event` is
  asynchronous, whereas `sync_send_event` is synchronous. For
  a GenFSM, the different values a callback can return depend
  on the type of callback.

  State handling returns for `send_event` callbacks:

      { :next_state, next_state_name, new_state_data }
      { :next_state, next_state_name, new_state_data, timeout }
      { :next_state, next_state_name, new_state_data, :hibernate }
      { :stop, reason, new_state_data }

  State handling returns for `sync_send_event` callbacks:

      { :reply, reply, next_state_name, new_state_data }
      { :reply, reply, next_state_name, new_state_data, timeout }
      { :reply, reply, next_state_name, new_state_data, :hibernate }
      { :next_state, next_state_name, new_state_data }
      { :next_state, next_state_name, new_state_data, timeout }
      { :next_state, next_state_name, new_state_data, :hibernate }
      { :stop, reason, reply, new_state_data }
      { :stop, reason, new_state_date }

  There are 6 callbacks required to be implemented in a GenFsm plus 1
  or 2 for each state. The `GenFSM.Behaviour` module defines
  `handle_sync_event`, `handle_info`, `terminate` and `code_change`
  for you. The list of callbacks are:

  * `init(args)` - invoked when the FSM is started;
  * `handle_sync_event(event, from, state_name, state_data)` - invoked to 
  handle `sync_send_all_state_event` messages;
  * `handle_event(event, state_name, state_data)` - invoked to handle 
  `send_all_state_event` messages;
  * `handle_info(msg, state_name, state_data)` - handle all other 
  messages which are normally received by processes;
  * `terminate(reason, state_name, state_data)` - called when the FSM 
  is about to terminate, useful for cleaning up;
  * `code_change(old_vsn, state, extra)` - called when the application 
  code is being upgraded live (hot code swap);

  Unlike `GenServer` and `GenEvent`, the callback `init/1` is not
  implemented by default, as it requires the next state to be returned.

  For each state you need to define either or both of these:

  * `state_name(event, state_data)` - invoked to handle 
  `send_event` messages;
  * `state_name(event, from, state_data)`- invoked to handle 
  `sync_send_event` messages;

  If you send asynchronous events you only need to implement the
  `state_name/2` variant and vice-versa for synchronous events and
  `state_name/3`. Keep in mind that if you mix `send_event` and
  `sync_send_event` the best thing to do is to implement both
  callbacks for all states.

  Starting and sending messages to the GenFSM is done via Erlang's
  `:gen_fsm` module. For more information, please refer to the
  following:

  * http://www.erlang.org/doc/man/gen_fsm.html
  * http://www.erlang.org/doc/design_principles/fsm.html
  * http://learnyousomeerlang.com/finite-state-machines
  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behavior :gen_fsm

      @doc false
      def handle_event(_event, state_name, state_data) do
        { :next_state, state_name, state_data }
      end

      @doc false
      def handle_sync_event(_event, from, state_name, state_data) do
        { :reply, :default_implementation, state_name, state_data }
      end

      @doc false
      def handle_info(_msg, state_name, state_data) do
        { :next_state, state_name, state_data }
      end

      @doc false
      def terminate(_reason, _state_name, _state_data) do
        :ok
      end

      @doc false
      def code_change(_old, state_name, state_data, _extra) do
        { :ok, state_name, state_data }
      end

      defoverridable [handle_event: 3, handle_sync_event: 4,
                      handle_info: 3, terminate: 3, code_change: 4]
    end
  end
end
