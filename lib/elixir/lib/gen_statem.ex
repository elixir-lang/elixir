defmodule GenStatem do
  @moduledoc """
  A behaviour module for implementing the generic state machine
  Documentation for `GenStatem`.

  # Features

  - [Co-located state code](#state_functions)
  - [Arbitrary term state](#handle_event_function)
  - [Event postponing](#event-postponing)
  - [Self-generated events](#event-insertion)
  - [State time-out](`t:state_timeout/0`)
  - [Multiple generic named time-outs](`t:generic_timeout/0`)
  - [Absolute time-out time](`t:timeout_option/0`)
  - [Automatic state enter calls](#state-enter-calls)
  - [Reply from other state than the request](#reply-to-a-call), traceable with
    [`:sys`](`:sys`)
  - [Multiple replies](#reply-to-a-call), traceable with [`:sys`](`:sys`)
  - [Changing the callback module](#change_callback_module)

  Two [_callback modes_](`t:callback_mode/0`) are supported:

  - `state_functions` - for finite-state machine which requires the state
    to be an atom and uses that state
    as the name of the current callback function, arity 3.
  - `handle_event_function` - that allows the state to be any term
    and that uses `c:handle_event/4` as callback function for all states.

  # Callback module

  A `GenStatem` assumes all specific parts to be located
  in a callback module exporting a predefined set of functions.
  The relationship between the behavior functions
  and the callback functions is as follows:

  ```
  GenStatem module               Callback module
  -----------------              ---------------
  GenStatem.start
  GenStatem.start_monitor
  GenStatem.start_link    -----> Module.init/1

  Server start or code change
                          -----> Module.callback_mode/0
                          selects callback mode

  GenStatem.stop
  Supervisor exit
  Callback failure        -----> Module.terminate/3

  GenStatem.call
  GenStatem.cast
  GenStatem.send_request
  :erlang.send
  :erlang.'!'             -----> Module.state_name/3
                       or -----> Module.handle_event/4
                       depending on callback mode

  Release upgrade/downgrade
  (code change)
                          -----> Module.code_change/4
  ```

  # State callback {: #state-callback }

  The _state callback_ for a specific [state](`t:state/0`) in a `GenStatem`
  is the callback function that is called for all events in this state.
  It is selected depending on which [_callback mode_](`t:callback_mode/0`)
  that the callback module defines with the callback function
  [`Module.callback_mode/0`](`c:callback_mode/0`).

  [](){: #state_functions }
  When the [_callback mode_](`t:callback_mode/0`) is `state_functions`,
  the state must be an atom and is used as the _state callback_ name;
  see [`Module.state_name/3`](`c:state_name/3`).  This co-locates all code
  for a specific state in one function as the `GenStatem` engine branches
  depending on state name.  Note the fact that the callback function
  [`Module.terminate/3`](`c:terminate/3`) makes the state name `terminate`
  unusable in this mode.

  [](){: #handle_event_function }
  When the [_callback mode_](`t:callback_mode/0`) is `handle_event_function`,
  the state can be any term and the _state callback_ name is
  [`Module.handle_event/4`](`c:handle_event/4`). This makes it easy
  to branch depending on state or event as you desire. Be careful about
  which events you handle in which states so that you do not accidentally
  postpone an event forever creating an infinite busy loop.

  # Event types

  Events are of different [types](`t:event_type/0`),
  therefore the callback functions can know the origin of an event
  when handling it.  [External events](`t:external_event_type/0`) are
  `:call`,  `:cast`, and `:info`. Internal events are
  [`timeout`](`t:timeout_event_type/0`) and `:internal`.

  # Event handling

  When `GenStatem` receives a process message it is transformed
  into an event and the [_state callback_](#state-callback)
  is called with the event as two arguments: type and content. When the
  [_state callback_](#state-callback) has processed the event
  it returns to `GenStatem` which does a _state transition_. If this
  _state transition_ is to a different state, that is: `next_state !== state`,
  it is a _state change_.

  # Transition actions

  The [_state callback_](#state-callback) may return
  [_transition actions_](`t:action/0`) for `GenStatem` to execute
  during the _state transition_, for example to set a time-out
  or reply to a call.

  # Reply to a call {: #reply-to-a-call }

  See [`GenStatem:call/2,3`](#call-reply) about how to reply
  to a call.  A reply can be sent from any _state callback_,
  not just the one that got the request event.

  # Event postponing {: #event-postponing }

  One of the possible _transition actions_ is to postpone the current event.
  Then it will not be handled in the current state.  The `GenStatem` engine
  keeps a queue of events divided into postponed events and
  events still to process (not presented yet).  After a _state change_
  the queue restarts with the postponed events.

  The `GenStatem` event queue model is sufficient to emulate
  the normal process message queue with selective receive.
  Postponing an event corresponds to not matching it
  in a receive statement, and changing states corresponds to
  entering a new receive statement.

  # Event insertion {: #event-insertion }

  The [_state callback_](#state-callback) can insert
  events using the [_transition action_](`t:action/0`) `:next_event`,
  and such an event is inserted in the event queue as the next to call the
  [_state callback_](#state-callback) with. That is,
  as if it is the oldest incoming event. A dedicated `t:event_type/0`
  `:internal` can be used for such events making it possible to
  safely distinguish them from external events.

  Inserting an event replaces the trick of calling your own state handling
  functions that you often would have to resort to in, for example,
  `m:gen_fsm` to force processing an inserted event before others.

  > #### Note {: .info }
  >
  > If you postpone an event and (against good practice) directly call
  > a different _state callback_, the postponed event is not retried,
  > since there was no _state change_.
  >
  > Instead of directly calling a _state callback_, do a _state change_.
  > This makes the `GenStatem` engine retry postponed events.
  >
  > Inserting an event in a _state change_ also triggers
  > the new  _state callback_ to be called with that event
  > before receiving any external events.

  # State enter calls {: #state-enter-calls }

  The `GenStatem` engine can automatically make a special call to the
  [_state callback_](#state-callback) whenever a new state is
  entered; see `t:state_enter/0`. This is for writing code common
  to all state entries.  Another way to do it is to explicitly insert
  an event at the _state transition_, and/or to use a dedicated
  _state transition_ function, but that is something you will have to
  remember at every _state transition_ to the state(s) that need it.

  For the details of a _state transition_, see type `t:transition_option/0`.

  # Hibernation

  The `GenStatem` process can go into hibernation;
  see `proc_lib:hibernate/3`. It is done when
  a [_state callback_](#state-callback) or
  [`Module.init/1`](`c:init/1`) specifies `hibernate`
  in the returned [`Actions`](`t:enter_action/0`) list. This feature
  can be useful to reclaim process heap memory while the server
  is expected to be idle for a long time. However, use it with care,
  as hibernation can be too costly to use after every event;
  see `:erlang.hibernate/3`.

  There is also a server start option
  [`{hibernate_after, Timeout}`](`t:enter_loop_opt/0`)
  for [`start/3,4`](`start/3`), [`start_link/3,4`](`start_link/3`),
  [`start_monitor/3,4`](`start_monitor/3`),
  that may be used to automatically hibernate the server.

  # Callback failure

  If a callback function fails or returns a bad value,
  the `GenStatem` terminates.  However, an exception of class
  [`throw`](`:erlang.throw/1`) is not regarded as an error
  but as a valid return, from all callback functions.

  # System messages and the [`:sys`](`:sys`) module

  A `GenStatem` handles system messages as described in [`:sys`](`:sys`).
  The [`:sys`](`:sys`) module can be used for debugging a `GenStatem`.
  Replies sent through [_transition actions_](`t:action/0`)
  gets logged, but not replies sent through [`reply/1,2`](`reply/2`).

  # Trapping exit

  A `GenStatem` process, like all `gen_`\* behaviours,
  does not trap exit signals automatically;
  this must be explicitly initiated in the callback module
  (by calling [`process_flag(trap_exit, true)`](`:erlang.process_flag/2`)
  preferably from `c:init/1`.

  # Server termination

  If the `GenStatem` process terminates, e.g. as a result
  of a callback function returning `{stop, Reason}`, an exit signal
  with this `Reason` is sent to linked processes and ports.
  See [Processes](https://www.erlang.org/doc/system/ref_man_processes.html#error-handling)
  in the Reference Manual for details regarding error handling
  using exit signals.

  > #### Note {: .info }
  >
  > For some important information about distributed signals, see the
  > [_Blocking Signaling Over Distribution_
  > ](https://www.erlang.org/doc/system/ref_man_processes#blocking-signaling-over-distribution)
  > section in the _Processes_ chapter of the _Erlang Reference Manual_.
  > Blocking signaling can, for example, cause call time-outs in `GenStatem`
  > to be significantly delayed.

  # Bad argument

  Unless otherwise stated, all functions in this module fail if the specified
  `GenStatem` does not exist or if bad arguments are specified.

  ## Example

  The following example shows a simple pushbutton model
  for a toggling pushbutton implemented with
  [_callback mode_](`t:callback_mode/0`) `state_functions`.
  You can push the button and it replies if it went on or off,
  and you can ask for a count of how many times it has been pushed
  to switch on.

  ### Pushbutton State Diagram

  ```mermaid
  ---
  title: Pushbutton State Diagram
  ---
  stateDiagram-v2
      [*]  --> off
      off  --> on  : push<br/><ul><li>Increment count</li><li>Reply 'on'</li></ul>
      on   --> off : push<br/><ul><li>Reply 'off'</li></ul>
  ```

  Not shown in the state diagram:
  * The API function `push()` generates an event `push` of type `call`.
  * The API function `get_count()` generates an event `get_count`
    of type `call` that is handled in all states by replying with
    the current count value.
  * Unknown events are ignored and discarded.
  * There is boilerplate code for start, stop, terminate, code change,
    init, to set the callback mode to `state_functions`, etc...

  ### Pushbutton Code

  The following is the complete callback module file `pushbutton.erl`:

  ```elixir
  defmodule PushButton do
    use GenStatem

    defp name, do: :pushbutton_statem

    ## API. This example uses a registered name name()
    ## and does not link to the caller
    def start, do: GenStatem.start(__MODULE__, [], name: name())

    def push, do: GenStatem.call(name(), :push)

    def get_count, do: GenStatem.call(name(), :get_count)

    def stop, do: GenStatem.stop(name())

    ## Mandatory callback functions
    def init([]) do
      ## Set the initial state + data.  Data is used only as a counter.
      state = :off
      data = 0
      {:ok, state, data}
    end

    def callback_mode(), do: :state_functions

    def code_change(_old_vsn, old_state, old_data, _extra) do
  	{:ok, old_state, old_data}
    end

    ## Mandatory callback functions
    def terminate, do: :void

    ### state callback(s)

    def off({:call, from}, :push, data) do
      ## Go to 'on', increment count and reply
      ## that the resulting status is 'on'
      {:next_state, :on, data + 1,[{:reply, from, :on}]}
    end

    def off(event_type, event_content, data) do
      handle_event(event_type, event_content, data)
    end

    def on({:call, from}, :push, data) do
      ## Go to 'off' and reply that the resulting status is 'off'
      {:next_state, :off, data, [{:reply, from, :off}]}
    end

    def on(event_type, event_content, data) do
      handle_event(event_type, event_content, data)
    end

    ## Handle events common to all states
    defp handle_event({:call, from}, :get_count, data) do
      ## Reply with the current count
      {:keep_state, data,[{:reply, from, data}]}
    end
    defp handle_event(_event_type, _event_content, data) do
      ## Ignore all other events
      {:keep_state, data}
    end
  end
  ```

  The following is a shell session when running it:

  ```elixir
  iex(1)> PushButton.start
  {:ok, #PID<0.203.0>}
  iex(2)> PushButton.get_count
  0
  iex(3)> PushButton.push
  :on
  iex(4)> PushButton.get_count
  1
  iex(5)> PushButton.push
  :off
  iex(6)> PushButton.get_count
  1
  iex(7)> PushButton.stop
  :ok
  iex(8)> PushButton.push
  ** (GenStatem.GenError)
  Class: exit
  Reason: noproc
  MFArgs: GenStatem.call(:pushbutton_statem, :push, :infinity)
  Stacktrace:
      (stdlib 6.2.2) gen.erl:580: :gen.do_for_proc/2
      (GenStatem 0.1.0) lib/GenStatem.ex:1997: GenStatem.call/3
      (elixir 1.18.3) src/elixir.erl:386: :elixir.eval_external_handler/3
      (stdlib 6.2.2) erl_eval.erl:919: :erl_eval.do_apply/7
      (elixir 1.18.3) src/elixir.erl:364: :elixir.eval_forms/4
      (elixir 1.18.3) lib/module/parallel_checker.ex:120: Module.ParallelChecker.verify/1
      (iex 1.18.3) lib/iex/evaluator.ex:336: IEx.Evaluator.eval_and_inspect/3
      (iex 1.18.3) lib/iex/evaluator.ex:310: IEx.Evaluator.eval_and_inspect_parsed/3

      (GenStatem 0.1.0) lib/GenStatem.ex:2001: GenStatem.call/3
      iex:8: (file)
  ```

  To compare styles, here follows the same example using
  [_callback mode_](`t:callback_mode/0`) `handle_event_function`,
  or rather, the code to replace after function [`init/1`](`c:init/1`)
  of the `pushbutton.erl` example file above:

  ```elixir
  ### state callback(s)

  def handle_event({:call, from}, :push, :off, data) do
    ## Go to 'on', increment count and reply
    ## that the resulting status is 'on'
    {:next_state, :on, data + 1,[{:reply, from, :on}]}
  end

  def handle_event({:call, from}, :push, :on, data) do
    ## Go to 'off' and reply that the resulting status is 'off'
    {:next_state, :off, data, [{:reply, from, :off}]}
  end

  ##
  ## Event handling common to all states
  def handle_event({:call, from}, :get_count, state, data) do
    ## Reply with the current count
    {:next_state, state, data, [{:reply, from, data}]};
  end

  def handle_event(_event_type, _old_state, current_state, data) do
    ## Ignore all other events
    {:next_state, current_state, data}
  end
  ```

  ## See Also

  `m:GenEvent`, `m:GenServer`, `m:Supervisor`.
  """
  alias GenStatem.GenError

  @typedoc """
  A [`call`](`t:external_event_type/0`) event's reply destination.

  Destination to use when replying through, for example,
  the action [`{:reply, from, reply}`](`t:reply_action/0`)
  to a process that has called the `GenStatem` server
  using [`call/2,3`](`call/3`).
  """
  # Reply-to specifier for call
  @type from() :: {to :: pid(), tag :: reply_tag()}

  @typedoc """
  A handle that associates a reply to the corresponding request.
  """
  @opaque reply_tag() :: :gen.reply_tag()

  @typedoc """
  State name or state term.

  If the [_callback mode_](`t:callback_mode/0`) is `handle_event_function`,
  the state can be any term. After a _state change_ (`next_state !== state`),
  all postponed events are retried.

  Comparing two states for strict equality is assumed to be a fast operation,
  since for every _state transition_ the `GenStatem` engine has to deduce
  if it is a  _state change_.

  > #### Note {: .info }
  > The smaller the state term, in general, the faster the comparison.
  >
  > Note that if the "same" state term is returned for a state transition
  > (or a return action without a `next_state` field is used),
  > the comparison for equality is always fast because that can be seen
  > from the term handle.
  >
  > But if a newly constructed state term is returned,
  > both the old and the new state terms will have to be traversed
  > until an inequality is found, or until both terms
  > have been fully traversed.
  >
  > So it is possible to use large state terms that are fast to compare,
  > but very easy to accidentally mess up. Using small state terms is
  > the safe choice.
  """
  # For state_name/3 callback functions
  @type state() ::
          state_name()
          # For handle_event/4 callback function
          | term()

  @typedoc """
  State name in [_callback mode_](`t:callback_mode/0`) `state_functions`.

  If the [_callback mode_](`t:callback_mode/0`) is `state_functions`,
  the state must be an atom. After a _state change_ (`next_state !== state`),
  all postponed events are retried. Note that the state `terminate`
  is not possible to use since it would collide with the optional
  callback function [`Module.terminate/3`](`c:terminate/3`).
  """
  @type state_name() :: atom()

  @typedoc """
  Generic state data for the server.

  A term in which the state machine implementation is to store
  any server data it needs. The difference between this and the `t:state/0`
  itself is that a change in this data does not cause postponed events
  to be retried. Hence, if a change in this data would change
  the set of events that are handled, then that data item
  should be part of the `t:state/0` instead.
  """
  @type data() :: term()

  @typedoc """
  All event types: [external](`t:external_event_type/0`),
  [time-out](`t:timeout_event_type/0`), or `internal`.

  `internal` events can only be generated by the state machine itself
  through the _transition action_ [`next_event`](`t:action/0`).
  """
  @type event_type() ::
          external_event_type() | timeout_event_type() | :internal

  @typedoc """
  Event from a [call](`call/3`), [cast](`cast/2`),
  or regular process message; "info".

  Type `{:call, from}` originates from the API functions
  [`call/2,3`](`call/3`) or `send_request/2`. The event contains
  [`From`](`t:from/0`), which is whom to reply to
  by a `t:reply_action/0` or [`reply/2,3`](`reply/2`) call.

  Type `cast` originates from the API function `cast/2`.

  Type `info` originates from regular process messages
  sent to the `GenStatem` process.
  """
  @type external_event_type() ::
          {:call, from :: from()} | :cast | :info

  @typedoc """
  [Event time-out](`t:event_timeout/0`),
  [generic time-out](`t:generic_timeout/0`),
  or [state time-out](`t:state_timeout/0`).

  The time-out event types that the state machine can generate
  for itself with the corresponding `t:timeout_action/0`s:

  | Time-out type     | Action                         | Event type        |
  |-------------------|--------------------------------|-------------------|
  | Event time-out    | `{:timeout, time, ...}`         | `:timeout`         |
  | Generic time-out  | `{{:timeout, name}, time, ...}` | `{:timeout, name}` |
  | State time-out    | `{:state_timeout, time, ...}`   | `:state_timeout`   |

  In short; the action to set a time-out with
  [`event_type`](`t:timeout_event_type/0`) is `{event_type, time, ...}`.
  """
  @type timeout_event_type() ::
          :timeout | {:timeout, name :: term()} | :state_timeout

  @typedoc """
  Event payload from the event's origin, delivered to
  the [_state callback_](#state-callback).

  See [`event_type`](`t:event_type/0`) that describes the origins of
  the different event types, which is also where the event's content
  comes from.
  """
  @type event_content() :: term()

  @typedoc """
  Return value from [`Module.callback_mode/0`](`c:callback_mode/0`).

  This is the return type from
  [`Module.callback_mode/0`](`c:callback_mode/0`)
  which selects [_callback mode_](`t:callback_mode/0`)
  and whether to do [_state enter calls_](`t:state_enter/0`),
  or not.
  """
  @type callback_mode_result() ::
          callback_mode() | [callback_mode() | state_enter()]

  @typedoc """
  One function per state or one common event handler.

  The _callback mode_ is selected with the return value from
  [`Module.callback_mode/0`](`c:callback_mode/0`):

  - **`state_functions`** - The state must be of type `t:state_name/0`
    and one callback function per state, that is,
    [`Module.state_name/3`](`c:state_name/3`), is used.

  - **`handle_event_function`** - The state can be any term and the callback
    function [`Module.handle_event/4`](`c:handle_event/4`)
    is used for all states.

  The function [`Module.callback_mode/0`](`c:callback_mode/0`) is called
  when starting the `GenStatem`, after code change and after changing
  the callback module with any of the actions
  [`change_callback_module`](#change_callback_module),
  [`push_callback_module`](#push_callback_module),
  or [`pop_callback_module`](#pop_callback_module).
  The result is cached for subsequent calls to
  [_state callbacks_](#state-callback).
  """
  @type callback_mode() :: :state_functions | :handle_event_function

  @typedoc """
  [_Callback mode_](`t:callback_mode/0`) modifier
  for _state enter calls_: the atom `:state_enter`.

  Both _callback modes_ can use _state enter calls_,
  and this is selected by adding this `:state_enter` flag
  to the [_callback mode_](`t:callback_mode/0`) return value from
  [`Module.callback_mode/0`](`c:callback_mode/0`).

  If [`Module.callback_mode/0`](`c:callback_mode/0`) returns
  a list containing `:state_enter`, the `GenStatem` engine will,
  at every _state change_, that is; `next_state !== current_state`,
  call the [_state callback_](#state-callback) with arguments
  `(enter, OldState, Data)` or `(enter, OldState, State, Data)`,
  depending on the [_callback mode_](`t:callback_mode/0`).

  This may look like an event but is really a call performed
  after the previous [_state callback_](#state-callback) returned,
  and before any event is delivered to the new
  [_state callback_](#state-callback).
  See [`Module.state_name/3`](`c:state_name/3`) and
  [`Module.handle_event/4`](`c:handle_event/4`).  A _state enter call_
  may be repeated without doing a _state change_ by returning
  a [`repeat_state`](`t:state_callback_result/2`) or
  [`repeat_state_and_data`](`t:state_callback_result/2`) action
  from the _state callback_.

  If [`Module.callback_mode/0`](`c:callback_mode/0`) does not return
  a list containing `:state_enter`, no _state enter calls_ are done.

  If [`Module.code_change/4`](`c:code_change/4`) should transform the state,
  it is regarded as a state rename and not a _state change_,
  which will not cause a _state enter call_.

  Note that a _state enter call_ **will** be done right before entering
  the initial state, which may be seen as a state change from no state
  to the initial state. In this case `OldState =:= State`,
  which cannot happen for a subsequent state change,
  but will happen when repeating the _state enter call_.
  """
  @type state_enter() :: :state_enter

  @typedoc """
  _State transition_ options set by [actions](`t:action/0`).

  These determine what happens during the _state transition_.
  The _state transition_ takes place when the
  [_state callback_](#state-callback) has processed an event
  and returns. Here are the sequence of steps for a _state transition_:

  1. All returned [actions](`t:action/0`) are processed
     in order of appearance.  In this step all replies generated
     by any `t:reply_action/0` are sent.  Other actions set
     `t:transition_option/0`s that come into play in subsequent steps.

  2. If [_state enter calls_](`t:state_enter/0`) are used,
     it is either the initial state or one of the callback results
     [`repeat_state`](`t:state_callback_result/2`) or
     [`repeat_state_and_data`](`t:state_callback_result/2`) is used the
     `GenStatem` engine calls the current _state callback_ with arguments
     [`(enter, State, Data)`](`t:state_enter/0`) or
     [`(enter, State, State, Data)`](`t:state_enter/0`) (depending on
     [_callback mode_](`t:callback_mode/0`)) and when it returns
     starts again from the top of this sequence.

     If [_state enter calls_](`t:state_enter/0`) are used,
     and the state changes, the `GenStatem` engine calls
     the new _state callback_ with arguments
     [`(enter, OldState, Data)`](`t:state_enter/0`) or
     [`(enter, OldState, State, Data)`](`t:state_enter/0`) (depending on
     [_callback mode_](`t:callback_mode/0`)) and when it returns
     starts again from the top of this sequence.

  3. If `t:postpone/0` is `true`, the current event is postponed.

  4. If this is a _state change_, the queue of incoming events is reset
     to start with the oldest postponed.

  5. All events stored with `t:action/0` `next_event` are inserted
     to be processed before previously queued events.

  6. Time-out timers `t:event_timeout/0`, `t:generic_timeout/0` and
     `t:state_timeout/0` are handled.  Time-outs with zero time
     are guaranteed to be delivered to the state machine
     before any external not yet received event so if there is
     such a time-out requested, the corresponding time-out zero event
     is enqueued as the newest received event; that is after
     already queued events such as inserted and postponed events.

     Any event cancels an `t:event_timeout/0` so a zero time event time-out
     is only generated if the event queue is empty.

     A _state change_ cancels a `t:state_timeout/0` and any new transition
     option of this type belongs to the new state, that is;
     a `t:state_timeout/0` applies to the state the state machine enters.

  7. If there are enqueued events the
     [_state callback_](#state-callback) for the possibly
     new state is called with the oldest enqueued event, and we start again
     from the top of this sequence.

  8. Otherwise the `GenStatem` goes into `receive` or hibernation
     (if `t:hibernate/0` is `true`) to wait for the next message.
     In hibernation the next non-system event awakens the `GenStatem`,
     or rather the next incoming message awakens the `GenStatem`,
     but if it is a system event it goes right back into hibernation.
     When a new message arrives the
     [_state callback_](#state-callback) is called with
     the corresponding event, and we start again
     from the top of this sequence.

  > #### Note {: .info }
  > The behaviour of a time-out zero (a time-out with time `0`)
  > differs subtly from Erlang's `receive ... after 0 ... end`.
  >
  > The latter receives one message if there is one,
  > while using the `t:timeout_action/0` `{timeout, 0}` does not
  > receive any external event.
  >
  > `m:gen_server`'s time-out works like Erlang's
  > `receive ... after 0 ... end`, in contrast to `GenStatem`.
  """
  @type transition_option() ::
          postpone()
          | hibernate()
          | event_timeout()
          | generic_timeout()
          | state_timeout()

  @typedoc """
  Postpone an event to handle it later.

  If `true`, postpones the current event.
  After a _state change_ (`next_state !== state`), it is retried.
  """
  ## If 'true' postpone the current event
  ## and retry it when the state changes (!==)
  @type postpone() :: boolean()

  @typedoc """
  Hibernate the server process.

  If `true`, hibernates the `GenStatem` by calling `proc_lib:hibernate/3`
  before going into `receive` to wait for a new external event.

  There is also a server start option
  [`{hibernate_after, Timeout}`](`t:enter_loop_opt/0`)
  for automatic hibernation.

  > #### Note {: .info }
  >
  > If there are enqueued events to process when hibernation is requested,
  > this is optimized by not hibernating but instead calling
  > [`:erlang.garbage_collect/0`](`:erlang.garbage_collect/0`) to simulate,
  > in a more efficient way, that the `GenStatem` entered hibernation
  > and immediately got awakened by an enqueued event.
  """
  ## If 'true' hibernate the server instead of going into receive
  @type hibernate() :: boolean()

  @typedoc """
  How long to wait for an event.

  Starts a timer set by `t:timeout_action/0`
  `Time`, or `{timeout, Time, EventContent [, Options]}`.

  When the timer expires an event of `t:event_type/0` `timeout`
  will be generated. See `:erlang.start_timer/4` for how `Time`
  and [`Options`](`t:timeout_option/0`) are interpreted.  Future
  `:erlang.start_timer/4` `Options` will not necessarily be supported.

  Any event that arrives cancels this time-out. Note that a retried
  or inserted event counts as arrived. So does a state time-out zero event,
  if it was generated before this time-out is requested.

  If `Time` is `infinity`, no timer is started,
  as it never would expire anyway.

  If `Time` is relative and `0` no timer is actually started,
  instead the the time-out event is enqueued to ensure
  that it gets processed before any not yet received external event,
  but after already queued events.

  Note that it is not possible nor needed to cancel this time-out,
  as it is cancelled automatically by any other event, meaning that
  whenever a callback is invoked that may want to cancel this time-out,
  the timer is already cancelled or expired.

  The timer `EventContent` can be updated with the
  [`{timeout, update, NewEventContent}`](`t:timeout_update_action/0`)
  action without affecting the time of expiry.
  """
  ## Generate a ('timeout', EventContent, ...) event
  ## unless some other event is delivered
  @type event_timeout() ::
          time :: timeout() | integer()

  @typedoc """
  How long to wait for a named time-out event.

  Starts a timer set by `t:timeout_action/0`
  `{{timeout, Name}, Time, EventContent [, Options]}`.

  When the timer expires an event of `t:event_type/0` `{timeout, Name}`
  will be generated. See `:erlang.start_timer/4` for how `Time`
  and [`Options`](`t:timeout_option/0`) are interpreted. Future
  `:erlang.start_timer/4` `Options` will not necessarily be supported.

  If `Time` is `infinity`, no timer is started,
  as it never would expire anyway.

  If `Time` is relative and `0` no timer is actually started,
  instead the time-out event is enqueued to ensure
  that it gets processed before any not yet received external event.

  Setting a timer with the same `Name` while it is running
  will restart it with the new time-out value.  Therefore it is possible
  to cancel a specific time-out by setting it to `infinity`.
  It can also be cancelled more explicitly with the
  [`{{timeout, Name}, cancel}`](`t:timeout_cancel_action/0`) action.

  The timer `EventContent` can be updated with the
  [`{{timeout, Name}, update, NewEventContent}`](`t:timeout_update_action/0`)
  action without affecting the time of expiry.
  """
  ## Generate a ({'timeout',Name}, EventContent, ...) event
  @type generic_timeout() ::
          time :: timeout() | integer()

  @typedoc """
  How long to wait in the current state.

  Starts a timer set by `t:timeout_action/0`, or
  `{:state_timeout, time, event_content [, options]}`.

  When the timer expires an event of `t:event_type/0` `:state_timeout`
  will be generated. See `:erlang.start_timer/4` for how `time`
  and [`options`](`t:timeout_option/0`) are interpreted. Future
  `:erlang.start_timer/4` `options` will not necessarily be supported.

  A _state change_ cancels this timer, if it is running. That is, if the
  `t:timeout_action/0` that starts this timer is part of a list of
  `t:action/0`s for a _state change_, `next_state !== current_state`,
  the timer runs in the **`next_state`**.

  If the state machine stays in that new state, now the current state,
  the timer will run until it expires, which creates the time-out event.
  If the state machine changes states from the now current state,
  the timer is cancelled.  During the _state change_ from
  the now current state, a new _state time-out_ may be started
  for the next **`next_state`**.

  If the `t:timeout_action/0` that starts this timer
  is part of a list of `t:action/0`s for a _state transition_
  that is not a _state change_, the timer runs in the current state.

  If `Time` is `infinity`, no timer is started,
  as it never would expire anyway.

  If `Time` is relative and `0` no timer is actually started,
  instead the the time-out event is enqueued to ensure
  that it gets processed before any not yet received external event.

  Setting this timer while it is running will restart it
  with the new time-out value.  Therefore it is possible
  to cancel this time-out by setting it to `infinity`.
  It can also be cancelled more explicitly with
  [`{state_timeout, cancel}`](`t:timeout_cancel_action/0`).

  The timer `EventContent` can be updated with the
  [`{state_timeout, update, NewEventContent}`](`t:timeout_update_action/0`)
  action without affecting the time of expiry.
  """
  ## Generate a ('state_timeout', EventContent, ...) event
  ## unless the state is changed
  @type state_timeout() ::
          time :: timeout() | integer()

  @typedoc """
  Time-out timer start option, to select absolute time of expiry.

  If `Abs` is `true` an absolute timer is started,
  and if it is `false` a relative, which is the default.
  See [`:erlang.start_timer/4`](`:erlang.start_timer/4`) for details.
  """
  @type timeout_option() :: {:abs, abs :: boolean()}

  @typedoc """
  Actions for a _state transition_, or when starting the server.

  These _transition actions_ can be invoked by returning them from the
  [_state callback_](#state-callback) when it is called
  with an [event](`t:event_type/0`), from [`Module.init/1`](`c:init/1`).
  They are **not allowed** from _state enter calls_.

  Actions are executed in the containing list order.

  Actions that set [transition options](`t:transition_option/0`)
  override any previous of the same type, so the last
  in the containing list wins.  For example, the last `t:postpone/0`
  overrides any previous `t:postpone/0` in the list.

  - **`{:postpone, value}`** - Sets the
    [`transition_option()` ](`t:transition_option/0`)`t:postpone/0`
    for this _state transition_.  This action is ignored when returned from
    [`Module.init/1`](`c:init/1`), as there is no event to postpone
    in those cases.

    `:postpone` is equivalent to `{:postpone, true}`.

  - **`{:next_event, event_type, event_content}`** - This action
    does not set any [`transition_option()`](`t:transition_option/0`)
    but instead stores the specified `event_type` and `event_content`
    for insertion after all actions have been executed.

    The stored events are inserted in the queue as the next to process
    before any already queued events. The order of these stored events
    is preserved, so the first `:next_event` in the containing list
    becomes the first to process.

    An event of type [`internal`](`t:event_type/0`) should be used
    when you want to reliably distinguish an event inserted this way
    from any external event.

  - **`{:change_callback_module, new_module}`** {: #change_callback_module } -
    Changes the callback module to `new_module` which will be used
    when calling all subsequent [state callbacks](#state-callback).\

    The `GenStatem` engine will find out the
    [_callback mode_](`t:callback_mode/0`) of `new_module` by calling
    [`new_module.callback_mode/0`](`c:callback_mode/0`) before the next
    [state callback](#state-callback).

    Changing the callback module does not affect the _state transition_
    in any way, it only changes which module that handles the events.
    Be aware that all relevant callback functions in `new_module` such as
    the [state callback](#state-callback),
    [`new_module.code_change/4`](`c:code_change/4`),
    [`new_module.format_status/1`](`c:format_status/1`) and
    [`new_module.terminate/3`](`c:terminate/3`) must be able to handle
    the state and data from the old module.

  - **`{:push_callback_module, new_module}`** {: #push_callback_module } -
     Pushes the current callback module to the top of an internal stack
     of callback modules, and changes the callback module to `new_module`.
     Otherwise like `{:change_callback_module, new_module}` above.

  - **`pop_callback_module`** {: #pop_callback_module } -
    Pops the top module from the internal stack of callback modules
    and changes the callback module to be the popped module.
    If the stack is empty the server fails.
    Otherwise like `{:change_callback_module, new_module}` above.
  """
  ## During a state change:
  ## * next_state and new_data are set.
  ## * All action()s are executed in order of apperance.
  ## * Postponing the current event is performed
  ##   if 'postpone' is 'true'.
  ## * A state time-out is started if 'timeout' is set.
  ## * Pending events are handled or if there are
  ##   no pending events the server goes into receive
  ##   or hibernate (if 'hibernate' is 'true')
  ##
  ## These action()s are executed in order of appearence
  ## in the containing list. The ones that set options
  ## will override any previous so the last of each kind wins.
  ##
  # Set the postpone option
  @type action() ::
          :postpone
          | {:postpone, postpone :: postpone()}
          ##
          ## All 'next_event' events are kept in a list and then
          ## inserted at state changes so the first in the
          ## action() list is the first to be delivered.
          # Insert event as the next to handle
          | {:next_event, event_type :: event_type(), event_content :: event_content()}
          | {:change_callback_module, new_module :: module()}
          | {:push_callback_module, new_module :: module()}
          | :pop_callback_module
          | enter_action()

  @typedoc """
  Actions for any callback: hibernate, time-outs or replies.

  These _transition actions_ are allowed when a `t:action/0` is allowed,
  and also from a _state enter call_, and can be invoked
  by returning them from the [_state callback_](#state-callback), from
  [`Module.init/1`](`c:init/1`).

  Actions are executed in the containing list order.

  Actions that set [transition options](`t:transition_option/0`)
  override any previous of the same type, so the last in the containing
  list wins. For example, the last `t:event_timeout/0` overrides any previous
  `t:event_timeout/0` in the list.

  - **`{:hibernate, value}`** - Sets the `t:transition_option/0`
    `t:hibernate/0` for this _state transition_.

    `hibernate` is equivalent to `{:hibernate, true}`.
  """
  # Set the hibernate option
  @type enter_action() ::
          :hibernate
          | {:hibernate, hibernate :: hibernate()}
          | timeout_action()
          | reply_action()

  @typedoc """
  Event time-out, generic time-outs or state time-out.

  These _transition actions_ can be invoked by returning them from the
  [_state callback_](#state-callback), from
  [`Module.init/1`](`c:init/1`).

  These time-out actions sets time-out [transition options](`t:transition_option/0`).

  - **`time`** - Short for `{:timeout, time, time}`, that is, the time-out message
    is the time-out time. This form exists to allow the
    [_state callback_](#state-callback) return value
    `{:next_state, next_state, new_data, time}`

  - **`{:timeout, time, event_content, [options]}`** -
    Sets the `t:transition_option/0` `t:event_timeout/0` to `time`
    with `event_content`, and time-out options
    [`options`](`t:timeout_option/0`).

  - **`{{:timeout, name}, time, event_content, [options]}`** -
    Sets the `t:transition_option/0` `t:generic_timeout/0` to `time`
    for time-out `name` with `event_content`, and time-out options
    [`options`](`t:timeout_option/0`).

  - **`{:state_timeout, time, event_content, [options]}`** -
    Sets the `t:transition_option/0` `t:state_timeout/0` to `time`
    with `event_content`, and time-out options [`Options`](`t:timeout_option/0`).
  """
  # {:timeout, time, time}
  @type timeout_action() ::
          (time :: event_timeout())
          # Set the event_timeout option
          | {:timeout, time :: event_timeout(), event_content :: event_content()}
          # Set the event_timeout option
          | {:timeout, time :: event_timeout(), event_content :: event_content(),
             options :: timeout_option() | [timeout_option()]}
          ##
          # Set the generic_timeout option
          | {{:timeout, name :: term()}, time :: generic_timeout(),
             event_content :: event_content()}
          # Set the generic_timeout option
          | {{:timeout, name :: term()}, time :: generic_timeout(),
             event_content :: event_content(), options :: timeout_option() | [timeout_option()]}
          ##
          # Set the state_timeout option
          | {:state_timeout, time :: state_timeout(), event_content :: event_content()}
          # Set the state_timeout option
          | {:state_timeout, time :: state_timeout(), event_content :: event_content(),
             options :: timeout_option() | [timeout_option()]}
          | timeout_cancel_action()
          | timeout_update_action()

  @typedoc """
  Clearer way to cancel a time-out than the original
  setting it to ':infinity'.

  It has always been possible to cancel a time-out using
  `t:timeout_action/0` with `time = :infinity`, since setting a new
  time-out time overrides a running timer, and since setting the time
  to `:infinity` is optimized to not setting a timer (that never
  will expire).  Using this action shows the intention more clearly.
  """
  @type timeout_cancel_action() ::
          {:timeout, :cancel}
          | {{:timeout, name :: term()}, :cancel}
          | {:state_timeout, :cancel}

  @typedoc """
  Update the `event_content` without affecting the time of expiry.

  Sets a new `event_content` for a running time-out timer. See
  [timeout_action()](`t:timeout_action/0`) for how to start a time-out.

  If no time-out of this type is active, instead inserts the time-out event
  just like when starting a time-out with relative `time = 0`.  This is a
  time-out autostart with immediate expiry, so there will be noise for example
  if a generic time-out name was misspelled.
  """
  @type timeout_update_action() ::
          {:timeout, :update, event_content :: event_content()}
          | {{:timeout, name :: term()}, :update, event_content :: event_content()}
          | {:state_timeout, :update, event_content :: event_content()}

  @typedoc """
  Reply to a [`call/2,3`](`call/3`).

  This _transition action_ can be invoked by returning it from the
  [_state callback_](#state-callback), from [`Module.init/1`](`c:init/1`).

  It does not set any [`transition_option()`](`t:transition_option/0`)
  but instead replies to a caller waiting for a reply in `call/3`.
  `from` must be the term from argument [`{:call, from}`](`t:event_type/0`)
  in a call to a [_state callback_](#state-callback).

  Note that using this action from [`Module.init/1`](`c:init/1`) would be weird
  on the border of witchcraft since there has been no earlier call to a
  [_state callback_](#state-callback) in this server.
  """
  # Reply to a caller
  @type reply_action() :: {:reply, from :: from(), reply :: term()}

  @type init_result(state_type) :: init_result(state_type, term())

  @typedoc """
  The return value from [`Module.init/1`](`c:init/1`).

  For a successful initialization, `state` is the initial `t:state/0`,
  and `data` the initial server `t:data/0` of the `GenStatem`.

  The [`actions`](`t:action/0`) are executed when entering the first
  [state](`t:state/0`) just as for a [_state callback_](#state-callback),
  except that the action `postpone` is forced to `false` since there is
  no event to postpone.

  For an unsuccessful initialization, `{:stop, reason}`, `{:error, reason}`,
  or `:ignore` should be used; see [`start_link/3,4`](`start_link/3`).
  """
  @type init_result(state_type, data_type) ::
          {:ok, state :: state_type, data :: data_type}
          | {:ok, state :: state_type, data :: data_type, actions :: [action()] | action()}
          | :ignore
          | {:stop, reason :: term()}
          | {:error, reason :: term()}

  @type state_enter_result(state) :: state_enter_result(state, term())

  @typedoc """
  Return value from a [_state callback_](#state-callback)
  after a _state enter call_.

  `state` is the current state and it cannot be changed since the state callback
   was called with a [_state enter call_](`t:state_enter/0`).

  - **`{:next_state, state, new_data [, actions]}`** -
    The `GenStatem` does a state transition to `state`, which has to be
    equal to the current state, sets `new_data`, and executes all `actions`.
  """
  # {:next_state, state, new_data,[]}
  @type state_enter_result(state, data_type) ::
          {:next_state, state, new_data :: data_type}
          # State entry for state State
          | {:next_state, state, new_data :: data_type,
             actions :: [enter_action()] | enter_action()}
          | state_callback_result(enter_action(), data_type)

  @type event_handler_result(state_type) ::
          event_handler_result(state_type, term())

  @typedoc """
  Return value from a [_state callback_](#state-callback)
  after handling an event.

  `state_type` is `t:state_name/0`
  if [_callback mode_](`t:callback_mode/0`) is `:state_functions`,
  or `t:state/0`
  if [_callback mode_](`t:callback_mode/0`) is `:handle_event_function`.

  - **`{:next_state, next_state, new_data [, actions]}`** -
    The `GenStatem` does a _state transition_ to `next_state`
    (which may be the same as the current state), sets `new_data`
    as the current server `t:data/0`, and executes all `actions`.
    If `NextState !== CurrentState` the _state transition_
    is a _state change_.
  """
  # {next_state,NextState,NewData,[]}
  @type event_handler_result(state_type, data_type) ::
          {:next_state, next_state :: state_type, new_data :: data_type}
          # State transition, maybe to the same state
          | {:next_state, next_state :: state_type, new_data :: data_type,
             actions :: [action()] | action()}
          | state_callback_result(action(), data_type)

  @typedoc """
  Return value from any [_state callback_](#state-callback).

  `ActionType` is `t:enter_action/0` if the state callback
  was called with a [_state enter call_](`t:state_enter/0`),
  and `t:action/0` if the state callback was called with an event.

  - **`{keep_state, NewData [, Actions]}`** - The same as
    `{next_state, CurrentState, NewData [, Actions]}`.

  - **`keep_state_and_data | {keep_state_and_data, Actions}`** -
    The same as `{keep_state, CurrentData [, Actions]}`.

  - **`{repeat_state, NewData [, Actions]}`** - If the `GenStatem`
    runs with [_state enter calls_](`t:state_enter/0`),
    the _state enter call_ is repeated, see type `t:transition_option/0`.
    Other than that `{repeat_state, NewData [, Actions]}` is the same as
    `{keep_state, NewData [, Actions]}`.

  - **`repeat_state_and_data | {repeat_state_and_data, Actions}`** -
    The same as `{repeat_state, CurrentData [, Actions]}`.

  - **`{stop, Reason [, NewData]}`** - Terminates the `GenStatem`
    by calling [`Module.terminate/3`](`c:terminate/3`)
    with `Reason` and `NewData`, if specified. An exit signal
    with this reason is sent to linked processes and ports.

  - **`stop`** - The same as `{stop, normal}`.

  - **`{stop_and_reply, Reason, Replies [, NewData]}`** -
    Sends all `Replies`, then terminates the `GenStatem`
    like with `{stop, Reason [, NewData]}`.

  All these terms are tuples or atoms and will be so
  in all future versions of `GenStatem`.
  """
  # {keep_state,new_data,[]}
  @type state_callback_result(action_type, data_type) ::
          {:keep_state, new_data :: data_type}
          # Keep state, change data
          | {:keep_state, new_data :: data_type, actions :: [action_type] | action_type}
          # {keep_state_and_data,[]}
          | :keep_state_and_data
          # Keep state and data -> only actions
          | {:keep_state_and_data, actions :: [action_type] | action_type}
          ##
          # {repeat_state,new_data,[]}
          | {:repeat_state, new_data :: data_type}
          # Repeat state, change data
          | {:repeat_state, new_data :: data_type, actions :: [action_type] | action_type}
          # {repeat_state_and_data,[]}
          | :repeat_state_and_data
          # Repeat state and data -> only actions
          | {:repeat_state_and_data, actions :: [action_type] | action_type}
          ##
          # {stop,normal}
          | :stop
          # Stop the server
          | {:stop, reason :: term()}
          # Stop the server
          | {:stop, reason :: term(), new_data :: data_type}
          ##
          # Reply then stop the server
          | {:stop_and_reply, reason :: term(), replies :: [reply_action()] | reply_action()}
          # Reply then stop the server
          | {:stop_and_reply, reason :: term(), replies :: [reply_action()] | reply_action(),
             new_data :: data_type}

  @typedoc "An opaque request identifier. See `send_request/2` for details."
  @opaque request_id() :: :gen.request_id()

  @typedoc """
  An opaque collection of request identifiers (`t:request_id/0`).

  Each request identifier can be associated with
  a label chosen by the user.  For more information see `reqids_new/0`.
  """
  @opaque request_id_collection() :: :gen.request_id_collection()

  @typedoc """
  Response time-out for an asynchronous call.

  Used to set a time limit on how long to wait for a response using either
  `receive_response/2`, `receive_response/3`, `wait_response/2`, or
  `wait_response/3`.  The time unit used is `millisecond`.

   Currently valid values:

  - **`0..4294967295`** - Time-out relative to current time in milliseconds.

  - **`infinity`** - Infinite time-out. That is,
    the operation will never time out.

  - **`{abs, Timeout}`** - An absolute
    [Erlang monotonic time](`:erlang.monotonic_time/1`)
    time-out in milliseconds.  That is, the operation will time out when
    [`:erlang.monotonic_time(millisecond)`](`:erlang.monotonic_time/1`)
    returns a value larger than or equal to `Timeout`.
    `Timeout` is not allowed to identify a time further into the future
    than `4294967295` milliseconds.  Specifying the time-out
    using an absolute value is especially handy when you have
    a deadline for responses corresponding to a complete collection
    of requests (`t:request_id_collection/0`), since you do not have to
    recalculate the relative time until the deadline over and over again.
  """
  @type response_timeout() ::
          timeout() | {:abs, integer()}

  ## The state machine init function.  It is called only once and
  ## the server is not running until this function has returned
  ## an {:ok, ...} tuple.  Thereafter the state callbacks are called
  ## for all events to this server.
  ## ----------------------
  @doc """
  Initialize the state machine.

  Whenever a `GenStatem` is started using
  [`start_link/3,4`](`start_link/3`),
  [`start_monitor/3,4`](`start_monitor/3`), or
  [`start/3,4`](`start/3`), this function is called by the new process
  to initialize the implementation state and server data.

  `Args` is the `Args` argument provided to that start function.

  > #### Note {: .info }
  >
  > Note that if the `GenStatem` is started through `m:proc_lib`, this
  > callback will never be called.  Since this callback is not optional
  > it can in that case be implemented as:
  >
  > ```erlang
  > -spec init(_) -> no_return().
  > init(Args) -> :erlang.error(not_implemented, [Args]).
  > ```
  """
  @callback init(args :: term()) :: init_result(state())

  ## This callback shall return the callback mode of the callback module.
  ##
  ## It is called once after init/0 and code_change/4 but before
  ## the first state callback state_name/3 or handle_event/4.
  ## ----------------------
  @doc """
  Select the _callback mode_ and possibly
  [_state enter calls_](`t:state_enter/0`).

  This function is called by a `GenStatem` when it needs to find out the
  [_callback mode_](`t:callback_mode/0`) of the callback module.

  The value is cached by `GenStatem` for efficiency reasons,
  so this function is only called once after server start,
  after code change, and after changing the callback module,
  but before the first [_state callback_](#state-callback)
  in the current callback module's code is called.  More occasions may be
  added in future versions of `GenStatem`.

  Server start happens either when [`Module.init/1`](`c:init/1`) returns.
  Code change happens when [`Module.code_change/4`](`c:code_change/4`)
  returns.  A change of the callback module happens when
  a [_state callback_](#state-callback) returns
  any of the actions [`change_callback_module`](#push_callback_module),
  [`push_callback_module`](#push_callback_module) or
  [`pop_callback_module`](#pop_callback_module).

  The `callback_mode` is either just `t:callback_mode/0`
  or a list containing `t:callback_mode/0` and possibly
  the atom [`:state_enter`](`t:state_enter/0`).

  > #### Note {: .info }
  >
  > If this function's body does not return an inline constant value
  > the callback module is doing something strange.
  """
  @callback callback_mode() :: callback_mode_result()

  ## Example state callback for state name state_name
  ## when callback_mode() === :state_functions
  ##
  ## In this mode all states has to be of type state_name() i.e atom().
  ##
  ## Note that the only callbacks that have arity 3 are these
  ## state_name/3 callbacks and terminate/3, so the state name
  ## 'terminate' is unusable in this mode.
  ## ----------------------
  @doc """
  [_State callback_](#state-callback) in
  [_callback mode_](`t:callback_mode/0`) `:state_functions`.

  State callback that handles all events in state `state_name`, where
  [`state_name :: state_name()`](`t:state_name/0`)
  has to be an `t:atom/0`.

  `state_name` cannot be `terminate` since that would collide
  with the callback function [`Module.terminate/3`](`c:terminate/3`).

  Besides that when doing a [_state change_](#state-callback)
  the next state always has to be an `t:atom/0`,
  this function is equivalent to
  [`Module.handle_event(event_type, event_content,
  __ENV__.function, data)`](`c:handle_event/4`),
  which is the [_state callback_](#state-callback) in
  [_callback mode_](`t:callback_mode/0`) `handle_event_function`.
  """
  @callback state_name(:enter, old_state_name :: state_name(), data()) ::
              state_enter_result(:state_name)

  @callback state_name(
              event_type :: event_type(),
              event_content :: event_content(),
              data :: data()
            ) :: event_handler_result(state_name())

  ## State callback for all states
  ## when callback_mode() =:= handle_event_function.
  ## ----------------------

  @doc """
  [_state callback_](#state-callback) in
  [_callback mode_](`t:callback_mode/0`) `handle_event_function`.

  Whenever a `GenStatem` receives an event from [`call/2,3`](`call/3`),
  `cast/2`, or as a normal process message, this function is called.

  If `event_type` is [`{:call, from}`](`t:event_type/0`),
  the caller waits for a reply.  The reply can be sent from this
  or from any other [_state callback_](#state-callback)
  by returning with `{:reply, from, reply}` in [`actions`](`t:action/0`),
  in [`replies`](`t:reply_action/0`), or by calling
  [`reply(from, reply)`](`reply/2`).

  If this function returns with a next state
  that does not match equal (`!==`) to the current state,
  all postponed events are retried in the next state.

  For options that can be set and actions that can be done
  by `GenStatem` after returning from this function, see `t:action/0`.

  When the `GenStatem` runs with [_state enter calls_](`t:state_enter/0`),
  this function is also called with arguments `(:enter, old_state, ...)`
  during every _state change_.  In this case there are some restrictions
  on the [actions](`t:action/0`) that may be returned:

  - `t:postpone/0` is not allowed since a _state enter call_
  is not an event so there is no event to postpone.
  - [`{:next_event, _, _}`](`t:action/0`) is not allowed since
  using _state enter calls_ should not affect how events
  are consumed and produced.
  - It is not allowed to change states from this call.
  Should you return `{:next_state, next_state, ...}`
  with `next_state !== state` the `GenStatem` crashes.

  Note that it is actually allowed to use `{:repeat_state, new_data, ...}`
  although it makes little sense since you immediately
  will be called again with a new _state enter call_ making this
  just a weird way of looping, and there are better ways to loop.

  If you do not update `new_data` and have some loop termination condition,
  or if you use `{:repeat_state_and_data, _}` or `:repeat_state_and_data`
  you have an infinite loop\!

  You are advised to use `{:keep_state, ...}`, `{:keep_state_and_data, _}`
  or `:keep_state_and_data` since changing states
  from a _state enter call_ is not possible anyway.

  Note the fact that you can use [`throw`](`:erlang.throw/1`)
  to return the result, which can be useful.  For example to bail out with
  [`throw(:keep_state_and_data)`](`throw/1`) from deep within complex code
  that cannot return `{:next_state, state, data}` because `state` or `data`
  is no longer in scope.
  """
  @callback handle_event(
              :enter,
              old_state :: state(),
              current_state :: state(),
              data :: data()
            ) :: state_enter_result(current_state :: state())

  @callback handle_event(
              event_type :: event_type(),
              event_content :: event_content(),
              current_state :: state(),
              data :: data()
            ) :: event_handler_result(state())

  ## Clean up before the server terminates.
  ## ----------------------
  @doc """
  Handle state machine termination.

  This function is called by a `GenStatem` when it is about to terminate.
  It is to be the opposite of [`Module.init/1`](`c:init/1`)
  and do any necessary cleaning up.  When it returns, the `GenStatem`
  terminates with `reason`.  The return value is ignored.

  `reason` is a term denoting the stop reason and [`state`](`t:state/0`)
  is the internal state of the `GenStatem`.

  `reason` depends on why the `GenStatem` is terminating. If it is because
  another callback function has returned, a stop tuple `{:stop, reason}` in
  [`actions`](`t:action/0`), `reason` has the value specified in that tuple.
  If it is because of a failure, `reason` is the error reason.

  If the `GenStatem` is part of a supervision tree and is ordered by its
  supervisor to terminate, this function is called with `reason = :shutdown`
  if both the following conditions apply:

  - The `GenStatem` process has been set to trap exit signals.
  - The shutdown strategy as defined in the supervisor's
    child specification is an integer time-out value, not `:brutal_kill`.

  Even if the `GenStatem` is _not_ part of a supervision tree,
  this function is called if it receives an `:EXIT` message
  from its parent. `reason` is the same as in the `:EXIT` message.

  If the `GenStatem` process is not set up to trap exit signals it is
  immediately terminated, just like any process, and this function is not called.

  Notice that for any other reason than `:normal`, `:shutdown`, or
  `{:shutdown, term}`, the `GenStatem` is assumed to terminate
  because of an error and an error report is issued using `m:logger`.

  When the `GenStatem` process exits, an exit signal
  with the same reason is sent to linked processes and ports,
  just as for any process.
  """
  @callback terminate(
              reason :: :normal | :shutdown | {:shutdown, term()} | term(),
              current_state :: state(),
              data()
            ) :: any()

  @doc """
  Update the [state](`t:state/0`) and [data](`t:data/0`) after code change.

  This function is called by a `GenStatem` when it is to update
  its internal state during a release upgrade/downgrade that is,
  when the instruction `{:update, module, change, ...}`,
  where `change = {:advanced, extra}`, is specified in
  the [`appup`](https://www.erlang.org/doc/apps/sasl/appup.html) file.
  For more information, see
  [Release Handling Instructions in OTP Design Principles
  ](https://www.erlang.org/docs/28/system/release_handling.html#release-handling-principles).

  For an upgrade, `old_vsn` is `vsn`, and for a downgrade, `old_vsn` is
  `{:down, vsn}`. `vsn` is defined by the `vsn` attribute(s)
  of the old version of the callback module `Module`.
  If no such attribute is defined, the version is the checksum
  of the Beam file.

  `old_state` and `old_data` are the internal state of the `GenStatem`.

  `extra` is passed "as is" from the `{:advanced, extra}` part
  of the update instruction.

  If successful, the function must return the updated internal state
  in an `{ok, new_state, new_data}` tuple.

  If the function returns a failure `reason`, the ongoing upgrade fails
  and rolls back to the old release. Note that `reason` cannot be
  an `{:ok, _, _}` tuple since that will be regarded
  as a `{:ok, new_state, new_data}` tuple, and that a tuple matching `{:ok, _}`
  is an also invalid failure `reason`. It is recommended to use
  an atom as `reason` since it will be wrapped in an `{:error, reason}` tuple.

  Also note when upgrading a `GenStatem`, this function and hence the
  `change = {:advanced, extra}` parameter
  in the [`appup`](https://www.erlang.org/doc/apps/sasl/appup.html) file is not only
  needed to update the internal state or to act on the `extra`
  argument. It is also needed if an upgrade or downgrade should change
  [_callback mode_](`t:callback_mode/0`), or else the _callback mode_
  after the code change will not be honoured, most probably causing
  a server crash.

  If the server changes callback module using any of the actions
  [`change_callback_module`](#change_callback_module),
  [`push_callback_module`](#push_callback_module), or
  [`pop_callback_module`](#pop_callback_module), be aware that it is always
  the current callback module that will get this callback call.
  That the current callback module handles the current
  state and data update should be no surprise, but it
  must be able to handle even parts of the state and data
  that it is not familiar with, somehow.

  In the supervisor [child specification
  ](https://www.erlang.org/docs/28/system/sup_princ.html#child-specification)
  there is a list of modules which is recommended to contain
  only the callback module. For a `GenStatem` with multiple callback modules
  there is no real need to list all of them, it may not even be possible since
  the list could change after code upgrade.  If this list would contain only
  the start callback module, as recommended, what is important
  is to upgrade _that_ module whenever a _synchronized code replacement_ is done.
  Then the release handler concludes that an upgrade that upgrades _that_ module
  needs to suspend, code change, and resume any server whose child specification
  declares that it is using _that_ module.
  And again; the _current_ callback module will get the
  [`Module.code_change/4`](`c:code_change/4`) call.

  > #### Note {: .info }
  >
  > If a release upgrade/downgrade with `change = {:advanced, extra}`
  > specified in the `.appup` file is made
  > when [`Module.code_change/4`](`c:code_change/4`) is not implemented
  > the process will crash with exit reason `undef`.
  """
  @callback code_change(
              old_vsn :: term() | {:down, term()},
              old_state :: state(),
              old_data :: data(),
              extra :: term()
            ) :: {:ok, new_state :: state(), new_data :: data()} | (reason :: term())

  @typedoc """
  A map that describes the server's status.

  The keys are:
  - **`state`** - The current state.
  - **`data`** - The state data.
  - **`reason`** - The reason that caused the process to terminate.
  - **`queue`** - The event queue.
  - **`postponed`** - The queue of [postponed](`t:postpone/0`) events.
  - **`timeouts`** - The active [time-outs](`t:timeout_action/0`).
  - **`log`** - The [sys log](`:sys.log/2`) of the server.

  New associations may be added to the status map without prior notice.
  """
  @type format_status() :: %{
          :state => state(),
          :data => data(),
          :reason => term(),
          :queue => [{event_type(), event_content()}],
          :postponed => [{event_type(), event_content()}],
          :timeouts => [{timeout_event_type(), event_content()}],
          :log => [:sys.system_event()]
        }

  ## Format the callback module status in some sensible that is
  ## often condensed way.
  ## ----------------------
  @doc """
  Format/limit the status value.

  This function is called by a `GenStatem` process
  in order to format/limit the server status
  for debugging and logging purposes.

  It is called in the following situations:

  - [`:sys.get_status/1,2`](`:sys.get_status/1`) is invoked
  to get the `GenStatem` status.
  - The `GenStatem` process terminates abnormally and logs an error.

  This function is useful for changing the form and appearance
  of the `GenStatem` status for these cases.  A callback module
  wishing to change the [`:sys.get_status/1,2`](`:sys.get_status/1`)
  return value and how its status appears in termination error logs,
  exports an instance of [`Module.format_status/1`](`c:format_status/1`),
  which will get a map `Status` that describes the current state
  of the `GenStatem`, and shall return a map `NewStatus`
  containing the same keys as the input map,
  but it may transform some values.

  One use case for this function is to return compact alternative state
  representations to avoid having large state terms printed in log files.
  Another is to hide sensitive data from being written to the error log.

  Example:

  ```elixir
  def format_status(status) when is_map(status) do
    status
    |> Enum.map(fn
      {:state, state} when is_map(state) ->
        {:state, Map.delete(state, :private_key)}
      {:message, {:password, _pass}} ->
        {:message, {:password, :removed}}
      pair ->
        pair
    end)
    |> Map.new()
  end
  ```

  > #### Note {: .info }
  >
  > This callback is optional, so a callback module does not need
  > to export it.  The `GenStatem` module provides
  > a default implementation of this function that returns `{state, data}`.
  """
  @callback format_status(status :: format_status()) :: new_status :: format_status()

  # Has got a default implementation
  @optional_callbacks format_status: 1,
                      # Has got a default implementation
                      terminate: 3,
                      # Only needed by advanced soft upgrade
                      code_change: 4,
                      ## Just an example callback;
                      state_name: 3,
                      ## for callback_mode() === :state_functions
                      ## there has to be a state_name/3 callback function
                      ## for every StateName in your state machine,
                      ## but not one has to be named 'state_name'
                      # Only for callback_mode() === :handle_event_function
                      handle_event: 4

  ### ==========================================================================
  ### API
  @typedoc """
  Server name specification: `:local`, `:global`, or `:via` registered.

  Name specification to use when starting a `GenStatem` server.
  See `start_link/3` and `t:server_ref/0` below.
  """
  # Duplicate of :gen.emgr_name()
  @type server_name() ::
          {:local, atom()}
          | {:global, global_name :: term()}
          | {:via, reg_mod :: module(), name :: term()}

  @typedoc """
  Server specification: `t:pid/0` or registered `t:server_name/0`.

  To be used in [`call/2,3`](`call/3`) to specify the server.

  It can be:

  - **`pid() | local_name`** - The `GenStatem` is locally registered.

  - **`{name, node}`** - The `GenStatem` is locally registered
  on another node.

  - **`{:global, global_name}`** - The `GenStatem` is globally registered
  in `m:global`.

  - **`{:via, reg_mod, via_name}`** - The `GenStatem` is registered
  in an alternative process registry.  The registry callback module
  `reg_mod` is to export functions `register_name/2`, `unregister_name/1`,
  `whereis_name/1`, and `send/2`, which are to behave like
  the corresponding functions in `m:global`.
  Thus, `{:via, :global, global_name}` is the same as `{:global, global_name}`.
  """
  # What gen:call/3,4 and gen:stop/1,3 accepts
  @type server_ref() ::
          pid()
          | (local_name :: atom())
          | {name :: atom(), node :: atom()}
          | {:global, global_name :: term()}
          | {:via, reg_mod :: module(), via_name :: term()}

  @typedoc """
  Server [start options](#start-options) for the
  [`start/3,4`](`start/3`), [`start_link/3,4`](`start_link/3`),
  and [`start_monitor/3,4`](`start_monitor/3`) functions.

  See [`start_link/4`](#start-options).
  """
  # Duplicate of :gen.option()
  @type start_opt() ::
          {:name, server_name() | atom()}
          | {:timeout, time :: timeout()}
          | {:spawn_opt, [:proc_lib.start_spawn_option()]}
          | enter_loop_opt()

  @typedoc """
  Server [start options](#start-options) for the
  [`start/3,4`](`start/3`), [`start_link/3,4`](`start_link/3`),
  and [`start_monitor/3,4`](`start_monitor/3`), functions.

  See [`start_link/4`](#start-options).
  """
  # Some :gen.option()s works for enter_loop/*
  @type enter_loop_opt() ::
          {:hibernate_after, hibernate_after_timeout :: timeout()}
          | {:debug, dbgs :: [:sys.debug_option()]}

  @typedoc """
  [Return value](#start-return-values) from the [`start/3,4`](`start/3`)
  and [`start_link/3,4`](`start_link/3`) functions.

  See [`start_link/4`](#start-return-values).
  """
  # :gen.start_ret() without monitor return
  @type start_ret() ::
          {:ok, pid()}
          | :ignore
          | {:error, term()}

  @typedoc """
  Return value from the [`start_monitor/3,4`](`start_monitor/3`) functions.

  As for [`start_link/4`](#start-return-values) but a successful return
  wraps the process ID and the [monitor reference](`:erlang.monitor/2`) in a
  `{:ok, {`[`pid()`](`t:pid/0`)`, `[`reference()`](`t:reference/0`)`}}`
  tuple.
  """
  # :gen.start_ret() with only monitor return
  @type start_mon_ret() ::
          {:ok, {pid(), reference()}}
          | :ignore
          | {:error, term()}

  @doc false
  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      @behaviour GenStatem

      if not Module.has_attribute?(__MODULE__, :doc) do
        @doc """
        Returns a specification to start this module under a supervisor.

        See `Supervisor`.
        """
      end

      def child_spec(init_args) do
        default = %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [init_args]}
        }

        Supervisor.child_spec(default, unquote(Macro.escape(opts)))
      end

      @doc false
      def get_proc do
        case Process.info(self(), :registered_name) do
          {_, []} -> self()
          {_, name} -> name
        end
      end

      @doc false
      def init(args) do
        proc = get_proc()

        raise "attempted to call GenStatem #{inspect(proc)} but no init/1 clause was provided"
      end

      def callback_mode do
        proc = get_proc()

        raise "attempted to call GenStatem #{inspect(proc)} but no callback_mode/1 clause was provided"
      end

      @doc false
      def terminate(_reason, _state, _data), do: :ok

      @doc false
      def code_change(_oldvsn, oldstate, olddata, _extra) do
        {:ok, oldstate, olddata}
      end

      defoverridable init: 1,
                     code_change: 4,
                     terminate: 3,
                     callback_mode: 0
    end
  end

  ## Start a state machine
  ## ----------------------
  @doc """
  Start a `GenStatem` process linked to the current process.

  This is often used to start the `GenStatem` as part of a supervision tree.

  Once the server is started, the `c:init/1` function of the given `module` is
  called with `init_arg` as its argument to initialize the server. To ensure a
  synchronized start-up procedure, this function does not return until `c:init/1`
  has returned.

  Note that a `GenStatem` started with `start_link/3` is linked to the
  parent process and will exit in case of crashes from the parent. The GenStatem
  will also exit due to the `:normal` reasons in case it is configured to trap
  exits in the `c:init/1` callback.

  ## Options

    * `:name` - used for name registration as described in the "Name
      registration" section in the documentation for `GenStatem`

    * `:timeout` - if present, the server is allowed to spend the given number of
      milliseconds initializing or it will be terminated and the start function
      will return `{:error, :timeout}`

    * `:debug` - if present, the corresponding function in the [`:sys` module](`:sys`) is invoked

    * `:spawn_opt` - if present, its value is passed as options to the
      underlying process as in `Process.spawn/4`

    * `:hibernate_after` - if present, the GenStatem process awaits any message for
      the given number of milliseconds and if no message is received, the process goes
      into hibernation automatically (by calling `:proc_lib.hibernate/3`).

  ## Return values

  If the server is successfully created and initialized, this function returns
  `{:ok, pid}`, where `pid` is the PID of the server. If a process with the
  specified server name already exists, this function returns
  `{:error, {:already_started, pid}}` with the PID of that process.

  If the `c:init/1` callback fails with `reason`, this function returns
  `{:error, reason}`. Otherwise, if it returns `{:stop, reason}`
  or `:ignore`, the process is terminated and this function returns
  `{:error, reason}` or `:ignore`, respectively.
  """
  @spec start_link(module(), term(), [start_opt()]) :: start_ret()
  def start_link(module, init_arg, start_opt \\ []) when is_atom(module) and is_list(start_opt) do
    do_start(:link, module, init_arg, start_opt)
  end

  @doc """
  Starts a `GenStatem` process without links (outside of a supervision tree).

  See `start_link/3` for more information.
  """
  @spec start(module(), term(), [start_opt()]) :: start_ret()
  def start(module, init_arg, options \\ []) when is_atom(module) and is_list(options) do
    do_start(:nolink, module, init_arg, options)
  end

  @doc """
  Starts a `GenStatem` process monitored and registered, but not linked.

  See `start_link/3` for more information.
  """
  @spec start_monitor(module(), term(), [start_opt()]) :: start_mon_ret()
  def start_monitor(module, init_arg, options \\ []) when is_atom(module) and is_list(options) do
    do_start(:monitor, module, init_arg, options)
  end

  @spec do_start(link :: :monitor | :link | :nolink, module(), term(), [start_opt()]) ::
          start_ret :: {:ok, pid()} | {:ok, {pid(), reference()}} | :ignore | {:error, term()}
  defp do_start(link, module, init_arg, start_opt) do
    case Keyword.pop(start_opt, :name) do
      {nil, opts} ->
        :gen.start(:gen_statem, link, module, init_arg, opts)

      {atom, opts} when is_atom(atom) ->
        :gen.start(:gen_statem, link, {:local, atom}, module, init_arg, opts)

      {{:global, _term} = tuple, opts} ->
        :gen.start(:gen_statem, link, tuple, module, init_arg, opts)

      {{:via, via_module, _term} = tuple, opts} when is_atom(via_module) ->
        :gen.start(:gen_statem, link, tuple, module, init_arg, opts)

      {other, _} ->
        raise ArgumentError, """
        expected :name option to be one of the following:

          * nil
          * atom
          * {:global, term}
          * {:via, module, term}

        Got: #{inspect(other)}
        """
    end
  end

  @doc """
  Synchronously stops the state machine with the given `reason`.

  The `c:terminate/3` callback of the given `server` will be invoked before
  exiting. This function returns `:ok` if the server terminates with the
  given reason; if it terminates with another reason, the call exits.

  This function keeps OTP semantics regarding error reporting.
  If the reason is any other than `:normal`, `:shutdown` or
  `{:shutdown, _}`, an error report is logged.
  """
  @spec stop(server_ref(), reason :: term(), timeout()) :: :ok
  def stop(server_ref, reason \\ :normal, timeout \\ :infinity) do
    :gen.stop(server_ref, reason, timeout)
  end

  @doc """
  Cast an event to a server.

  Sends an asynchronous `cast` event to the `GenStatem`
  [`ServerRef`](`t:server_ref/0`) and returns `ok` immediately,
  ignoring if the destination node or `GenStatem` does not exist.

  The `GenStatem` calls the
  [_state callback_](#state-callback)
  with `t:event_type/0` `cast` and event content `Msg`.
  """
  @spec cast(server_ref :: server_ref(), msg :: term()) :: :ok
  def cast(server_ref, msg) when is_pid(server_ref) or is_atom(server_ref) do
    send(server_ref, wrap_cast(msg))
    :ok
  catch
    _class, _reason ->
      :ok
  end

  def cast({:global, name}, msg) do
    :global.send(name, wrap_cast(msg))
    :ok
  catch
    _class, _reason -> :ok
  end

  def cast({:via, reg_mod, via_name}, msg) do
    reg_mod.send(via_name, wrap_cast(msg))
    :ok
  catch
    _class, _reason -> :ok
  end

  def cast({name, node} = server_ref, msg) when is_atom(name) and is_atom(node) do
    send(server_ref, wrap_cast(msg))
    :ok
  catch
    _class, _reason -> :ok
  end

  @doc """
  Call a server: send request and wait for response.

  Makes a synchronous call to the `GenStatem`
  [`ServerRef`](`t:server_ref/0`) by sending a request
  and waiting until the response arrives.

  [](){: #call-reply }
  The `GenStatem` calls the
  [_state callback_](#state-callback)
  with `t:event_type/0` `{:call, from}` and event content `request`.

  The server's reply is sent from a [_state callback_](#state-callback),
  by returning a [_transition action_](`t:action/0`) `{:reply, from, reply}`,
  calling [`reply(replies)`](`reply/1`) with such a reply action
  in the `replies` list, or calling [`reply(from, reply)`](`reply/2`).

  `timeout` is an integer > 0, which specifies how many milliseconds
  to wait for a reply, or the atom `:infinity` to wait indefinitely,
  which is the default. If no reply is received within the specified time,
  the function call fails.

  The call can also fail, for example, if the `GenStatem`
  dies before or during this function call.

  When this call fails it raises the [`GenError`](`m:GenStatem.GenError`) Exception and exit
  the calling process.
  """
  @spec call(
          server_ref :: server_ref(),
          request :: term(),
          timeout :: timeout()
        ) :: reply :: term()
  def call(server_ref, request, timeout \\ :infinity) do
    :gen.call(server_ref, :"$gen_call", request, timeout)
  catch
    ## 'gen' raises 'exit' for problems
    :exit, reason ->
      raise GenError,
        class: :exit,
        reason: reason,
        mfargs: {__MODULE__, :call, [server_ref, request, timeout]},
        stacktrace: __STACKTRACE__
  else
    {:ok, reply} -> reply
  end

  @doc """
  Send an asynchronous `call` request.

  Sends `request` to the `GenStatem` process identified by `server_ref`
  and returns a request identifier `req_id`.

  The return value `req_id` shall later be used with `receive_response/2`,
  `wait_response/2`, or `check_response/2` to fetch the actual result
  of the request.  Besides passing the request identifier directly
  to these functions, it can also be stored in a request identifier
  collection using `reqids_add/3`. Such a collection of request identifiers
  can later be used in order to get one response corresponding to a
  request in the collection by passing the collection as argument to
  `receive_response/3`, `wait_response/3`, or `check_response/3`.  If you
  are about to store the request identifier in a collection, you may want
  to consider using `send_request/4` instead.

  The call
  `GenStatem.send_request(server_ref, request) |> GenStatem.wait_response(timeout)`
   can be seen as equivalent to [`GenStatem.call(server, request, timeout)`](`call/3`),
  ignoring the error handling.

  See [`call/3`](#call-reply) about how the request is handled
  and the `reply` is sent by the `GenStatem` server.

  The server's `reply` is returned by one of the [`receive_response/1,2`](`receive_response/2`),
  [`wait_response/1,2`](`wait_response/2`), or `check_response/2` functions.
  """
  @spec send_request(server_ref :: server_ref(), request :: term()) ::
          req_id :: request_id()
  def send_request(server_ref, request) do
    :gen.send_request(server_ref, :"$gen_call", request)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :send_request, [server_ref, request]}
  end

  @doc """
  Send an asynchronous `call` request and add it to a request identifier collection.

  Sends `request` to the `GenStatem` process identified by `server_ref`.
  The `label` will be associated with the request identifier
  of the operation and added to the returned request identifier collection
  `new_req_id_collection`.  The collection can later be used in order to
  get one response corresponding to a request in the collection
  by passing the collection as argument to `receive_response/3`,
  `wait_response/3`, or `check_response/3`.

  The same as calling
  [`reqids_add(​`](`reqids_add/3`)[`send_request(server_ref, request),
  `](`send_request/2`)[`label, req_id_collection)`](`reqids_add/3`),
  but slightly more efficient.
  """
  @spec send_request(
          server_ref :: server_ref(),
          request :: term(),
          label :: term(),
          req_id_col :: request_id_collection()
        ) ::
          new_req_id_collection :: request_id_collection()
  def send_request(server_ref, request, label, req_id_col) do
    :gen.send_request(server_ref, :"$gen_call", request, label, req_id_col)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :send_request, [server_ref, request, label, req_id_col]}
  end

  @doc """
  Wait for a request response.

  Waits for the response to the request identifier `req_id`.  The request
  must have been made by `send_request/2` to the `GenStatem` process.
  This function must be called from the same process from which
  `send_request/2` was called.

  `wait_time` specifies how long to wait for a reply. If no reply is received
  within the specified time, the function returns `timeout` and no cleanup is done,
  Thus the function can be invoked repeatedly until a reply is returned.

  See [`call/3`](#call-reply) about how the request is handled and the `Reply` is
  sent by the `GenStatem` server.

  If the `GenStatem` server process is dead or dies while this function waits for
  the reply, it returns an `error` return with the exit `reason`.

  The difference between `receive_response/2` and `wait_response/2` is that
  `receive_response/2` abandons the request at time-out so that a potential future
  response is ignored, while `wait_response/2` does not.
  """
  @spec wait_response(
          req_id :: request_id(),
          wait_time :: response_timeout()
        ) ::
          result ::
          {:reply, reply :: term()}
          | {:error, {reason :: term(), server_ref()}}
          | timeout()
  def wait_response(req_id, wait_time \\ :infinity) do
    :gen.wait_response(req_id, wait_time)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :wait_response, [req_id, wait_time]}
  end

  @doc """
  Wait for any request response in a collection.

  Waits for a response in `req_id_collection`.  All request identifiers
  of `req_id_collection` must correspond to requests that have been made
  using `send_request/2` or `send_request/4`, and all requests
  must have been made by the process calling this function.

  The `label` in the response is the `label` associated with
  the request identifier that the response corresponds to.
  The `label` of a request identifier is associated
  when [adding the request id](`reqids_add/3`) to a collection,
  or when sending the request using `send_request/4`.

  Compared to `wait_response/2`, the returned result or exception
  associated with a specific request identifier will be wrapped
  in a 3-tuple `{response, label, new_req_id_collection}`.
  `response` is the value that would have been produced
  by `wait_response/2`, `label` is the value associated with
  the specific [request identifier](`t:request_id/0`)
  and `new_req_id_collection` is a possibly modified
  request identifier collection.

  If `req_id_collection` is empty, `no_request` is returned.

  If no response is received before `wait_time` has expired,
  `timeout` is returned.  It is valid to continue waiting
  for a response as many times as needed up until a response
  has been received and completed by `check_response()`,
  `receive_response()`, or `wait_response()`.

  The difference between `receive_response/3` and `wait_response/3`
  is that `receive_response/3` abandons requests at time-out
  so that potential future responses are ignored, while
  `wait_response/3` does not.

  If `delete` is `true`, the association with `label` has been deleted
  from `req_id_collection` in the resulting `new_req_id_collection`. If
  `delete` is `false`, `new_req_id_collection` will equal `req_id_collection`.
  Note that deleting an association is not for free and that a collection
  containing already handled requests can still be used by subsequent calls to
  `wait_response/3`, `check_response/3`, and `receive_response/3`.

  However, without deleting handled associations, the above calls will not be
  able to detect when there are no more outstanding requests to handle, so you
  will have to keep track of this some other way than relying on a `:no_request`
  return. Note that if you pass a collection only containing associations of
  already handled or abandoned requests to this function, it will always block
  until `wait_time` expires and then return `:timeout`.
  """
  @spec wait_response(
          req_id_collection :: request_id_collection(),
          wait_time :: response_timeout(),
          delete :: boolean()
        ) ::
          result ::
          {
            response :: {:reply, reply :: term()} | {:error, {reason :: term(), server_ref()}},
            label :: term(),
            new_req_id_collection :: request_id_collection()
          }
          | :no_request
          | :timeout
  def wait_response(req_id_col, wait_time, delete) do
    :gen.wait_response(req_id_col, wait_time, delete)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :wait_response, [req_id_col, wait_time, delete]}
  end

  @doc """
  Receive a request response.

  Receive a response corresponding to the request identifier `req_id`.
  The request must have been made by `send_request/2`
  to the `gen_statem` process.  This function must be called
  from the same process from which `send_request/2` was made.

  `timeout` specifies how long to wait for a response. If no response is
  received within the specified time, this function returns `timeout`.
  Assuming that the server executes on a node supporting aliases (introduced in OTP 24)
  the request will also be abandoned.  That is, no response will be received
  after a time-out. Otherwise, a stray response might be received at a later time.

  See [`call/3`](#call-reply) about how the request is handled and the `reply` is sent
  by the `gen_statem` server.

  If the `gen_statem` server process is dead or dies while this function waits for the
  reply, it returns an `error` return with the exit `reason`.

  The difference between `wait_response/2` and `receive_response/2` is that
  `receive_response/2` abandons the request at time-out so that a potential future
  response is ignored, while `wait_response/2` does not.
  """
  @spec receive_response(
          req_id :: request_id(),
          response_timeout :: response_timeout()
        ) ::
          {:reply, reply :: term()}
          | {:error, {reason :: term(), server_ref()}}
          | timeout()
  def receive_response(req_id, timeout \\ :infinity) do
    :gen.receive_response(req_id, timeout)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :receive_response, [req_id, timeout]}
  end

  @spec receive_response(
          req_id_col :: request_id_collection(),
          timeout :: response_timeout(),
          delete :: boolean()
        ) ::
          result ::
          {
            response ::
              {:reply, reply :: term()}
              | {:error, {reason :: term(), server_ref()}},
            label :: term(),
            new_req_id_col :: request_id_collection()
          }
          | :no_request
          | :timeout
  def receive_response(req_id_col, timeout, delete) do
    :gen.receive_response(req_id_col, timeout, delete)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :wait_response, [req_id_col, timeout, delete]}
  end

  @doc """
  Check if a received message is a request response.

  Checks if `msg` is a response corresponding to the request identifier `req_id`.
  The request must have been made by `send_request/2` and by the same process
  calling this function.

  If `msg` is a reply to the handle `req_id` the result of the request is returned
  in `reply`.  Otherwise this function returns `:no_reply` and no cleanup is done,
  and thus the function shall be invoked repeatedly until the response is returned.

  See [`call/3`](#call-reply) about how the request is handled and the `reply` is
  sent by the `GenStatem` server.

  If the `GenStatem` server process has died when this function is called, that
  is; `msg` reports the server's death, this function returns an `error` return
  with the exit `reason`.
  """
  @spec check_response(msg :: term(), req_id :: request_id()) ::
          {:reply, reply :: term()}
          | {:error, {reason :: term(), server_ref()}}
          | :no_reply
  def check_response(msg, req_id) do
    :gen.check_response(msg, req_id)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :check_response, [msg, req_id]}
  end

  @spec check_response(
          msg :: term(),
          req_id_collection :: request_id_collection(),
          delete :: boolean()
        ) ::
          {{:reply, reply :: term()} | {:error, {reason :: term(), server_ref()}},
           label :: term(), new_req_id_collection :: request_id_collection()}
          | :no_request
          | :no_reply
  def check_response(msg, req_id_col, delete) do
    :gen.check_response(msg, req_id_col, delete)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :check_response, [msg, req_id_col, delete]}
  end

  @spec reqids_new() :: new_req_id_collection :: request_id_collection()
  def reqids_new do
    :gen.reqids_new()
  end

  @doc "Return the number of request identifiers in `req_id_collection`."
  @spec reqids_size(req_id_collection :: request_id_collection()) ::
          non_neg_integer()
  def reqids_size(req_id_collection) do
    :gen.reqids_size(req_id_collection)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :reqids_size, [req_id_collection]}
  end

  @doc """
  Store a request identifier in a colletion.

  Stores `req_id` and associates a `label` with the request identifier
  by adding this information to `req_id_collection` and returning
  the resulting request identifier collection.
  """
  @spec reqids_add(
          req_id :: request_id(),
          label :: term(),
          req_id_collection :: request_id_collection()
        ) :: new_req_id_collection :: request_id_collection()
  def reqids_add(req_id, label, req_id_collection) do
    :gen.reqids_add(req_id, label, req_id_collection)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :reqids_add, [req_id, label, req_id_collection]}
  end

  @doc """
  Send one or multiple `call` replies.

  This funcion can be used by a `GenStatem` callback to explicitly send
  one or multiple replies to processes waiting for `call` requests' replies,
  when it is impractical or impossible to return `t:reply_action/0`s
  from a [_state callback_](#state-callback).

  > #### Note {: .info }
  >
  > A reply sent with this function is not visible in [`:sys`](`:sys`) debug output.
  """
  def reqids_to_list(req_id_collection) do
    :gen.reqids_to_list(req_id_collection)
  catch
    _class, _reason ->
      raise GenError,
        class: :error,
        reason: :badarg,
        mfargs: {__MODULE__, :reqids_to_list, [req_id_collection]}
  end

  # end

  @doc """
  Send one or multiple `call` replies.

  This funcion can be used by a `GenStatem` callback to explicitly send
  one or multiple replies to processes waiting for `call` requests' replies,
  when it is impractical or impossible to return `t:reply_action/0`s
  from a [_state callback_](#state-callback).

  > #### Note {: .info }
  >
  > A reply sent with this function is not visible in [`:sys`](`:sys`) debug output.
  """
  @spec reply(replies :: [reply_action()] | reply_action()) :: :ok
  def reply({:reply, from, reply}) do
    reply(from, reply)
  end

  def reply(replies) when is_list(replies) do
    replies(replies)
  end

  @doc """
  Send a `call` `reply` to `from`.

  This funcion can be used by a `GenStatem` callback to explicitly send
  a reply to a process waiting for a `call` requests' reply,
  when it is impractical or impossible to return a `t:reply_action/0`
  from a [_state callback_](#state-callback).

  > #### Note {: .info }
  >
  > A reply sent with this function is not visible in [`:sys`](`:sys`) debug output.
  """
  @spec reply(from :: from(), reply :: term()) :: :ok
  def reply(from, reply), do: :gen.reply(from, reply)

  defp replies([{:reply, from, reply} | replies]) do
    reply(from, reply)
    replies(replies)
  end

  defp replies([]), do: :ok

  @compile {:inline, wrap_cast: 1}
  defp wrap_cast(msg) do
    {:"$gen_cast", msg}
  end

  defmodule GenError do
    defexception [:class, :reason, :mfargs, :stacktrace]

    @impl true
    def exception(
          class: class,
          reason: reason,
          mfargs: mfargs
        ) do
      %GenError{
        class: class,
        reason: reason,
        mfargs: mfargs,
        stacktrace: nil
      }
    end

    def exception(
          class: class,
          reason: reason,
          mfargs: mfargs,
          stacktrace: stacktrace
        ) do
      %GenError{
        class: class,
        reason: reason,
        mfargs: mfargs,
        stacktrace: stacktrace
      }
    end

    @impl true
    def message(
          %GenError{
            class: class,
            reason: reason,
            mfargs: {module, function, args}
          } = call_error
        ) do
      mfargs = Exception.format_mfa(module, function, args)
      message = "\nClass: #{class}\nReason: #{reason}\nMFArgs: #{mfargs}"

      case Map.get(call_error, :stacktrace) do
        nil ->
          message

        stack_trace ->
          stacktrace = Exception.format_stacktrace(stack_trace)
          "#{message}\nStacktrace:\n#{stacktrace}"
      end
    end
  end
end
