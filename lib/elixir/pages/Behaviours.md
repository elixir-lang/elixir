# Behaviours

Behaviours in Elixir (and Erlang) are a way to separate and abstract the generic part of a component (which becomes the *behaviour module*) from the specific part (which becomes the *callback module*).

A behaviour module defines a set of functions and macros (referred to as *callbacks*) that callback modules implementing that behaviour must export. This "interface" identifies the specific part of the component. For example, the `GenServer` behaviour and functions abstract away all the message-passing (sending and receiving) and error reporting that a "server" process will likely want to implement from the specific parts such as the actions that this server process has to perform.

If a callback module that implements a given behaviour doesn't export all the functions and macros defined by that behaviour, the user will be notified through warnings during the compilation process (no errors will happen).

Elixir's standard library contains a few frequently used behaviours such as `GenServer`, `Supervisor`, and `Application`.

## Defining a behaviour

A behaviour is always backed by a module (which is how the behaviour will be identified): the module where callbacks are defined. To define a behaviour module, it's enough to define one or more callbacks in that module. To define callbacks, the `@callback` and `@macrocallback` module attributes can be used (for function callbacks and macro callbacks respectively).

    defmodule MyBehaviour do
      @callback my_fun(arg :: any) :: any
      @macrocallback my_macro(arg :: any) :: Macro.t
    end

As seen in the example above, defining a callback is a matter of defining a specification for that callback, made of:

  * the callback name (`my_fun` or `my_macro` in the example)
  * the arguments that the callback must accept (`arg :: any` in the example)
  * the *expected* type of the callback return value

For more information on typespecs, consult the ["Typespecs"](typespecs.html) page in the Elixir documentation. As mentioned in this page, type specification are only annotations used by documentation and tools, so defining such specifications for behaviours serves mostly for such purposes.

### Optional callbacks

Optional callbacks are callbacks that callback modules may implement if they want to, but are not required to.
Usually, behaviour modules know if they should call those callbacks based on configuration, or they check if the callbacks are defined with `function_exported?/3` or `macro_exported?/3`.
Optional callbacks can be defined through the `@optional_callbacks` module attribute, which has to be a keyword list with function or macro name as key and arity as value. For example:

    defmodule MyBehaviour do
      @callback vital_fun() :: any
      @callback non_vital_fun() :: any
      @macrocallback non_vital_macro(arg :: any) :: Macro.t
      @optional_callbacks non_vital_fun: 0, non_vital_macro: 1
    end

One example of optional callback in Elixir's standard library is `c:GenServer.format_status/2`.

## Implementing behaviours

To specify that a module implements a given behaviour, the `@behaviour` attribute must be used:

    defmodule MyBehaviour do
      @callback my_fun(arg :: any) :: any
    end

    defmodule MyCallbackModule do
      @behaviour MyBehaviour
      def my_fun(arg), do: arg
    end
