defmodule Function do
  @moduledoc """
  A set of funtions for working with functions.

  We can have two types of captures functions - external and local -
  external functions are our normal functions residing in modules while
  local are the anonymous functions defined with `fn` or the capture
  operator `&`.
  """

  @doc """
  Captures a function by a given module, function name and arity.

  Useful when you want to capture a function when having variables.

  ## Examples

    iex> Function.capture(String, :length, 1)
    &String.length/1
  """
  @type information ::
          :type
          | :module
          | :arity
          | :name
          | :env
          | :pid
          | :index
          | :new_index
          | :new_uniq
          | :uniq
  @type value :: any

  @spec capture(atom, atom, integer) :: (... -> any)
  def capture(mod, fun, arity) when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    :erlang.make_fun(mod, fun, arity)
  end

  @doc """
  Returns a keyword list with information about a function.

  The {key,value}s will include the following:
    - :type - :local(for anonymous functions) or :external(for named functions)
    - :module - an atom - the module where the function is defined when anonymous
    or the module which the function refers to when it's a named one.
    - :arity - the number of arguments the function is to be called with
    - :name - the name of the functions
    - :env - a list of the environment or free variables. For named functions, the returned list is always empty.

  When it is an anonymous function it will also return info about:
    - :pid - process identifier of the process that originally created the fun
    - :index - an integer - is an index into the module fun table.
    - :new_index - an integer - is an index into the module fun table.
    - :new_uniq - a binary-  it's a unique value for this fun. It is calculated from the compiled code for the entire module.
    - :uniq - an integer, a unique value for this fun. As from Erlang/OTP R15, this integer is calculated from the compiled code for the entire module. Before Erlang/OTP R15, this integer was based on only the body of the fun.


  ## Examples
    iex> fun = fn x -> x end
    iex> info = Function.info(fun)
    iex> info |> Keyword.get(:arity)
    1
    iex> info |> Keyword.get(:type)
    :local
  """
  @spec info((... -> any)) :: [{information, value}]
  def info(fun) when is_function(fun), do: :erlang.fun_info(fun)

  @doc """
  Returns a tuple of information about the function in the form {:info, information}.

  For any function, the information asked for can be any of the atoms :module, :name, :arity, :env, or :type.

  For anonymous functions fun, there is also information about any of the atoms :index, :new_index, :new_uniq, :uniq, and :pid.
  For a named function, the value of any of these items is always the atom :undefined.

  For more information on each of the value returned - check the docs for Function.info/1.

  ## Examples

    iex> f = fn x -> x end
    iex> Function.info(f, :arity)
    {:arity, 1}

  """
  @spec info((... -> any)) :: {information, value}
  def info(fun, item) when is_function(fun) and is_atom(item) do
    :erlang.fun_info(fun, item)
  end
end
