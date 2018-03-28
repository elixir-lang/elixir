defmodule Function do
  @moduledoc """
  A set of funtions for working with functions.

  We can have two types of captures functions: external and local.
  External functions are functions residing in modules that are captured
  with `&/1`, such as `&String.length/1`. Local are the anonymous functions
  defined with `fn/1` or with the capture operator `&/1` using `&1`, `&2`
  and so on as replacements.
  """

  @type information ::
          :arity
          | :env
          | :index
          | :module
          | :name
          | :new_index
          | :new_uniq
          | :pid
          | :type
          | :uniq
  @type value :: any

  @doc """
  Captures a function by a given `module`, `function_name` and `arity`.

  Inlined by the compiler.

  ## Examples

      iex> Function.capture(String, :length, 1)
      &String.length/1

  """
  @spec capture(module, atom, integer) :: fun
  def capture(module, function_name, arity) do
    :erlang.make_fun(module, function_name, arity)
  end

  @doc """
  Returns a keyword list with information about a function.

  The `{key, value}`s will include the following:

    * `:type` - `:local` (for anonymous functions) or `:external` (for
  named functions)
    * `:module` - an atom - the module where the function is defined when
    anonymous or the module which the function refers to when it's a named one.
    * `:arity` - the number of arguments the function is to be called with
    * `:name` - the name of the functions
    * `:env` - a list of the environment or free variables. For named
      functions, the returned list is always empty.

  When it is an anonymous function it will also return info about:
    * `:pid` - process identifier of the process that originally created
  the funciton
    * `:index` - an integer - is an index into the module function table.
    * `:new_index` - an integer - is an index into the module function table.
    * `:new_uniq` - a binary - it's a unique value for this function. It is
      calculated from the compiled code for the entire module.
    * `:uniq` - an integer, a unique value for this function. This integer is
      calculated from the compiled code for the entire module.

  **Note**: this function must be used only for debugging purposes.

  Inlined by the compiler.

  ## Examples

      iex> fun = fn x -> x end
      iex> info = Function.info(fun)
      iex> Keyword.get(info, :arity)
      1
      iex> Keyword.get(info, :type)
      :local

  """
  @spec info(fun) :: [{information, value}]
  def info(fun), do: :erlang.fun_info(fun)

  @doc """
  Returns a tuple of information about the function in the form
  `{:info, information}`.

  For any function, the information asked for can be any of the atoms
  `:module`, `:name`, `:arity`, `:env`, or `:type`.

  For anonymous functions, there is also information about any of the
  atoms `:index`, `:new_index`, `:new_uniq`, `:uniq`, and `:pid`.
  For a named function, the value of any of these items is always the
  atom `:undefined`.

  For more information on each of the value returned. Check the docs for
  `Function.info/1`.

  Inlined by the compiler.

  ## Examples

      iex> f = fn x -> x end
      iex> Function.info(f, :arity)
      {:arity, 1}
      iex> Function.info(f, :type)
      {:type, :local}

  """
  @spec info(fun) :: {information, value}
  def info(fun, item), do: :erlang.fun_info(fun, item)
end
