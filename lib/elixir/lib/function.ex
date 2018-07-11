defmodule Function do
  @moduledoc """
  A set of functions for working with functions.

  There are two types of captured functions: **external** and **local**.
  External functions are functions residing in modules that are captured
  with `&/1`, such as `&String.length/1`. Local functions are anonymous functions
  defined with `fn/1` or with the capture operator `&/1` using `&1`, `&2`,
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

  @doc """
  Captures the given function.

  Inlined by the compiler.

  ## Examples

      iex> Function.capture(String, :length, 1)
      &String.length/1

  """
  @doc since: "1.7.0"
  @spec capture(module, atom, arity) :: fun
  def capture(module, function_name, arity) do
    :erlang.make_fun(module, function_name, arity)
  end

  @doc """
  Returns a keyword list with information about a function.

  The returned keys (with the corresponding possible values) for
  all types of functions (local and external) are the following:

    * `:type` - `:local` (for anonymous functions) or `:external` (for
      named functions).

    * `:module` - an atom which is the module where the function is defined when
    anonymous or the module which the function refers to when it's a named function.

    * `:arity` - (integer) the number of arguments the function is to be called with.

    * `:name` - (atom) the name of the function.

    * `:env` - a list of the environment or free variables. For named
      functions, the returned list is always empty.

  When `fun` is an anonymous function (that is, the type is `:local`), the following
  additional keys are returned:

    * `:pid` - PID of the process that originally created the function.

    * `:index` - (integer) an index into the module function table.

    * `:new_index` - (integer) an index into the module function table.

    * `:new_uniq` - (binary) a unique value for this function. It's
      calculated from the compiled code for the entire module.

    * `:uniq` - (integer) a unique value for this function. This integer is
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

      iex> fun = &String.length/1
      iex> info = Function.info(fun)
      iex> Keyword.get(info, :type)
      :external
      iex> Keyword.get(info, :name)
      :length

  """
  @doc since: "1.7.0"
  @spec info(fun) :: [{information, term}]
  def info(fun), do: :erlang.fun_info(fun)

  @doc """
  Returns a specific information about the function.

  The returned information is a two-element tuple in the shape of
  `{info, value}`.

  For any function, the information asked for can be any of the atoms
  `:module`, `:name`, `:arity`, `:env`, or `:type`.

  For anonymous functions, there is also information about any of the
  atoms `:index`, `:new_index`, `:new_uniq`, `:uniq`, and `:pid`.
  For a named function, the value of any of these items is always the
  atom `:undefined`.

  For more information on each of the possible returned values, see
  `info/1`.

  Inlined by the compiler.

  ## Examples

      iex> f = fn x -> x end
      iex> Function.info(f, :arity)
      {:arity, 1}
      iex> Function.info(f, :type)
      {:type, :local}

      iex> fun = &String.length/1
      iex> Function.info(fun, :name)
      {:name, :length}
      iex> Function.info(fun, :pid)
      {:pid, :undefined}

  """
  @doc since: "1.7.0"
  @spec info(fun, item) :: {item, term} when item: information
  def info(fun, item), do: :erlang.fun_info(fun, item)
end
