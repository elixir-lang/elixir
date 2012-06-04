defmodule Macro do
  @moduledoc """
  This module provides conveniences for working with macros.
  """

  @doc """
  Recursively escapes the given value so it can be inserted
  into a syntax tree. Structures that are valid syntax nodes
  (like atoms, integers, binaries) are represented by themselves.

  ## Examples

      Macro.escape(:foo)
      #=> :foo

      Macro.escape({ :a, :b, :c })
      #=> { :{}, 0, [:a, :b, :c] }
  """
  def escape({ left, right }) do
    { escape(left), escape(right) }
  end

  def escape(tuple) when is_tuple(tuple) do
    { :{}, 0, escape(tuple_to_list(tuple)) }
  end

  def escape(list) when is_list(list) do
    lc item in list, do: escape(item)
  end

  def escape(other), do: other

  @doc """
  Receives an expression representation and expands it. The following
  contents are expanded:

  * Macros (local or remote) are expanded;
  * Aliases are expanded (if possible) and return atoms;
  * All pseudo-variables (__FILE__, __MODULE__, etc);

  In case the expression cannot be expanded, return the expression itself.

  ## Examples

  In the example below, we have a macro that generates a module
  with a function named `name_length` that returns the length
  of the module name. The value of this function will be calculated
  at compilation time and not at runtime.

  Consider the implementation below:

      defmacro defmodule_with_length(name, do: block) do
        length = length(atom_to_list(name))

        quote do
          defmodule unquote(name) do
            def name_length, do: unquote(length)
            unquote(block)
          end
        end
      end

  When invoked like this:

      defmodule_with_length My.Module do
        def other_function, do: ...
      end

  The compilation will fail because `My.Module` when quoted
  is not an atom, but a syntax tree as follow:

      {:__aliases__, 0, [:My, :Module] }

  That said, we need to expand the aliases node above to an
  atom, so we can retrieve its length. Expanding the node is
  not straight-forward because we also need to expand the
  caller aliases. For example:

      alias MyHelpers, as: My

      defmodule_with_length My.Module do
        def other_function, do: ...
      end

  The final module name will be `MyHelpers.Module` and not
  `My.Module`. With `Macro.expand`, such aliases are taken
  into consideration. Local and remote macros are also
  expanded. We could rewrite our macro above to use this
  function as:

      defmacro defmodule_with_length(name, do: block) do
        expanded = Macro.expand(name, __CALLER__)
        length   = length(atom_to_list(expanded))

        quote do
          defmodule unquote(name) do
            def name_length, do: unquote(length)
            unquote(block)
          end
        end
      end

  """
  def expand(aliases, env)

  # The first case we handle is __aliases__. In case
  # aliases just contain one item, we are sure it is
  # an atom, so we just expand it based on the aliases
  # dict.
  def expand({ :__aliases__, _, [h] }, env) do
    expand_alias(h, env)
  end

  # In case aliases contains more than one item, we need
  # to loop them checking if they are all atoms or not.
  # Macros and pseudo-variables are then expanded.
  def expand({ :__aliases__, _, [h|t] }, env) do
    aliases = case is_atom(h) do
      true  -> [expand_alias(h, env)|t]
      false -> [h|t]
    end

    aliases = lc alias in aliases, do: expand(alias, env)
    :lists.all(is_atom(&1), aliases) && Erlang.elixir_aliases.concat(aliases)
  end

  # Expand Erlang.foo calls
  def expand({ { :".", _, [{ :__aliases__, _, [:Erlang] }, atom] }, _, args }, _env) when
    is_atom(atom) and (is_atom(args) or args == []), do: atom

  # Expand pseudo-variables
  def expand({ :__MAIN__, _, atom }, _env)  when is_atom(atom), do: :__MAIN__
  def expand({ :__MODULE__, _, atom }, env) when is_atom(atom), do: env.module
  def expand({ :__FILE__, _, atom }, env)   when is_atom(atom), do: env.file
  def expand({ :__ENV__, _, atom }, env)    when is_atom(atom), do: env

  # Expand possible macro invocation
  # translate_each({Atom, Line, Args} = Original, S) when is_atom(Atom) ->
  # translate_each({{'.', _, [Left, Right]}, Line, Args} = Original, S) when is_atom(Right) ->

  # Anything else is just returned
  def expand(other, _env), do: other

  ## Helpers

  defp expand_alias(h, env) do
    atom = list_to_atom('__MAIN__.' ++ atom_to_list(h))
    Erlang.elixir_aliases.lookup(atom, env.aliases)
  end
end