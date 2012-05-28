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
  Receives an expression and expands its aliases returning an atom.
  Returns nil if the expression is not an alias or contains dynamic
  values.

  ## Examples

  In the example below, we have a macro that generates a module
  with a function named `name_length`. The value of this
  function will be calcualted at compilation time and not at
  runtime.

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
  `My.Module`. This function is a helper that does such
  expansion, abstracting all details for you. We could
  rewrite our macro above to use this function as:

      defmacro defmodule_with_length(name, do: block) do
        expanded = Macro.expand_aliases(name, __CALLER__)
        length   = length(atom_to_list(expanded))

        quote do
          defmodule unquote(name) do
            def name_length, do: unquote(length)
            unquote(block)
          end
        end
      end

  """

  # In case aliases just contain one item, we are sure
  # it is an atom, so we just expand it based on the
  # aliases dict.
  def expand_aliases({ :__aliases__, _, [h] }, env) do
    translate_aliases_head(h, env)
  end

  # In case aliases contains more than one item, we need
  # to loop them checking if they are all atoms or not.
  # Some expressions (like __MAIN__ and __MODULE__) are
  # allowed.
  def expand_aliases({ :__aliases__, _, [h|t] }, env) do
    aliases = case is_atom(h) do
      true  -> [translate_aliases_head(h, env)|t]
      false -> [h|t]
    end

    aliases = lc alias in aliases, do: translate_alias(alias, env)
    :lists.all(fn x -> x != false end, aliases) && Erlang.elixir_aliases.concat(aliases)
  end

  # In case is not an aliases, it may be __MAIN__, __MODULE__
  # or a bare atom, otherwise we return false.
  def expand_aliases(other, env) do
    translate_alias(other, env) || nil
  end

  ## Helpers

  defp translate_aliases_head(h, env) do
    atom = list_to_atom('__MAIN__.' ++ atom_to_list(h))
    Erlang.elixir_aliases.lookup(atom, env.aliases)
  end

  defp translate_alias({ { :".", _, [{ :__aliases__, _, [:Erlang] }, atom] }, _, args }, _env) when
    is_atom(atom) and (is_atom(args) or args == []), do: atom

  defp translate_alias({ :__MAIN__, _, atom }, _env) when is_atom(atom),  do: :__MAIN__
  defp translate_alias({ :__MODULE__, _, atom }, env) when is_atom(atom), do: env.module
  defp translate_alias(atom, _env) when is_atom(atom), do: atom
  defp translate_alias(_other, _env), do: false
end