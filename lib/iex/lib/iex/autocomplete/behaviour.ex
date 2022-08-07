defmodule IEx.Autocomplete.Behaviour do
  @moduledoc """
  New IEx autocomplete mechanisms need to expose
  two functions:

  - `expandable_fragment/1`, which receives a *reversed* charlist of the current IEx statement, and returns a charlist
  of what can be expanded by the current autocomplete mechanism.
  If the charlist is empty, it means the current autocomplete mechanism can't expand the current IEx statement.

  - `expand/2`, which receives a *reversed* charlist of the current IEx statement,
  and yields a `{:yes, hint, available_options}` or `{:no, '', []}`.

  In the `:yes` tuple, `hint` is a charlist,
  and `available_options` is a list of charlists:
    - If `hint` is defined (its length > 0), it means the IEx shell will append the hint to the current statement;
    - If `available_options` are defined (it has at least one element of length > 0), it means the IEx shell will
  print the available options to the user.
  """

  @type code :: List.Chars.t()
  @type hint :: List.Chars.t()
  @type available_options :: [List.Chars.t()]
  @type yes_tuple :: {:yes, hint, available_options}
  @type no_tuple :: {:no, '', []}

  @callback expandable_fragment(code) :: List.Chars.t()
  @callback expand(code, IEx.Broker.shell()) :: yes_tuple | no_tuple
end
