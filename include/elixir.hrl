-define(ELIXIR_WRAP_CALL(Line, Module, Method, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Method} },
    Args
  }).

-define(ELIXIR_PREPEND(Prefix, Atom), list_to_atom(lists:concat([Prefix, Atom]))).