-define(ELIXIR_WRAP_CALL(Module, Method, Args),
  { call, 0,
    { remote, 0, { atom, 0, Module }, { atom, 0, Method} },
    Args
  }).
