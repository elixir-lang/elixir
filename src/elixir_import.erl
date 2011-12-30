%% There are two kind of imports. One done by `import` and
%% another by `require`. The first one matters only for local
%% calls and the second one for macros.
-module(elixir_import).
-export([default_imports/0, only_imports/1,
  calculate/5, handle_import/5,
  format_error/1, ensure_no_macro_conflict/4,
  build_table/1, delete_table/1, record/4]).
-include("elixir.hrl").
-compile({inline,[in_erlang_macros/0]}).

%% Create tables that are responsible to store
%% import and macro invocations.

macro_table(Module)  -> ?ELIXIR_ATOM_CONCAT([m, Module]).
only_table(Module)   -> ?ELIXIR_ATOM_CONCAT([o, Module]).

build_table(Module) ->
  ets:new(macro_table(Module),  [set, named_table, private]),
  ets:new(only_table(Module),   [set, named_table, private]).

delete_table(Module) ->
  ets:delete(macro_table(Module)),
  ets:delete(only_table(Module)).

record(_Kind, _Tuple, _Receiver, #elixir_scope{module={0, nil}}) ->
  [];

record(macro, Tuple, Receiver, #elixir_scope{module={_,Module}}) ->
  ets:insert(macro_table(Module), { Tuple, Receiver }).

%% Handle the import macro declaration.

handle_import(Line, Filename, Module, Ref, [{only,_}] = Opts) when is_atom(Ref) ->
  Imports = calculate(Line, Filename, Ref, Opts, fun() -> Ref:module_info(exports) end),
  ets:insert(only_table(Module), Imports);

handle_import(Line, Filename, _Module, _Ref, _Opts) ->
  elixir_errors:syntax_error(Line, Filename, "invalid args for: ", "import").

%% Calculate the new { Key, List } tuple of imports
%% according to the function and options given.

calculate(Line, Filename, Key, Opts, Fun) ->
  New = case orddict:find(only, Opts) of
    { ok, Only } -> Only;
    error ->
      case orddict:find(except, Opts) of
        { ok, Except } -> Fun() -- Except;
        error -> Fun()
      end
  end,
  ensure_no_in_erlang_macro_conflict(Line, Filename, Key, New),
  {Key,New}.

%% Return configured imports and defaults

only_imports(Module) -> ets:tab2list(only_table(Module)).

default_imports() ->
  [
    { '::Elixir::Macros', in_elixir_macros() }
  ].

%% Ensure the given functions don't clash with any
%% of Elixir "implemented in Erlang" macros.

ensure_no_in_erlang_macro_conflict(Line, Filename, Key, [{Name,Arity}|T]) ->
  InErlang = in_erlang_macros(),
  case orddict:find(Name, InErlang) of
    { ok, Value } ->
      case (Value == '*') or (Value == Arity) of
        true  ->
          Tuple = { import_conflict, { Key, Name, Arity } },
          elixir_errors:form_error(Line, Filename, ?MODULE, Tuple);
        false -> ensure_no_in_erlang_macro_conflict(Line, Filename, Key, T)
      end;
    error -> ensure_no_in_erlang_macro_conflict(Line, Filename, Key, T)
  end;

ensure_no_in_erlang_macro_conflict(_Line, _Filename, _Key, []) -> ok.

%% Find conlicts in the given list of functions with the set of imports.

ensure_no_macro_conflict(Line, Filename, Module, AllDefined) ->
  %% Find functions that were defined and invoked
  Table = macro_table(Module),
  Matches = [X || X <- AllDefined, ets:member(Table, X)],

  case Matches of
    [{Name,Arity}|_] ->
      Key = ets:lookup_element(Table, {Name, Arity }, 2),
      Tuple = { macro_conflict, { Key, Name, Arity } },
      elixir_errors:form_error(Line, Filename, ?MODULE, Tuple);
    [] ->
      ok
  end.

%% Error handling

format_error({macro_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("used imported macro ~s#~s/~B conflicts with local function or import", [Receiver, Name, Arity]);

format_error({import_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("could not import ~s#~s/~B because it conflicts with Elixir internal macros", [Receiver, Name, Arity]).

%% Macros implemented in Elixir - imported by default

in_elixir_macros() ->
  try
    '::Elixir::Macros':'__macros__'()
  catch
    error:undef -> []
  end.

%% Macros implemented in Erlang - imported and non overridable by default

in_erlang_macros() ->
  orddict:from_list([
    {'=',2},
    {'+',1},
    {'+',2},
    {'-',1},
    {'-',2},
    {'*',2},
    {'/',2},
    {'<-',2},
    {'++',2},
    {'--',2},
    {'andalso',2},
    {'orelse',2},
    {'not',1},
    {'and',2},
    {'or',2},
    {'xor',2},
    {'<',2},
    {'>',2},
    {'<=',2},
    {'>=',2},
    {'==',2},
    {'!=',2},
    {'===',2},
    {'!==',2},
    {erlang_op,1},
    {erlang_op,2},
    {'case','*'},
    {'block','*'},
    {'kv_block','*'},
    {'bitstr','*'},
    {'{}','*'},
    {'use','*'},
    {'require','*'},
    {'module','*'},
    {'__MODULE__','*'},
    {'__FILE__','*'},
    {'__LINE__','*'},
    {'import','*'},
    {'ref',1},
    {'::','*'},
    {'def','*'},
    {'defp','*'},
    {'defmacro','*'},
    {'quote','*'},
    {'fn','*'},
    {'receive','*'},
    {'try','*'}
  ]).