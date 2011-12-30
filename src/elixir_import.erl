%% There are two kind of imports. One done by `import` and
%% another by `require`. The first one matters only for local
%% calls and the second one for macros.
-module(elixir_import).
-export([local_imports/0, macro_imports/0,
  update/6, only_imports/1,
  format_error/1, ensure_no_macro_conflict/4,
  build_table/1, delete_table/1, record/4]).
-include("elixir.hrl").
-compile({inline,[in_erlang_macros/0]}).

%% Create tables that are responsible to store
%% local and macro invocations.

macro_table(Module)  -> ?ELIXIR_ATOM_CONCAT([m, Module]).
local_table(Module)  -> ?ELIXIR_ATOM_CONCAT([l, Module]).
only_table(Module)   -> ?ELIXIR_ATOM_CONCAT([o, Module]).

build_table(Module) ->
  ets:new(macro_table(Module),  [set, named_table, private]),
  ets:new(local_table(Module),  [bag, named_table, private]),
  ets:new(only_table(Module),   [set, named_table, private]).

delete_table(Module) ->
  ets:delete(macro_table(Module)),
  ets:delete(local_table(Module)),
  ets:delete(only_table(Module)).

record(_Kind, _Tuple, _Receiver, #elixir_scope{module={0, nil}}) ->
  [];

record(Kind, Tuple, Receiver, #elixir_scope{module={_,Module}}) ->
  record_(Kind, Tuple, Receiver, Module).

record_(local, Tuple, _, Module) ->
  ets:insert(local_table(Module), Tuple);

record_(macro, Tuple, Receiver, Module) ->
  ets:insert(macro_table(Module), { Tuple, Receiver });

record_(only, Imports, Receiver, Module) ->
  ets:insert(only_table(Module), { Receiver, Imports }).

%% Update the ListOfTuples by removing any previous
%% value of Key and adding new ones according to the rules
%% given by Opts or the default value in Fun.

update(Line, Key, ListOfTuples, Opts, Fun, S) ->
  OldImports = lists:keydelete(Key, 1, ListOfTuples),

  New = case orddict:find(only, Opts) of
    { ok, Only } -> Only;
    error ->
      case orddict:find(except, Opts) of
        { ok, Except } -> Fun() -- Except;
        error -> Fun()
      end
  end,

  ensure_no_in_erlang_macro_conflict(Line, Key, New, S),
  [{Key,New}|OldImports].

%% Return configured imports

only_imports(Module) -> ets:tab2list(only_table(Module)).

%% Return default local and macros for import

local_imports() -> [].

macro_imports() ->
  [
    { '::Elixir::Macros', in_elixir_macros() }
  ].

%% Ensure the given functions don't clash with any
%% of Elixir "implemented in Erlang" macros.

ensure_no_in_erlang_macro_conflict(Line, Key, [{Name,Arity}|T], S) ->
  InErlang = in_erlang_macros(),
  case orddict:find(Name, InErlang) of
    { ok, Value } ->
      case (Value == '*') or (Value == Arity) of
        true  ->
          Tuple = { import_conflict, { Key, Name, Arity } },
          elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple);
        false -> ensure_no_in_erlang_macro_conflict(Line, Key, T, S)
      end;
    error -> ensure_no_in_erlang_macro_conflict(Line, Key, T, S)
  end;

ensure_no_in_erlang_macro_conflict(_Line, _Key, [], _S) -> ok.

%% Find conlicts in the given list of functions with the set of imports.

ensure_no_macro_conflict(Line, Module, AllDefined, S) ->
  %% Find functions that were defined and invoked
  Table = macro_table(Module),
  Matches = [X || X <- AllDefined, ets:member(Table, X)],

  case Matches of
    [{Name,Arity}|_] ->
      Key = ets:lookup_element(Table, {Name, Arity }, 2),
      Tuple = { macro_conflict, { Key, Name, Arity } },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple);
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