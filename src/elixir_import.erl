%% There are two kind of imports. One done by `import` and
%% another by `require`. The first one matters only for local
%% calls and the second one for macros.
-module(elixir_import).
-export([macro_imports/0, local_imports/1,
  calculate/7, handle_import/4, handle_import/5,
  format_error/1,
  ensure_no_macro_conflict/4, ensure_no_local_conflict/4,
  build_table/1, delete_table/1, record/4]).
-include("elixir.hrl").
-compile({inline,[in_erlang_macros/0]}).

%% Create tables that are responsible to store
%% import and macro invocations.

macro_table(Module)  -> ?ELIXIR_ATOM_CONCAT([m, Module]).
import_table(Module) -> ?ELIXIR_ATOM_CONCAT([i, Module]).

build_table(Module) ->
  ets:new(macro_table(Module),  [set, named_table, private]),
  ets:new(import_table(Module), [set, named_table, private]).

delete_table(Module) ->
  ets:delete(macro_table(Module)),
  ets:delete(import_table(Module)).

record(_Kind, _Tuple, _Receiver, #elixir_scope{module={0, nil}}) ->
  [];

record(macro, Tuple, Receiver, #elixir_scope{module={_,Module}}) ->
  ets:insert(macro_table(Module), { Tuple, Receiver }).

%% Handle the import macro declaration.

handle_import(Line, Filename, Module, Ref) ->
  handle_import(Line, Filename, Module, Ref, []).

handle_import(Line, Filename, Module, Ref, Opts) when is_atom(Ref), is_list(Opts) ->
  Table = import_table(Module),
  ets:delete(Table, Ref),
  All = ets:tab2list(Table),
  Imports = calculate(Line, Filename, Ref, Opts, All, fun() -> Ref:module_info(exports) end, false),
  ets:insert(Table, Imports);

handle_import(Line, Filename, _Module, _Ref, _Opts) ->
  elixir_errors:syntax_error(Line, Filename, "invalid args for: ", "import").

%% Calculate the new { Key, List } tuple of imports
%% according to the function and options given.
%% If the Kind option is not false, we also check if the
%% only options given are valid and raise an error
%% if not. We usually pass the Kind option for require
%% but not for import because we want to allow importing
%% of not defined modules (similar to Erlang).

calculate(Line, Filename, Key, Opts, All, Fun, Kind) ->
  New = case orddict:find(only, Opts) of
    { ok, Only } ->
      Condition = Kind /= false andalso (Only -- Fun()),
      case Condition of
        [{Name,Arity}|_] ->
          Tuple = { invalid_import, { Key, Name, Arity, Kind } },
          elixir_errors:form_error(Line, Filename, ?MODULE, Tuple);
        _ -> Only
      end;
    error ->
      case orddict:find(except, Opts) of
        { ok, Except } -> Fun() -- Except;
        error -> Fun()
      end
  end,
  ensure_no_conflicts(Line, Filename, New, All),
  ensure_no_in_erlang_macro_conflict(Line, Filename, Key, New),
  {Key,New}.

%% Return configured imports and defaults

local_imports(Module) -> ets:tab2list(import_table(Module)).

macro_imports() ->
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

%% Check if any of the locals invoked conflicts with in Erlang macros.
ensure_no_local_conflict(_Line, _Filename, _Module, _AllDefined) ->
  [].

%% Find conlicts in the given list of functions with the recorded set
%% of macros. Erlang automatically checks for conflict with imported
%% locals, so we need to handle the case just with macros.

ensure_no_macro_conflict(Line, Filename, Module, AllDefined) ->
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

%% Find conlicts in the given list of functions with the set of imports.
%% Used internally to ensure a newly imported fun or macro does not
%% conflict with an already imported set.

ensure_no_conflicts(Line, Filename, Functions, [{Key,Value}|T]) ->
  Filtered = lists:filter(fun(X) -> lists:member(X, Functions) end, Value),
  case Filtered of
    [{Name,Arity}|_] ->
      Tuple = { already_imported, { Key, Name, Arity } },
      elixir_errors:form_error(Line, Filename, ?MODULE, Tuple);
    [] ->
      ensure_no_conflicts(Line, Filename, Functions, T)
  end;

ensure_no_conflicts(_Line, _Filename, _Functions, _S) -> ok.

%% Error handling

format_error({already_imported,{Receiver, Name, Arity}}) ->
  io_lib:format("function ~s/~B already imported from ~s", [Name, Arity, Receiver]);

format_error({macro_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("used imported macro ~s.~s/~B conflicts with local function or import", [Receiver, Name, Arity]);

format_error({invalid_import,{Receiver, Name, Arity, macro}}) ->
  io_lib:format("~s.~s/~B isn't a macro and can't be imported using require. Maybe you wanted to use import?", [Receiver, Name, Arity]);

format_error({import_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("could not import ~s.~s/~B because it conflicts with Elixir internal macros", [Receiver, Name, Arity]).

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
    {'apply',3},
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
    {'^','1'},
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
    {'module_ref',1},
    {'::','*'},
    {'def','*'},
    {'defp','*'},
    {'defmacro','*'},
    {'quote','1'},
    {'unquote','1'},
    {'unquote_splice','1'},
    {'fn','*'},
    {'receive','*'},
    {'try','*'},
    {'loop','*'}
  ]).