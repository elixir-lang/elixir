-module(elixir).
-export([start/0, file/1, eval/1, eval/2, eval/3, eval/4, eval/5, parse/2, parse/3]).
-export([compile_core/0]).
-include("elixir.hrl").

% OTP Application API
-export([start_app/0, start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  code:ensure_loaded(elixir_object_methods),
  [code:ensure_loaded(Module) || Module <- builtin_mixins()],
  elixir_sup:start_link([]).

stop(_S) ->
  ok.

config_change(_Changed, _New, _Remove) ->
  ok.

% Start elixir as an application.
start_app() ->
  case lists:keyfind(?MODULE,1, application:loaded_applications()) of
    false -> application:start(?MODULE);
    _ -> ok
  end.

% Boot and process given options. Invoked by Elixir's script.
start() ->
  start_app(),
  CodeInit = elixir_constants:lookup('Code::Init'),
  'exCode::Init':process_argv(CodeInit, init:get_plain_arguments()).

builtin_mixins() ->
  [
    'exInteger::Proto',
    'exFloat::Proto',
    'exAtom::Proto',
    'exList::Proto',
    'exString::Proto',
    'exBitString::Proto',
    'exOrderedDict::Proto',
    'exTuple::Proto',
    'exFunction::Proto',
    'exProcess::Proto',
    'exReference::Proto',
    'exPort::Proto'
  ].

file(Filepath) ->
  file(Filepath, [], #elixir_scope{}).

file(Filepath, Binding, Scope) ->
  List = read_file(Filepath),
  eval(List, Binding, Filepath, 1, Scope).

% Read a file as utf8
read_file(FileName) ->
  {ok, Device} = file:open(FileName, [read, {encoding, utf8}]),
  read_file(Device, []).

read_file(Device, Acc) ->
  case io:get_line(Device, "") of
    eof  ->
      file:close(Device),
      lists:append(lists:reverse(Acc));
    Line ->
      read_file(Device, [Line|Acc])
  end.

% Evaluates a string
eval(String) -> eval(String, []).
eval(String, Binding) -> eval(String, Binding, "nofile").
eval(String, Binding, Filename) -> eval(String, Binding, Filename, 1).
eval(String, Binding, Filename, Line) -> eval(String, Binding, Filename, Line, #elixir_scope{}).
eval(String, Binding, Filename, Line, Scope) ->
  SelfBinding = case proplists:get_value(self, Binding) of
    undefined -> lists:append(Binding, [{self,elixir_constants:lookup('Object')}]);
    _  -> Binding
  end,
  { ParseTree, NewScope } = parse(String, SelfBinding, Filename, Line, Scope),
  {value, Value, NewBinding} = erl_eval:exprs(ParseTree, SelfBinding),
  {Value, final_binding(NewBinding, NewScope#elixir_scope.vars) }.

% Parse string and transform tree to Erlang Abstract Form format
parse(String, Binding) -> parse(String, Binding, "nofile").
parse(String, Binding, Filename) -> parse(String, Binding, Filename, 1, #elixir_scope{}).
parse(String, Binding, Filename, Line, Scope) ->
  NewScope = Scope#elixir_scope{vars=binding_dict(Binding), filename=Filename},
  elixir_transform:parse(String, Line, NewScope).

binding_dict(List) -> binding_dict(List, dict:new()).
binding_dict([{self,_}|T], Dict) -> binding_dict(T, Dict);
binding_dict([{H,_}|T], Dict) -> binding_dict(T, dict:store(H, H, Dict));
binding_dict([], Dict) -> Dict.

final_binding(Binding, Vars) -> final_binding(Binding, [], Binding, Vars).
final_binding([{self,_}|T], Acc, Binding, Vars) -> final_binding(T, Acc, Binding, Vars);
final_binding([{Var,_}|T], Acc, Binding, Vars) ->
  case atom_to_list(Var) of
    [$X|_] -> final_binding(T, Acc, Binding, Vars);
    _ ->
      RealName = dict:fetch(Var, Vars),
      RealValue = proplists:get_value(RealName, Binding, nil),
      final_binding(T, [{Var, RealValue}|Acc], Binding, Vars)
  end;

final_binding([], Acc, _Binding, _Vars) -> lists:reverse(Acc).

%% Compilation helpers

internal_file(File) ->
  file(File, [{self,nil}], #elixir_scope{compile_path="exbin"}).

compile_core() ->
  code:ensure_loaded(elixir_object_methods),
  [internal_file(File) || File <- compile_main()],
  AllLists = [filelib:wildcard(Wildcard) || Wildcard <- compile_list()],
  Files = lists:append(AllLists) -- compile_main(),
  [file(File) || File <- Files].

compile_list() ->
  [
    "lib/*.ex",
    "lib/*/*.ex"
  ].

compile_main() ->
  [
    "lib/object.ex",
    "lib/module.ex",
    "lib/io.ex",
    "lib/atom.ex",
    "lib/list.ex",
    "lib/numeric.ex",
    "lib/integer.ex",
    "lib/float.ex",
    "lib/tuple.ex",
    "lib/string.ex",
    "lib/ordered_dict.ex",
    "lib/regexp.ex",
    "lib/bit_string.ex",
    "lib/process.ex",
    "lib/port.ex",
    "lib/reference.ex",
    "lib/function.ex",
    "lib/gen_server.ex",
    "lib/record.ex",
    "lib/file.ex",
    "lib/code.ex",
    "lib/code/formatter.ex",
    "lib/code/init.ex",
    "lib/code/server.ex",
    "lib/code/compiler.ex"
  ].