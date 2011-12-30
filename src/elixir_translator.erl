-module(elixir_translator).
-export([translate/2, translate_each/2, parse/3]).
-include("elixir.hrl").

parse(String, Line, #elixir_scope{filename=Filename} = S) ->
  Forms = forms(String, Line, Filename),
  translate(Forms, S).

forms(String, StartLine, Filename) ->
  try elixir_tokenizer:tokenize(String, StartLine) of
    {ok, Tokens} ->
      case elixir_parser:parse(Tokens) of
        {ok, Forms} -> Forms;
        {error, {Line, _, [Error, Token]}} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
      end;
    {error, {Line, Error, Token}} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
  catch
    { interpolation_error, { Line, Error, Token } } -> elixir_errors:syntax_error(Line, Filename, Error, Token)
  end.

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Assignment operator

translate_each({'=', Line, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL } = elixir_clauses:assigns(fun translate_each/2, Left, SR),
  { { match, Line, TLeft, TRight }, SL };

%% Operators

translate_each({ '+', _Line, [Expr] }, S) when is_number(Expr) ->
  translate_each(Expr, S);

translate_each({ '-', _Line, [Expr] }, S) when is_number(Expr) ->
  translate_each(-1 * Expr, S);

translate_each({ Op, Line, Exprs }, S) when is_list(Exprs),
  Op == '+'; Op == '-'; Op == '*'; Op == '/'; Op == '<-';
  Op == '++'; Op == '--'; Op == 'andalso'; Op == 'orelse';
  Op == 'not'; Op == 'and'; Op == 'or'; Op == 'xor';
  Op == '<'; Op == '>'; Op == '<='; Op == '>=';
  Op == '=='; Op == '!='; Op == '==='; Op == '!==' ->
  translate_each({ erlang_op, Line, [Op|Exprs] }, S);

%% Operators

translate_each({ erlang_op, Line, [Op, Expr] }, S) when is_atom(Op) ->
  { TExpr, NS } = translate_each(Expr, S),
  { { op, Line, convert_op(Op), TExpr }, NS };

translate_each({ erlang_op, Line, [Op|Args] }, S) when is_atom(Op) ->
  { [TLeft, TRight], NS }  = translate_args(Args, S),
  { { op, Line, convert_op(Op), TLeft, TRight }, NS };

%% Case

translate_each({'case', Line, [Expr, RawClauses]}, S) ->
  Clauses = orddict:erase(do, RawClauses),
  { TExpr, NS } = translate_each(Expr, S),
  { TClauses, TS } = elixir_clauses:match(Line, Clauses, NS),
  { { 'case', Line, TExpr, TClauses }, TS };

translate_each({'case', Line, Args}, S) when is_list(Args) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", "case");

%% Blocks

translate_each({ block, Line, [] }, S) ->
  { { atom, Line, nil }, S };

translate_each({ block, _Line, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ block, Line, Args }, S) when is_list(Args) ->
  { TArgs, NS } = translate(Args, S),
  { { block, Line, TArgs }, NS };

translate_each({ kv_block, Line, Args }, S) when is_list(Args) ->
  case S#elixir_scope.macro of
    { Receiver, Name, Arity } ->
      Desc = io_lib:format("~s.~s/~B", [Receiver, Name, Arity]),
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "key value blocks not supported by: ", Desc);
    _ ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "unhandled key value blocks", "")
  end;

%% Bit strings

translate_each({ bitstr, Line, Args }, S) when is_list(Args) ->
  { TArgs, { SC, SV } } = elixir_tree_helpers:build_bitstr(fun translate_arg/2, Args, Line, { S, S }),
  { TArgs, umergec(SV, SC) };

%% Containers

translate_each({'{}', Line, Args}, S) when is_list(Args) ->
  { TArgs, SE } = translate_args(Args, S),
  { {tuple, Line, TArgs}, SE };

%% Modules directives

translate_each({use, Line, [Ref|Args]}, S) ->
  case S#elixir_scope.module of
    {0,nil} ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for: ", "use");
    {_,Module} ->
      Call = { block, Line, [
        { require, Line, [Ref] },
        { { '.', Line, [Ref, '__using__'] }, Line, [Module|Args] }
      ] },
      translate_each(Call, S)
  end;

translate_each({require, Line, [Left]}, S) ->
  translate_each({ require, Line, [Left, []]}, S);

translate_each({require, Line, [Left,Opts]}, S) ->
  Right  = proplists:get_value(as, Opts, false),

  { TLeft, SL }  = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, SL#elixir_scope{noref=true}),

  { Old, New } = case { TLeft, TRight } of
    { { atom, _, ALeft }, { atom, _, false } } ->
      { ALeft, ALeft };
    { { atom, _, ALeft }, { atom, _, ARight } } ->
      { ALeft, ARight };
    _ ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid name for: ", "require")
  end,

  Truthy = fun(X) -> proplists:get_value(X, Opts, false ) /= false end,
  Import = lists:any(Truthy, [import, only, except]),

  %% Handle given :as
  elixir_module:ensure_loaded(Line, Old, S, Import),

  %% Handle given :import
  IS = case Import of
    true ->
      NewImports = elixir_import:update(Line, Old, SR#elixir_scope.imports,
        Opts, fun() -> elixir_macro:get_macros(Line, Old, S) end, SR),
      SR#elixir_scope{imports=NewImports};
    false -> SR
  end,

  %% Return result
  Tuple = { tuple, Line, [{atom, Line, Old}, {atom, Line, New}] },

  { Tuple, IS#elixir_scope{
    refer=orddict:store(New, Old, S#elixir_scope.refer),
    noref=S#elixir_scope.noref
  } };

%% Modules

translate_each({module, Line, [Ref, [{do,Block}]]}, S) ->
  { TRef, _ } = translate_each(Ref, S#elixir_scope{noref=true}),

  NS = case TRef of
    { atom, _, Module } ->
      S#elixir_scope{scheduled=[Module|S#elixir_scope.scheduled]};
    _ -> S
  end,

  { elixir_module:transform(Line, S#elixir_scope.filename, TRef, Block), NS };

%% Built-in macros

translate_each({'__MODULE__', Line, []}, S) ->
  { _, Module } = S#elixir_scope.module,
  { { atom, Line, Module }, S };

translate_each({'__LINE__', Line, []}, S) ->
  { { integer, Line, Line }, S };

translate_each({'__FILE__', _Line, []}, S) ->
  translate_each(list_to_binary(S#elixir_scope.filename), S);

%% References

translate_each({ref, Line, [Ref]}, S) when is_atom(Ref) ->
  Atom = list_to_atom("::" ++ atom_to_list(Ref)),
  { _, Module } = S#elixir_scope.module,

  Final = case S#elixir_scope.noref or (Module == nil) of
    true  -> Atom;
    false -> elixir_ref:lookup(Atom, S#elixir_scope.refer)
  end,

  { {atom, Line, Final }, S };

translate_each({'::', Line, [Left]}, S) ->
  translate_each({'::', Line, [nil,Left]}, S);

translate_each({'::', Line, [Left|Right]}, S) ->
  { TLeft, LS } = translate_each(Left, S),
  { TRight, RS } = translate_args(Right, (umergec(S, LS))#elixir_scope{noref=true}),
  TArgs = [TLeft|TRight],
  Atoms = [Atom || { atom, _, Atom } <- TArgs],
  Final = case length(Atoms) == length(TArgs) of
    true  -> { atom, Line, elixir_ref:concat(Atoms) };
    false ->
      FArgs = [elixir_tree_helpers:build_simple_list(Line, TArgs)],
      ?ELIXIR_WRAP_CALL(Line, elixir_ref, concat, FArgs)
  end,
  { Final, (umergev(LS, RS))#elixir_scope{noref=S#elixir_scope.noref} };

%% Def

translate_each({Kind, Line, [Call,[{do, Expr}]]}, S) when Kind == def; Kind == defp; Kind == defmacro ->
  Module = S#elixir_scope.module,
  case (Module == {0,nil}) or (S#elixir_scope.function /= []) of
    true -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for: ", atom_to_list(Kind));
    _ ->
      { TCall, Guards } = elixir_clauses:extract_guards(Call),
      { Name, Args } = elixir_clauses:extract_args(TCall),
      ClauseScope = S#elixir_scope{function=Name, counter=0, vars=dict:new()},
      { TClause, _ } = elixir_clauses:assigns_blocks(Line, fun translate/2, Args, [Expr], Guards, ClauseScope),

      Arity = length(element(3, TClause)),
      { Unpacked, Defaults } = elixir_def_defaults:unpack(Name, TClause),
      Function = { function, Line, Name, Arity, [Unpacked] },
      { elixir_def:wrap_definition(Kind, Line, S#elixir_scope.filename, element(2, Module), Function, Defaults), S }
  end;

translate_each({Kind, Line, Args}, S) when is_list(Args), Kind == def; Kind == defmacro; Kind == defp ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", atom_to_list(Kind));

%% Quoting

translate_each({quote, _Line, [[{do,Exprs}]]}, S) ->
  elixir_quote:translate_each(Exprs, S);

translate_each({quote, Line, Args}, S) when is_list(Args) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", "quote");

%% Functions

translate_each({fn, Line, RawArgs}, S) when is_list(RawArgs) ->
  case lists:split(length(RawArgs) - 1, RawArgs) of
    { Args, [[{do,Expr}]] } ->
      { TClause, NS } = elixir_clauses:assigns_blocks(Line, fun translate/2, Args, [Expr], S),
      { { 'fun', Line, {clauses, [TClause]} }, umergec(S, NS) };
    _ ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", "fn")
  end;

%% Try

translate_each({'try', Line, [Clauses]}, RawS) ->
  Do    = proplists:get_value('do',    Clauses, []),
  Catch = proplists:get_value('catch', Clauses, []),
  After = proplists:get_value('after', Clauses, []),

  S = RawS#elixir_scope{noname=true},

  { TDo, SB }    = translate([Do], S),
  { TCatch, SC } = elixir_clauses:try_catch(Line, [{'catch',Catch}], umergec(S, SB)),
  { TAfter, SA } = translate([After], umergec(S, SC)),
  { { 'try', Line, unpack_try(do, TDo), [], TCatch, unpack_try('after', TAfter) }, umergec(RawS, SA) };

translate_each({'try', Line, Args}, S) when is_list(Args) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", "try");

%% Receive

translate_each({'receive', Line, [RawClauses] }, S) ->
  Clauses = orddict:erase(do, RawClauses),
  case orddict:find('after', Clauses) of
    { ok, After } ->
      AClauses = orddict:erase('after', Clauses),
      { TClauses, SC } = elixir_clauses:match(Line, AClauses ++ [{'after',After}], S),
      { FClauses, [TAfter] } = lists:split(length(TClauses) - 1, TClauses),
      { _, _, [FExpr], _, FAfter } = TAfter,
      { { 'receive', Line, FClauses, FExpr, FAfter }, SC };
    error ->
      { TClauses, SC } = elixir_clauses:match(Line, Clauses, S),
      { { 'receive', Line, TClauses }, SC }
  end;

translate_each({'receive', Line, Args}, S) when is_list(Args) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", "receive");

%% Variables & Function calls

translate_each({Name, Line, false}, S) when is_atom(Name) ->
  Match = S#elixir_scope.assign,
  Vars = S#elixir_scope.vars,
  TempVars = S#elixir_scope.temp_vars,
  ClauseVars = S#elixir_scope.clause_vars,

  case Name of
    '_' -> { {var, Line, Name}, S };
    _ ->
      case { Match, dict:is_key(Name, Vars), lists:member(Name, TempVars) } of
        { true, true, true } -> { {var, Line, dict:fetch(Name, Vars) }, S };
        { true, Else, _ } ->
          { NewVar, NS } = case Else or S#elixir_scope.noname of
            true -> elixir_tree_helpers:build_var_name(Line, S);
            false -> { {var, Line, Name}, S }
          end,
          RealName = element(3, NewVar),
          { NewVar, NS#elixir_scope{
            vars=dict:store(Name, RealName, Vars),
            temp_vars=[RealName|TempVars],
            clause_vars=dict:store(Name, RealName, ClauseVars)
          } };
        { false, false, _ } -> translate_each({Name, Line, []}, S);
        { false, true, _ }  -> { {var, Line, dict:fetch(Name, Vars) }, S }
      end
  end;

translate_each({Atom, Line, Args}, S) when is_atom(Atom) ->
  Callback = fun() ->
    elixir_import:record(local, { Atom, length(Args) }, nil, S),
    { TArgs, NS } = translate_args(Args, S),
    { { call, Line, { atom, Line, Atom }, TArgs }, NS }
  end,
  elixir_macro:dispatch_imports(Line, Atom, Args, S, Callback);

%% Dot calls

translate_each({{'.', _, [Left, Right]}, Line, Args}, S) ->
  { TLeft,  SL } = translate_each(Left, S),
  { TRight,  SR } = translate_each(Right, umergec(S, SL)),

  Callback = fun() ->
    { TArgs, SA } = translate_args(Args, umergec(S, SR)),
    { { call, Line, { remote, Line, TLeft, TRight }, TArgs }, umergev(SL, umergev(SR,SA)) }
  end,

  case { TLeft, TRight } of
    { { atom, _, '::Erlang' }, { atom, _, Atom } } ->
      { { atom, Line, Atom }, S };
    { { atom, _, Receiver }, { atom, _, Name } }  ->
      elixir_macro:dispatch_refer(Line, Receiver, Name, Args, umergev(SL, SR), Callback);
    { { Kind, _, _ }, { atom, _, _ } } when Kind == var; Kind == tuple ->
      Callback();
    _ ->
      { TArgs, SA } = translate_args(Args, umergec(S, SR)),
      Apply = [TLeft, TRight, elixir_tree_helpers:build_simple_list(Line, TArgs)],
      { ?ELIXIR_WRAP_CALL(Line, erlang, apply, Apply), umergev(SL, umergev(SR,SA)) }
  end;

%% Anonymous function calls

translate_each({{'.', _, [Expr]}, Line, Args}, S) ->
  { TExpr, SE } = translate_each(Expr, S),
  { TArgs, SA } = translate_args(Args, umergec(S, SE)),
  { {call, Line, TExpr, TArgs}, umergev(SE, SA) };

%% Literals

translate_each({ Left, Right }, S) when is_atom(Left) ->
  { TLeft, SL }  = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, SL),
  { { tuple, 0, [TLeft, TRight] }, SR };

translate_each([], S) ->
  { { nil, 0 }, S };

translate_each(Args, S) when is_list(Args) ->
  [RTail|RArgs] = lists:reverse(Args),

  case RTail of
    {'|',_,[Left,Right]} ->
      Exprs = [Left|RArgs],
      { Tail, ST } = translate_each(Right, S),
      ListS = umergec(S, ST);
    _ ->
      Exprs = [RTail|RArgs],
      Tail = { nil, 0 },
      ST = S,
      ListS = S
  end,

  { TExprs, { SC, SV } } = elixir_tree_helpers:build_reverse_list(fun translate_arg/2, Exprs, 0, { ListS, ListS }, Tail),
  { TExprs, umergev(ST, umergec(SV, SC)) };

translate_each(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate_each(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate_each(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S };

translate_each(Bitstring, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

%% Helpers

% Variables in arguments are not propagated from one
% argument to the other. For instance:
%
%   x = 1
%   foo(x = x + 2, x)
%   x
%
% Should be the same as:
%
%   foo(3, 1)
%   3
%
translate_arg(Arg, { Acc, S }) ->
  { TArg, TAcc } = translate_each(Arg, Acc),
  { TArg, { umergec(S, TAcc), umergev(S, TAcc) } }.

translate_args(Args, S) ->
  { TArgs, { SC, SV } } = lists:mapfoldl(fun translate_arg/2, {S, S}, Args),
  { TArgs, umergec(SV, SC) }.

% Unpack a list of expressions from a block.
% Return an empty list in case it is an empty expression on after.
unpack_try(_, [{ block, _, Exprs }]) -> Exprs;
unpack_try('after', [{ nil, _ }])    -> [];
unpack_try(_, Exprs)                 -> Exprs.

% Receives two scopes and return a new scope based on the second
% with their variables merged.
umergev(S1, S2) ->
  V1 = S1#elixir_scope.vars,
  V2 = S2#elixir_scope.vars,
  C1 = S1#elixir_scope.clause_vars,
  C2 = S2#elixir_scope.clause_vars,
  S2#elixir_scope{
    vars=dict:merge(fun var_merger/3, V1, V2),
    clause_vars=dict:merge(fun var_merger/3, C1, C2)
  }.

% Receives two scopes and return a new scope based on the first
% with the counter values from the first one.
umergec(S1, S2) ->
  S1#elixir_scope{counter=S2#elixir_scope.counter}.

% Merge variables trying to find the most recently created.
var_merger(Var, Var, K2) -> K2;
var_merger(Var, K1, Var) -> K1;
var_merger(_Var, K1, K2) ->
  V1 = list_to_integer(tl(atom_to_list(K1))),
  V2 = list_to_integer(tl(atom_to_list(K2))),
  if V1 > V2 -> K1;
     true -> K2
  end.

convert_op('!==') -> '=/=';
convert_op('===') -> '=:=';
convert_op('!=')  ->  '/=';
convert_op('<=')  ->  '=<';
convert_op('<-')  ->  '!';
convert_op(Else)  ->  Else.