-module(elixir_translator).
-export([translate/2, translate_each/2, parse/3]).
-include("elixir.hrl").

parse(String, Line, #elixir_scope{filename=Filename} = S) ->
  Forms = forms(String, Line, Filename),
  { Translated, FS } = translate(Forms, S),
  Final = case FS#elixir_scope.module of
    [] -> Translated;
    _  -> Translated ++ [elixir_module:transform(0, compile, FS)]
  end,
  { Final, FS }.

% TODO test error inside interpolation are properly handled.
forms(String, StartLine, Filename) ->
  case elixir_tokenizer:tokenize(String, StartLine) of
    {ok, Tokens} ->
      try elixir_parser:parse(Tokens) of
        {ok, Forms} -> Forms;
        {error, {Line, _, [Error, Token]}} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
      catch
        { interpolation_error, { Line, Error, Token } } -> elixir_errors:syntax_error(Line, Filename, Error, Token)
      end;
    {error, {Line, Error, Token}} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
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

% TODO: Handle tree errors properly
translate_each({'case', _, Args} = Clause, _S) when is_list(Args) ->
  error({invalid_arguments_for_case, Clause});

%% Blocks

translate_each({ block, Line, [] }, S) ->
  { { atom, Line, nil }, S };

translate_each({ block, _Line, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ block, Line, Args }, S) when is_list(Args) ->
  { TArgs, NS } = translate(Args, S),
  { { block, Line, TArgs }, NS };

% TODO: Add tests that kv_block not handled by macros raises
% a meaningful exception

%% Bit strings

translate_each({ bitstr, Line, Args }, S) when is_list(Args) ->
  { TArgs, { SC, SV } } = elixir_tree_helpers:build_bitstr(fun translate_arg/2, Args, Line, { S, S }),
  { TArgs, umergec(SV, SC) };

%% Containers

translate_each({'{}', Line, Args}, S) when is_list(Args) ->
  { TArgs, SE } = translate_args(Args, S),
  { {tuple, Line, TArgs}, SE };

%% Modules

translate_each({module, Line, [Ref]}, S) ->
  case S#elixir_scope.function of
    [] ->
      { TRef, NS } = translate_each(Ref, S),
      case TRef of
        { atom, _, Module } ->
          FS = NS#elixir_scope{module = Module},
          Expr = case S#elixir_scope.module of
            [] -> elixir_module:transform(Line, build, FS);
            _  ->
              { block, Line, [
                elixir_module:transform(Line, compile, S),
                elixir_module:transform(Line, build, FS)
              ] }
          end,
          { Expr, FS };
        _ ->
          % TODO: Test me
          elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid module name")
      end;
    _ ->
      % TODO: Test me
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for module")
  end;

translate_each({'__MODULE__', Line, []}, S) ->
  case S#elixir_scope.module of
    [] -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no module defined");
    Module  -> {{atom, Line, Module }, S}
  end;

translate_each({endmodule, Line, []}, S) ->
  case S#elixir_scope.module of
    [] -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no module defined");
    _  -> { elixir_module:transform(Line, compile, S), S#elixir_scope{module=[]} }
  end;

%% References

translate_each({ref, Line, [Ref]}, S) when is_atom(Ref) ->
  { {atom, Line, list_to_atom("::" ++ atom_to_list(Ref)) }, S };

translate_each({'::', Line, Args}, S) when is_list(Args) ->
  { TArgs, NS } = translate_args(Args, S),
  Atoms = [Atom || { atom, _, Atom } <- TArgs],
  Final = case length(Atoms) == length(TArgs) of
    true  -> { atom, Line, elixir_module:modulize(Atoms) };
    false ->
      FArgs = [elixir_tree_helpers:build_simple_list(Line, TArgs)],
      ?ELIXIR_WRAP_CALL(Line, elixir_module, modulize, FArgs)
  end,
  { Final, NS };

%% Def

translate_each({Kind, Line, [Call,[{do, Expr}]]}, S) when Kind == def orelse Kind == defmacro ->
  Module = S#elixir_scope.module,
  case (Module == []) or (S#elixir_scope.function /= []) of
    true -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for method");
    _ ->
      { TCall, Guards } = elixir_clauses:extract_guards(Call),
      { Name, Args } = elixir_clauses:extract_args(TCall),
      ClauseScope = S#elixir_scope{function=Name, counter=0, vars=dict:new()},
      { TClause, _ } = elixir_clauses:assigns_blocks(fun translate/2, Args, [Expr], Guards, ClauseScope),

      Arity = length(element(3, TClause)),
      { Unpacked, Defaults } = elixir_def_defaults:unpack(Name, TClause),
      Method = { function, Line, Name, Arity, [Unpacked] },
      { elixir_def:wrap_definition(Kind, Line, S#elixir_scope.filename, Module, Method, Defaults), S }
  end;

% TODO: Handle tree errors properly
translate_each({Kind, Line, Args}, S) when is_list(Args), Kind == def orelse Kind == defmacro ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid args for " ++ atom_to_list(Kind));

%% Quoting

translate_each({quote, _Line, [Expr]}, S) ->
  elixir_quote:translate_each(Expr, S);

% TODO: Handle tree errors properly
translate_each({quote, _, Args} = Clause, _S) when is_list(Args) ->
  error({invalid_arguments_for_quote, Clause});

%% Functions

% TODO: Handle tree errors properly
translate_each({fn, Line, RawArgs}, S) when is_list(RawArgs) ->
  { Args, [[{do,Expr}]] } = lists:split(length(RawArgs) - 1, RawArgs),
  { TClause, NS } = elixir_clauses:assigns_blocks(fun translate/2, Args, [Expr], S),
  { { 'fun', Line, {clauses, [TClause]} }, NS };

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

% TODO: Handle tree errors properly
translate_each({'try', _, Args} = Clause, _S) when is_list(Args) ->
  error({invalid_arguments_for_try, Clause});

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
    { TArgs, NS } = translate_args(Args, S),
    { { call, Line, { atom, Line, Atom }, TArgs }, NS }
  end,
  elixir_macro:dispatch_one('::Elixir::Macros', Atom, Args, S, Callback);

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
      elixir_macro:dispatch_one(Receiver, Name, Args, umergev(SL, SR), Callback);
    { { Kind, _, _ }, { var, _, _ } } when Kind == var; Kind == tuple ->
      Callback();
    _ ->
      { TArgs, SA } = translate_args(Args, umergec(S, SR)),
      Apply = [TLeft, TRight, elixir_tree_helpers:build_simple_list(1, TArgs)],
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