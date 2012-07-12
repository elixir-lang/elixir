%% Main entry point for translations. Are macros that cannot be
%% overriden are defined in this file.
-module(elixir_translator).
-export([translate/2, translate_each/2, translate_args/2, translate_apply/7, raw_forms/3, forms/3]).
-import(elixir_scope, [umergev/2, umergec/2]).
-import(elixir_errors, [syntax_error/3, syntax_error/4, parse_error/4, assert_function_scope/3, assert_module_scope/3]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

raw_forms(String, StartLine, File) ->
  try elixir_tokenizer:tokenize(String, StartLine, File) of
    { ok, Tokens } ->
      case elixir_parser:parse(Tokens) of
        { ok, Forms } -> { ok, Forms };
        { error, { Line, _, [Error, Token] } } -> { error, { Line, Error, Token } }
      end;
    { error, { _, _, _ } } = Else -> Else
  catch
    { interpolation_error, { _, _, _ } = Tuple} -> { error, Tuple }
  end.

forms(String, StartLine, File) ->
  case raw_forms(String, StartLine, File) of
    { ok, Forms } -> Forms;
    { error, { Line, Error, Token } } -> parse_error(Line, File, Error, Token)
  end.

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Those macros are "low-level". They are the basic mechanism
%% that makes the language work and cannot be partially applied
%% nor overwritten.

%% Assignment operator

translate_each({'=', Line, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL } = elixir_clauses:assigns(fun translate_each/2, Left, SR),
  { { match, Line, TLeft, TRight }, SL };

%% Blocks and scope rewriters

translate_each({ '__block__', Line, [] }, S) ->
  { { atom, Line, nil }, S };

translate_each({ '__block__', _Line, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ '__block__', Line, Args }, S) when is_list(Args) ->
  { TArgs, NS } = translate(Args, S),
  { { block, Line, TArgs }, NS };

translate_each({ '__scope__', _Line, [[{file,File}],[{do,Expr}]] }, S) ->
  Old = S#elixir_scope.file,
  { TExpr, TS } = translate_each(Expr, S#elixir_scope{file=File}),
  { TExpr, TS#elixir_scope{file=Old} };

%% Erlang op

translate_each({ '__op__', Line, [Op, Expr] }, S) when is_atom(Op) ->
  { TExpr, NS } = translate_each(Expr, S),
  { { op, Line, convert_op(Op), TExpr }, NS };

translate_each({ '__op__', Line, [Op, Left, Right] }, S) when is_atom(Op) ->
  { [TLeft, TRight], NS }  = translate_args([Left, Right], S),
  { { op, Line, convert_op(Op), TLeft, TRight }, NS };

translate_each({ '__ambiguousop__', Line, [Var, H|T] }, S) ->
  { Name, _, _ } = Var,

  case dict:find(Name, S#elixir_scope.vars) of
    error -> translate_each({ Name, Line, [H|T] }, S);
    _ ->
      case T of
        [] -> translate_each(rellocate_ambiguous_op(H, Var), S);
        _  -> syntax_error(Line, S#elixir_scope.file, "Many arguments given to ~s, but ~s is a variable. Use even spaces to solve ambiguity.", [Name, Name])
      end
  end;

%% Containers

translate_each({ '<<>>', Line, Args }, S) when is_list(Args) ->
  case S#elixir_scope.context of
    assign ->
      elixir_tree_helpers:build_bitstr(fun translate_each/2, Args, Line, S);
    _ ->
      { TArgs, { SC, SV } } = elixir_tree_helpers:build_bitstr(fun translate_arg/2, Args, Line, { S, S }),
      { TArgs, umergec(SV, SC) }
  end;

translate_each({'{}', Line, Args}, S) when is_list(Args) ->
  { TArgs, SE } = translate_args(Args, S),
  { {tuple, Line, TArgs}, SE };

translate_each({'[]', _Line, Args}, S) when is_list(Args) ->
  translate_each(Args, S);

%% Lexical

translate_each({refer, Line, Args}, S) ->
  elixir_errors:deprecation(Line, S#elixir_scope.file, "refer is deprecated, please use alias instead"),
  translate_each({alias, Line, Args}, S);

translate_each({alias, Line, [Ref]}, S) ->
  translate_each({alias, Line, [Ref,[]]}, S);

translate_each({alias, Line, [Ref, KV]}, S) ->
  { TRef, SR } = translate_each(Ref, S),

  case TRef of
    { atom, _, Old } ->
      { New, SF } = case orddict:find(as, KV) of
        { ok, false } ->
          { Old, SR };
        Opt when Opt == { ok, true }; Opt == error ->
          { elixir_aliases:last(Old), SR };
        { ok, Other } ->
          { TOther, SA } = translate_each(Other, SR#elixir_scope{aliases=[]}),
          case TOther of
            { atom, _, Atom } -> { Atom, SA };
            _ -> syntax_error(Line, S#elixir_scope.file,
                   "invalid args for alias, expected an atom or alias as argument")
          end
      end,

      %% Avoid creating aliases if first == last
      %% unecessarily polluting the aliases dict
      case New == Old of
        true  -> { { nil, Line }, SF };
        false ->
          case string:tokens(atom_to_list(New), "-") of
            [_,_] -> [];
            _ -> syntax_error(Line, S#elixir_scope.file,
                   "invalid args for alias, cannot create nested alias ~s", [elixir_errors:inspect(New)])
          end,

          { { nil, Line }, SF#elixir_scope{
            aliases=orddict:store(New, Old, S#elixir_scope.aliases)
          } }
      end;
    _ ->
      syntax_error(Line, S#elixir_scope.file, "invalid args for alias, expected an atom or alias as argument")
  end;

translate_each({require, Line, [Ref]}, S) ->
  translate_each({require, Line, [Ref, []]}, S);

translate_each({require, Line, [Ref, KV]}, S) ->
  { TRef, SR } = translate_each(Ref, S),

  As = case orddict:find(as, KV) of
    { ok, Value } -> Value;
    error -> false
  end,

  case TRef of
    { atom, _, Old } ->
      elixir_aliases:ensure_loaded(Line, Old, SR),

      SF = SR#elixir_scope{
        requires=ordsets:add_element(Old, S#elixir_scope.requires)
      },

      translate_each({ alias, Line, [Ref, [{ as, As }]] }, SF);
    _ ->
      syntax_error(Line, S#elixir_scope.file, "invalid args for require, expected an atom or alias as argument")
  end;

translate_each({import, Line, [Left]}, S) ->
  translate_each({ import, Line, [Left, []]}, S);

translate_each({import, Line, [Left,Opts]}, S) when is_list(Opts) ->
  translate_each({ import, Line, [all, Left, Opts]}, S);

translate_each({import, Line, [Selector, Left]}, S) ->
  translate_each({ import, Line, [Selector, Left, []]}, S);

translate_each({import, Line, [Left, Right, Opts]}, S) ->
  { TSelector, SL } = translate_each(Left, S),
  { TRef, SR } = translate_each(Right, SL),

  Selector = case TSelector of
    { atom, _,  SelectorAtom } -> SelectorAtom;
    _ -> syntax_error(Line, S#elixir_scope.file, "invalid selector for import")
  end,

  Ref = case TRef of
    { atom, _, RefAtom } -> RefAtom;
    _ -> syntax_error(Line, S#elixir_scope.file, "invalid name for import")
  end,

  case is_list(Opts) of
    true -> [];
    _ -> syntax_error(Line, S#elixir_scope.file, "invalid options for import")
  end,

  As = case orddict:find(as, Opts) of
    { ok, Value } -> Value;
    error -> false
  end,

  elixir_aliases:ensure_loaded(Line, Ref, SR),
  SF = elixir_import:import(Line, Ref, Opts, Selector, SR),
  translate_each({ require, Line, [Ref, [{ as, As }]] }, SF);

%% Arg-less macros

translate_each({'__MODULE__', Line, Atom}, S) when is_atom(Atom) ->
  { { atom, Line, S#elixir_scope.module }, S };

translate_each({'__FILE__', _Line, Atom}, S) when is_atom(Atom) ->
  translate_each(S#elixir_scope.file, S);

translate_each({'__MAIN__', Line, Atom}, S) when is_atom(Atom) ->
  { { atom, Line, '__MAIN__' }, S };

translate_each({'__ENV__', Line, Atom}, S) when is_atom(Atom) ->
  { elixir_scope:to_erl_env({ Line, S }), S };

translate_each({'__CALLER__', Line, Atom}, S) when is_atom(Atom) ->
  { { var, Line, '__CALLER__' }, S#elixir_scope{caller=true} };

%% Arg-less deprecated macros

translate_each({'__FUNCTION__', Line, Atom}, S) when is_atom(Atom) ->
  elixir_errors:deprecation(Line, S#elixir_scope.file, "__FUNCTION__ is deprecated, use __ENV__.function instead"),
  case S#elixir_scope.function of
    nil ->
      { { atom, Line, nil }, S };
    { Name, Arity } ->
      { { tuple, Line, [ { atom, Line, Name }, { integer, Line, Arity } ] }, S }
  end;

translate_each({'__LINE__', Line, Atom}, S) when is_atom(Atom) ->
  elixir_errors:deprecation(Line, S#elixir_scope.file, "__LINE__ is deprecated, use __ENV__.line instead"),
  { { integer, Line, Line }, S };

%% Aliases

translate_each({ '__aliases__', Line, [H] }, S) ->
  Atom = list_to_atom("__MAIN__-" ++ atom_to_list(H)),
  { { atom, Line, elixir_aliases:lookup(Atom, S#elixir_scope.aliases) }, S };

translate_each({ '__aliases__', Line, [H|T] }, S) ->
  Aliases = case is_atom(H) of
    true ->
      Atom = list_to_atom("__MAIN__-" ++ atom_to_list(H)),
      [elixir_aliases:lookup(Atom, S#elixir_scope.aliases)|T];
    false ->
      [H|T]
  end,

  { TAliases, SA } = translate_args(Aliases, S),

  case lists:all(fun is_atom_tuple/1, TAliases) of
    true ->
      Atoms = [Atom || { atom, _, Atom } <- TAliases],
      { { atom, Line, elixir_aliases:concat(Atoms) }, SA };
    false ->
      Args = [elixir_tree_helpers:build_simple_list(Line, TAliases)],
      { ?ELIXIR_WRAP_CALL(Line, elixir_aliases, concat, Args), SA }
  end;

%% Quoting

translate_each({quote, Line, [Left, Right]}, S) ->
  translate_each({ quote, Line, [orddict:from_list(Left ++ Right)] }, S);

translate_each({quote, GivenLine, [[{do,Exprs}|T]]}, S) ->
  Marker = case orddict:find(hygiene, T) of
    { ok, false } -> nil;
    _ -> quoted
  end,

  { DefaultLine, WrappedExprs } = case orddict:find(location, T) of
    { ok, keep } ->
      Scoped = { '__scope__', GivenLine, [[{file,S#elixir_scope.file}],[{do,Exprs}]] },
      { keep, Scoped };
    _ ->
      { 0, Exprs}
  end,

  Line = case orddict:find(line, T) of
    { ok, keep } -> keep;
    { ok, Value } when is_integer(Value) -> Value;
    { ok, _ } -> syntax_error(GivenLine, S#elixir_scope.file,
                   "invalid args for quote. expected line to be the atom :keep or an integer");
    _ -> DefaultLine
  end,

  Unquote = case orddict:find(unquote, T) of
    { ok, false } -> false;
    _ -> true
  end,

  elixir_quote:quote(WrappedExprs, #elixir_quote{marker=Marker, line=Line, unquote=Unquote}, S);

translate_each({quote, Line, [_]}, S) ->
  syntax_error(Line, S#elixir_scope.file, "invalid args for quote");

translate_each({in_guard, Line, [[{do,Guard},{else,Else}]]}, S) ->
  elixir_errors:deprecation(Line, S#elixir_scope.file, "in_guard is deprecated, check __CALLER__.in_guard? instead"),
  case S#elixir_scope.context of
    guard -> translate_each(Guard, S);
    _ -> translate_each(Else, S)
  end;

%% Functions

translate_each({Key, Line, []}, S) when Key == fn; Key == loop ->
  syntax_error(Line, S#elixir_scope.file, "invalid args for ~s", [Key]);

translate_each({fn, Line, Args} = Original, S) when is_list(Args) ->
  { Left, Right } = elixir_tree_helpers:split_last(Args),

  case Right of
    [{do,Do}] ->
      translate_block_fn(Line, fn, Left, Do, S, []);
    _ when length(Args) == 2 ->
      case translate_args(Args, S) of
        { [{atom,_,Name}, {integer,_,Arity}], SA } ->
          case elixir_dispatch:import_function(Line, Name, Arity, SA) of
            false -> syntax_error(Line, S#elixir_scope.file, "cannot convert a macro to a function");
            Else  -> Else
          end;
        _ ->
          translate_partial_fn({ fn, Line, [{'__MODULE__', 0, nil}|Args] }, S)
      end;
    _ when length(Args) == 3 ->
      translate_partial_fn(Original, S);
    _ ->
      syntax_error(Line, S#elixir_scope.file, "invalid args for fn")
  end;

%% Loop and recur

translate_each({loop, Line, RawArgs}, RS) when is_list(RawArgs) ->
  elixir_errors:deprecation(Line, RS#elixir_scope.file, "loop is deprecated, please use named functions to recur instead"),

  { Args, KV } = elixir_tree_helpers:split_last(RawArgs),
  { ExVar, S } = elixir_scope:build_ex_var(Line, RS),

  { Function, SE } = case KV of
    [{do,Do}] ->
      FS = S#elixir_scope{recur=element(1,ExVar)},
      translate_block_fn(Line, loop, [], Do, FS, [ExVar]);
    _ ->
      syntax_error(Line, S#elixir_scope.file, "invalid args for loop")
  end,

  ErlVar = { var, Line, element(1, ExVar) },
  { TArgs, SA } = translate_args(Args, umergec(S, SE)),

  { { block, Line, [
    { match, Line, ErlVar, Function },
    { call, Line, ErlVar, [ErlVar|TArgs] }
  ] }, umergev(SE, SA) };

translate_each({recur, Line, Args}, S) when is_list(Args) ->
  case S#elixir_scope.recur of
    nil ->
      syntax_error(Line, S#elixir_scope.file, "cannot invoke recur outside of a loop");
    Recur ->
      ExVar = { Recur, Line, nil },
      Call = { { '.', Line, [ExVar] }, Line, [ExVar|Args] },
      translate_each(Call, S)
  end;

%% Super

translate_each({ super, Line, Args }, #elixir_scope{file=File} = S) ->
  Module = assert_module_scope(Line, super, S),
  Function = assert_function_scope(Line, super, S),
  elixir_def_overridable:ensure_defined(Line, Module, Function, S),

  { _, Arity } = Function,

  { TArgs, TS } = if
    is_atom(Args) ->
      elixir_def_overridable:retrieve_args(Line, Arity, S);
    length(Args) == Arity ->
      translate_args(Args, S);
    true ->
      syntax_error(Line, File, "super must be called with the same number of arguments as the current function")
  end,

  Super = elixir_def_overridable:name(Module, Function),
  { { call, Line, { atom, Line, Super }, TArgs }, TS#elixir_scope{super=true} };

translate_each({ 'super?', Line, [] }, S) ->
  Module = assert_module_scope(Line, 'super?', S),
  Function = assert_function_scope(Line, 'super?', S),
  Bool = elixir_def_overridable:is_defined(Module, Function),
  { { atom, Line, Bool }, S };

%% Comprehensions

translate_each({ Kind, Line, Args }, S) when is_list(Args), (Kind == lc) orelse (Kind == bc) ->
  translate_comprehension(Line, Kind, Args, S);

%% Variables

translate_each({'^', Line, [ { Name, _, Args } ] }, S) ->
  Result = case is_atom(Args) of
    true ->
      case S#elixir_scope.context of
        assign ->
          case dict:find(Name, S#elixir_scope.vars) of
            error -> "unbound variable ^~s";
            { ok, Value } -> { {var, Line, Value}, S }
          end;
        _ -> "cannot access variable ^~s outside of assignment"
      end;
    false -> "cannot use ^ with expression at ^~s, ^ must be used only with variables"
  end,

  case is_list(Result) of
    true ->
      syntax_error(Line, S#elixir_scope.file, Result, [Name]);
    false ->
      Result
  end;

translate_each({Name, Line, quoted}, S) when is_atom(Name) ->
  NewS = S#elixir_scope{vars=S#elixir_scope.quote_vars,noname=true},
  { TVar, VS } = elixir_scope:translate_var(Line, Name, NewS),
  { TVar, VS#elixir_scope{
    quote_vars=VS#elixir_scope.vars,
    noname=S#elixir_scope.noname,
    vars=S#elixir_scope.vars
  } };

translate_each({Name, Line, nil}, S) when is_atom(Name) ->
  elixir_scope:translate_var(Line, Name, S);

%% Local calls

translate_each({Atom, Line, Args} = Original, S) when is_atom(Atom) ->
  case sequential_partials(Args, 1) andalso
       elixir_dispatch:import_function(Line, Atom, length(Args), S) of
    false ->
      case handle_partials(Line, Original, S) of
        error ->
          Callback = fun() -> translate_local(Line, Atom, Args, S) end,
          elixir_dispatch:dispatch_import(Line, Atom, Args, S, Callback);
        Else  -> Else
      end;
    Else -> Else
  end;

%% Dot calls

translate_each({{'.', _, [Left, Right]}, Line, Args} = Original, S) when is_atom(Right) ->
  { TLeft,  SL } = translate_each(Left, S),

  Fun = (element(1, TLeft) == atom) andalso sequential_partials(Args, 1) andalso
    elixir_dispatch:require_function(Line, element(3, TLeft), Right, length(Args), SL),

  case Fun of
    false ->
      case handle_partials(Line, Original, S) of
        error ->
          { TRight, SR } = translate_each(Right, umergec(S, SL)),
          Callback = fun() -> translate_apply(Line, TLeft, TRight, Args, S, SL, SR) end,

          case TLeft of
            { atom, _, 'Elixir.Erlang' } ->
              case Args of
                [] -> { { atom, Line, Right }, S };
                _ ->
                  Message = "invalid args for Erlang.~s expression",
                  syntax_error(Line, S#elixir_scope.file, Message, [Right])
              end;
            { atom, _, Receiver } ->
              elixir_dispatch:dispatch_require(Line, Receiver, Right, Args, umergev(SL, SR), Callback);
            _ ->
              Callback()
          end;
        Else -> Else
      end;
    Else -> Else
  end;

%% Anonymous function calls

translate_each({{'.', _, [Expr]}, Line, Args} = Original, S) ->
  { TExpr, SE } = translate_each(Expr, S),
  case TExpr of
    { atom, _, Atom } ->
      translate_each({ Atom, Line, Args }, S);
    _ ->
      case handle_partials(Line, Original, S) of
        error ->
          { TArgs, SA } = translate_args(Args, umergec(S, SE)),
          { {call, Line, TExpr, TArgs}, umergev(SE, SA) };
        Else -> Else
      end
  end;

%% Literals

translate_each({ Left, Right }, S) ->
  translate_each({ '{}', 0, [Left, Right]}, S);

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

  { FExprs, FS } = case S#elixir_scope.context of
    assign ->
      elixir_tree_helpers:build_reverse_list(fun translate_each/2, Exprs, 0, ListS, Tail);
    _ ->
      { TArgs, { SC, SV } } =
        elixir_tree_helpers:build_reverse_list(fun translate_arg/2, Exprs, 0, { ListS, ListS }, Tail),
      { TArgs, umergec(SV, SC) }
  end,

  { FExprs, umergev(ST, FS) };

translate_each(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate_each(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate_each(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S };

translate_each(Bitstring, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

%% Helpers

translate_block_fn(Line, Key, Left, Right, S, ExtraArgs) ->
  Clauses = case { Left, Right } of
    { [], {'->',_,Pairs} } ->
      Pairs;
    { _, {'->',_,_} } ->
      syntax_error(Line, S#elixir_scope.file, "~s does not accept arguments when passing many clauses", [Key]);
    { Args, Expr } ->
      [{ Args, Expr }]
  end,

  Transformer = fun({ ArgsWithGuards, Expr }, Acc) ->
    { FinalArgs, Guards } = elixir_clauses:extract_last_guards(ArgsWithGuards),
    elixir_clauses:assigns_block(Line, fun elixir_translator:translate/2, ExtraArgs ++ FinalArgs, [Expr], Guards, umergec(S, Acc))
  end,

  { TClauses, NS } = lists:mapfoldl(Transformer, S, Clauses),
  { { 'fun', Line, {clauses, TClauses} }, umergec(S, NS) }.

translate_partial_fn({ fn, Line, Args } = Original, S) ->
  case handle_partials(Line, Original, S) of
    error ->
      { [A,B,C], SA } = translate_args(Args, S),
      { { 'fun', Line, { function, A, B, C } }, SA };
    Else -> Else
  end.

translate_local(Line, Name, Args, #elixir_scope{local=nil} = S) ->
  { TArgs, NS } = translate_args(Args, S),
  { { call, Line, { atom, Line, Name }, TArgs }, NS };

translate_local(Line, Name, Args, S) ->
  { TArgs, NS } = translate_args(Args, S),
  Remote = { remote, Line,
    { atom, Line, S#elixir_scope.local },
    { atom, Line, Name }
  },
  { { call, Line, Remote, TArgs }, NS }.

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
% However, notice that if we are doing an assignment,
% it behaves the same as translate.
translate_arg(Arg, { Acc, S }) ->
  { TArg, TAcc } = translate_each(Arg, Acc),
  { TArg, { umergec(S, TAcc), umergev(S, TAcc) } }.

translate_args(Args, #elixir_scope{context=assign} = S) ->
  translate(Args, S);

translate_args(Args, S) ->
  { TArgs, { SC, SV } } = lists:mapfoldl(fun translate_arg/2, {S, S}, Args),
  { TArgs, umergec(SV, SC) }.

% Translate apply. Used by both apply and
% external function invocation macros.
translate_apply(Line, TLeft, TRight, Args, S, SL, SR) ->
  Optimize = case (Args == []) orelse lists:last(Args) of
    { '|', _, _ } -> false;
    _ ->
      case { TLeft, TRight } of
        { { Kind, _, _ }, { atom, _, _ } } when Kind == var; Kind == tuple; Kind == atom ->
          true;
        _ ->
          false
      end
  end,

  case Optimize of
    true ->
      { TArgs, SA } = translate_args(Args, umergec(S, SR)),
      FS = umergev(SL, umergev(SR,SA)),
      { { call, Line, { remote, Line, TLeft, TRight }, TArgs }, FS };
    false ->
      { TArgs, SA } = translate_each(Args, umergec(S, SR)),
      FS = umergev(SL, umergev(SR,SA)),
      { ?ELIXIR_WRAP_CALL(Line, erlang, apply, [TLeft, TRight, TArgs]), FS }
  end.

%% Handle partials by automatically wrapping them in a function.
%% It also checks if we are in an assignment scope and does not
%% apply the function if this is the case.
handle_partials(_Line, _Original, #elixir_scope{context=assign}) ->
  error;

handle_partials(Line, Original, S) ->
  case convert_partials(Line, element(3, Original), S) of
    { Call, Def, SC } when Def /= [] ->
      Final = validate_partials(Line, Def, SC),
      Block = [{do, setelement(3, Original, Call)}],
      translate_each({ fn, Line, Final ++ [Block] }, SC);
    _ -> error
  end.

validate_partials(Line, Def, S) ->
  validate_partials(Line, lists:sort(Def), 1, S).

validate_partials(Line, [{ Pos, Item }|T], Pos, S) ->
  [Item|validate_partials(Line, T, Pos + 1, S)];

validate_partials(Line, [{ Pos, _ }|_], Expected, S) ->
  syntax_error(Line, S#elixir_scope.file, "partial variable &~w cannot be defined without &~w", [Pos, Expected]);

validate_partials(_Line, [], _Pos, _S) ->
  [].

%% This function receives arguments and then checks
%% the args for partial application. It returns a tuple
%% with three elements where the first element is the list
%% of call args, the second the list of def args for the
%% function definition and the third one is the new scope.
convert_partials(Line, List, S) -> convert_partials(Line, List, S, [], []).

convert_partials(Line, [{'&', _, [Pos]}|T], S, CallAcc, DefAcc) ->
  case lists:keyfind(Pos, 1, DefAcc) of
    false ->
      { Var, SC } = elixir_scope:build_ex_var(Line, S),
      convert_partials(Line, T, SC, [Var|CallAcc], [{Pos,Var}|DefAcc]);
    {Pos,Var} ->
      convert_partials(Line, T, S, [Var|CallAcc], DefAcc)
  end;

convert_partials(Line, [H|T], S, CallAcc, DefAcc) ->
  convert_partials(Line, T, S, [H|CallAcc], DefAcc);

convert_partials(_Line, [], S, CallAcc, DefAcc) ->
  { lists:reverse(CallAcc), lists:reverse(DefAcc), S }.

sequential_partials([{ '&', _, [Int] }|T], Int) ->
  sequential_partials(T, Int + 1);

sequential_partials([], Int) when Int > 1 -> true;
sequential_partials(_, _Int) -> false.

%% Convert operators

convert_op('and')  -> 'andalso';
convert_op('or')   -> 'orelse';
convert_op('and!') -> 'and';
convert_op('or!')  -> 'or';
convert_op('!==')  -> '=/=';
convert_op('===')  -> '=:=';
convert_op('!=')   ->  '/=';
convert_op('<=')   ->  '=<';
convert_op('<-')   ->  '!';
convert_op(Else)   ->  Else.

is_atom_tuple({ atom, _, _ }) -> true;
is_atom_tuple(_) -> false.

rellocate_ambiguous_op({ Op, Line, [Expr] }, Var) when Op == '+'; Op == '-' ->
  { Op, Line, [Var, Expr] };

rellocate_ambiguous_op({ Call, Line, [H|T] }, Var) ->
  { Call, Line, [rellocate_ambiguous_op(H, Var)|T] }.

%% Comprehensions

translate_comprehension(Line, Kind, Args, S) ->
  case elixir_tree_helpers:split_last(Args) of
    { Cases, [{do,Expr}] } ->
      { TCases, SC } = lists:mapfoldl(fun translate_comprehension_clause/2, S, Cases),
      { TExpr, SE }  = translate_comprehension_do(Line, Kind, Expr, SC),
      { { Kind, Line, TExpr, TCases }, umergec(S, SE) };
    _ ->
      syntax_error(Line, S#elixir_scope.file, "keyword argument :do missing for comprehension ~s", [Kind])
  end.

translate_comprehension_do(_Line, bc, { '<<>>', _, _ } = Expr, S) ->
  translate_each(Expr, S);

translate_comprehension_do(Line, bc, _Expr, S) ->
  syntax_error(Line, S#elixir_scope.file, "a bit comprehension expects a bit string << >> to be returned");

translate_comprehension_do(_Line, _Kind, Expr, S) ->
  translate_each(Expr, S).

translate_comprehension_clause({inbits, Line, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { b_generate, Line, TLeft, TRight }, SL };

translate_comprehension_clause({inlist, Line, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { generate, Line, TLeft, TRight }, SL };

translate_comprehension_clause(X, S) ->
  { TX, TS } = translate_each(X, S),
  Line = case X of
    { _, L, _ } -> L;
    _ -> 0
  end,
  { elixir_tree_helpers:convert_to_boolean(Line, TX, true), TS }.