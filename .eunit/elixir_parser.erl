-module(elixir_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/elixir_parser.yrl", 323).

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(exprs(Node), element(3, Node)).

% The following directive is needed for (significantly) faster compilation
% of the generated .erl file by the HiPE compiler. Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

%% Operators

build_op(Op, Left, Right) ->
  { ?op(Op), ?line(Op), [Left, Right] }.

build_expr_op(Op, Left, Right) ->
  { ?exprs(Op), ?line(Op), [Left, Right] }.

build_unary_op(Op, Expr) ->
  { ?op(Op), ?line(Op), [Expr] }.

build_special_op(Op, Expr) ->
  { ?exprs(Op), ?line(Op), [Expr] }.

%% Blocks

% Handle args that expects blocks of code
build_block([])                            -> nil;
build_block([Expr]) when not is_list(Expr) -> Expr;
build_block(Exprs)                         -> { block, 0, lists:reverse(Exprs) }.

% Handle key value blocks
build_kv_block(Delimiter, Contents, IncompleteList) ->
  Line = ?line(Delimiter),
  List = [{do,Contents}|IncompleteList],
  {'[:]', Line, build_kv_block(List, [])}.

% Build key value blocks by joining all clauses together
build_kv_block([{Key,Value}|T], Acc) ->
  New = build_block(Value),
  NewAcc = orddict:update(Key, fun(Old) -> [New|Old] end, [New], Acc),
  build_kv_block(T, NewAcc);

build_kv_block([], Acc) ->
  lists:reverse([build_kv_block(X) || X <- Acc]).

build_kv_block({Key,[Value]}) -> { Key, Value };
build_kv_block({Key,Else})    -> { Key, lists:reverse(Else) }.

%% Args
% Build args by transforming [:] into the final form []
% and properly sorting the items.

build_args(Args) -> lists:map(fun build_arg/1, Args).
build_arg({ '[:]', Line, Args }) -> sort_kv(Args);
build_arg(Else) -> Else.

%% Identifiers
% Those helpers are responsible to:
%
%   + Merge kv args and kv blocks arguments
%   + Handle dot operators and transform them in the proper function call
%   + Handle ambiguitity between p { }, p { 1 }, p { 1, 2 }

build_maybe_curly_identifier(Expr, [{ '{}', Line, []}]) ->
  build_identifier(Expr, [[{do,nil}]]);

build_maybe_curly_identifier(Expr, [{ '{}', Line, [Arg]}]) ->
  build_identifier(Expr, [[{do,Arg}]]);

build_maybe_curly_identifier(Expr, Args) ->
  build_identifier(Expr, Args).

build_identifier(Expr, Args, Block) ->
  build_identifier(Expr, merge_kv(Args, Block)).

build_identifier({ '.', DotLine, [Expr, { Kind, _, Identifier }] }, Args) when
  Kind == identifier; Kind == punctuated_identifier;
  Kind == paren_identifier; Kind == do_identifier ->
  build_identifier({ '.', DotLine, [Expr, Identifier] }, Args);

build_identifier({ '.', Line, _ } = Dot, Args) ->
  FArgs = case Args of
    false -> [];
    _ -> Args
  end,
  { Dot, Line, build_args(FArgs) };

build_identifier({ _, Line, Identifier }, false) ->
  { Identifier, Line, false };

build_identifier({ _, Line, Identifier }, Args) ->
  { Identifier, Line, build_args(Args) }.

%% Interpolation aware

build_string({ string, _Line, [H] }) when is_list(H) -> list_to_binary(H);
build_string({ string, Line, Args }) -> { bitstr, Line, Args }.

build_atom({ atom, _Line, [H] }) when is_atom(H) -> H;
build_atom({ atom, _Line, [H] }) when is_list(H) -> list_to_atom(H);
build_atom({ atom, Line, Args }) -> { binary_to_atom, Line, [{ bitstr, Line, Args}, utf8] }.

%% KV Helpers
% Merge key-value pairs from args and blocks

merge_kv([], Block)   -> [Block];
merge_kv(Args, Block) ->
  { Reverse, Last } = last(Args, []),
  case is_kv(Last) of
    true  ->
      { '[:]', Line, Left } = Last,
      { '[:]', _, Right }  = Block,
      KV = { '[:]', Line, Left ++ Right },
      lists:reverse([KV|Reverse]);
    false ->
      lists:reverse([Block,Last|Reverse])
  end.

last([L], Acc)   -> { Acc, L };
last([H|T], Acc) -> last(T, [H|Acc]).

is_kv({'[:]', _, Args}) -> lists:all(fun is_kv_tuple/1, Args);
is_kv(_) -> false.

is_kv_tuple({ Key, _ }) when is_atom(Key) -> true;
is_kv_tuple(_) -> false.

sort_kv(List) -> lists:sort(fun sort_kv/2, List).
sort_kv({ A, _ }, { B, _ }) -> A =< B.
-file("/usr/local/Cellar/erlang/R15B/lib/erlang/lib/parsetools-2.0.7/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try
        {text, Str} = erl_scan:token_info(Token, text),
        {line, Line} = erl_scan:token_info(Token, line),
        Parts = re:split(Str, "\n"),
        Dline = length(Parts) - 1,
        Yline = Line + Dline,
        case erl_scan:token_info(Token, column) of
            {column, Column} ->
                Col = byte_size(lists:last(Parts)),
                {Yline, Col + if Dline =:= 0 -> Column; true -> 1 end};
            undefined ->
                Yline
        end
    catch _:_ ->
        yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    case catch erl_scan:token_info(Token, text) of
        {text, Txt} -> Txt;
        _ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    case catch erl_scan:token_info(Token, location) of
        {location, Loc} -> Loc;
        _ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/elixir_parser.erl", 317).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, do_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, paren_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, punctuated_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, signed_number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_14(14, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccgoto_base_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_unmatched_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_4(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, do_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, paren_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, punctuated_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, signed_number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_base_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_base_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 259, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 248, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_call_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_matched_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_base_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_14(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_15(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_grammar(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_16(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_expr_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, do_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, paren_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, punctuated_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, signed_number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_call_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_18(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_19(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, do_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, paren_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, punctuated_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, signed_number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_var(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_20(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccgoto_call_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_21: see yeccpars2_18

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_matched_op_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_curly_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_unmatched_op_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_max_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 235, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_unary_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_open_paren(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_unary_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_unary_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_open_bracket(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_base_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_32: see yeccpars2_18

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dot_do_identifier(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_34: see yeccpars2_18

%% yeccpars2_35: see yeccpars2_4

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dot_identifier(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_unary_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_base_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dot_paren_identifier(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dot_punctuated_identifier(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_ref_identifier(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_base_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_43: see yeccpars2_4

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_base_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_45(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_open_curly(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_open_curly(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_49: see yeccpars2_4

%% yeccpars2_50: see yeccpars2_4

%% yeccpars2_51: see yeccpars2_4

%% yeccpars2_52: see yeccpars2_4

%% yeccpars2_53: see yeccpars2_4

%% yeccpars2_54: see yeccpars2_4

%% yeccpars2_55: see yeccpars2_4

%% yeccpars2_56: see yeccpars2_4

%% yeccpars2_57: see yeccpars2_4

%% yeccpars2_58: see yeccpars2_4

%% yeccpars2_59: see yeccpars2_4

%% yeccpars2_60: see yeccpars2_4

%% yeccpars2_61: see yeccpars2_4

yeccpars2_62(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_andand_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mult_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_64(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_multmult_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_add_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_addadd_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_add_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_68(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_addadd_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mult_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_70(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_multmult_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_match_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_and_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_andalso_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_74(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_comp_expr_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_75(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_or_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_76(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_orelse_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_77(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_or_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_78(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pipe_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_oror_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_80_(Stack),
 yeccgoto_oror_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_pipe_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_or_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_83_(Stack),
 yeccgoto_orelse_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_or_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_comp_expr_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_andalso_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_and_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_88_(Stack),
 yeccgoto_match_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_89_(Stack),
 yeccgoto_multmult_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_90_(Stack),
 yeccgoto_mult_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_91_(Stack),
 yeccgoto_addadd_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_add_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_addadd_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_add_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_95_(Stack),
 yeccgoto_multmult_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_mult_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_andand_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_99_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_100(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_102(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_105(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_107(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_108(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_108_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_110(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_112: see yeccpars2_111

%% yeccpars2_113: see yeccpars2_111

%% yeccpars2_114: see yeccpars2_111

%% yeccpars2_115: see yeccpars2_111

%% yeccpars2_116: see yeccpars2_111

%% yeccpars2_117: see yeccpars2_111

yeccpars2_118(S, do_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, paren_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, punctuated_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_119: see yeccpars2_111

%% yeccpars2_120: see yeccpars2_111

%% yeccpars2_121: see yeccpars2_111

%% yeccpars2_122: see yeccpars2_111

%% yeccpars2_123: see yeccpars2_111

%% yeccpars2_124: see yeccpars2_111

yeccpars2_125(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dot_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_dot_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_127_(Stack),
 yeccgoto_dot_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_128: see yeccpars2_111

yeccpars2_129(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_130(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, do_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, paren_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, punctuated_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, signed_number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_130_(Stack),
 yeccgoto_call_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_131: see yeccpars2_18

yeccpars2_132(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, do_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, paren_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, punctuated_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, signed_number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_var(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_133_(Stack),
 yeccgoto_call_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_134: see yeccpars2_18

%% yeccpars2_135: see yeccpars2_18

%% yeccpars2_136: see yeccpars2_111

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_138(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_139(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_139_(Stack),
 yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_140(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_141_(Stack),
 yeccgoto_curly_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_142(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_143_(Stack),
 yeccgoto_curly_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_curly_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_145(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_close_curly(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_147(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_147_(Stack),
 yeccgoto_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_close_curly(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_149: see yeccpars2_4

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 yeccgoto_base_orddict(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_151(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_comma_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_152(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_call_args_comma_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_153(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_call_args_comma_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_call_args_parens(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_156(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_kv_eol(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_kv_eol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_158_(Stack),
 yeccgoto_call_args_parens(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_close_paren(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_160(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_161_(Stack),
 yeccgoto_close_paren(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_162(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_163(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_comma_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_164_(Stack),
 yeccgoto_comma_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_165(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_165_(Stack),
 yeccgoto_comma_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_166_(Stack),
 yeccgoto_call_args_comma_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_167(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_167_(Stack),
 yeccgoto_kv_comma(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_168(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_169_(Stack),
 yeccgoto_kv_comma(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_170(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_170_(Stack),
 yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_(Stack),
 yeccgoto_curly_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 yeccgoto_matched_base_orddict(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_173(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_matched_comma_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_174(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_174_(Stack),
 yeccgoto_call_args_no_parens(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_175_(Stack),
 yeccgoto_call_args_no_parens(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_176: see yeccpars2_111

yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_177_(Stack),
 yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_178(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_178_(Stack),
 yeccgoto_matched_kv_comma(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_179: see yeccpars2_168

yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_180_(Stack),
 yeccgoto_matched_kv_comma(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_181(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_182(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_182_(Stack),
 yeccgoto_matched_comma_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_183_(Stack),
 yeccgoto_call_args_no_parens(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_184(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_185_(Stack),
 yeccgoto_curly_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_186_(Stack),
 yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_187(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_187_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_188(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_188_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_189(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_189_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_190(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_190_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_191(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_191_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_192(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_193_(Stack),
 yeccgoto_dot_do_identifier(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_(Stack),
 yeccgoto_dot_identifier(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_(Stack),
 yeccgoto_dot_paren_identifier(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_196_(Stack),
 yeccgoto_dot_punctuated_identifier(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_197(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_197_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_198(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_198_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_199(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_199_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_200(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_201(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_202(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_203(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_204(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_205(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 206, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_ref_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_ref_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_(Stack),
 yeccgoto_ref_identifier(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_208_(Stack),
 yeccgoto_unary_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_209(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_209_(Stack),
 yeccgoto_grammar(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_210(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, do_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, paren_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, punctuated_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, signed_number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_grammar(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_211_(Stack),
 yeccgoto_dot_identifier(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_212(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_212_(Stack),
 yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_213(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_214_(Stack),
 yeccgoto_block_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_215(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_do_eol(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_216(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_216_(Stack),
 yeccgoto_do_eol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_217(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_218: see yeccpars2_4

yeccpars2_219(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_220(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, do_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, paren_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, punctuated_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, signed_number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_220_(Stack),
 yeccgoto_kv_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_kv_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_222_(Stack),
 yeccgoto_do_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_223(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 226, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 227, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_do_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_225_(Stack),
 yeccgoto_do_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_end_eol(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_227(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, kv_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_228(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_229_(Stack),
 yeccgoto_end_eol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_230_(Stack),
 yeccgoto_do_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_231_(Stack),
 yeccgoto_open_bracket(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_232_(Stack),
 yeccgoto_unary_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_233_(Stack),
 yeccgoto_unary_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_234_(Stack),
 yeccgoto_open_paren(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_unary_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_236(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_236(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_236_(Stack),
 yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_237_(Stack),
 yeccgoto_block_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_238_(Stack),
 yeccgoto_block_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_239(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_239_(Stack),
 yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_block_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_241(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_242_(Stack),
 yeccgoto_block_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_243(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_244_(Stack),
 yeccgoto_block_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_245(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, do_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, paren_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, punctuated_identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, signed_number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_245_(Stack),
 yeccgoto_grammar(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_246(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_247(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '**', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, comp_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_248_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_249: see yeccpars2_162

yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_250_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_close_bracket(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_252(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_253_(Stack),
 yeccgoto_close_bracket(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_254_(Stack),
 yeccgoto_call_args(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_255: see yeccpars2_246

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_256_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_257_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_258(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 261, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_259_(Stack),
 yeccgoto_tuple(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_260_(Stack),
 yeccgoto_tuple(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_261(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_261(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_262(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, eol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_263_(Stack),
 yeccgoto_max_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_264_(Stack),
 yeccgoto_max_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_265(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, ref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, special_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_266(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_266(S, dot_call_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_266_(Stack),
 yeccgoto_matched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_267_(Stack),
 yeccgoto_unmatched_op_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_add_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(61, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_addadd_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addadd_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(60, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_and_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_and_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(59, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_andalso_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andalso_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(58, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_andand_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_andand_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(57, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_base_expr(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_expr(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_base_orddict(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_orddict(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_orddict(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_orddict(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_block_expr(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block_expr(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_call_args(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(258, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(255, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_call_args_comma_expr(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_comma_expr(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(153, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_comma_expr(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_call_args_no_parens(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(243, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_no_parens(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(239, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_no_parens(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_no_parens(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_call_args_parens(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(241, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_parens(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(236, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_parens(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_parens(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_parens(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(184, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_parens(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(170, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_args_parens(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(139, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_call_expr(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_close_bracket(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_close_bracket(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_close_bracket(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_close_curly(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_close_curly(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_close_paren(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_close_paren(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_comma_expr(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(152, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comma_expr(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(152, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comma_expr(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(152, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_comma_separator(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(162, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comma_separator(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(168, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comma_separator(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comma_separator(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comma_separator(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(249, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_comp_expr_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comp_expr_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(56, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_curly_block(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_block(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_block(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_block(212=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_block(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_block(241=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_curly_expr(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_curly_expr(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_do_block(20=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_block(212=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_block(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_block(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_block(241=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_block(243=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_do_eol(20, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_eol(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_eol(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_eol(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_eol(241, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_eol(243, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_dot_call_expr(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(122, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_call_expr(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(21, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_dot_do_identifier(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_do_identifier(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_dot_identifier(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(122, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_identifier(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_dot_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_dot_paren_identifier(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(122, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_paren_identifier(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_dot_punctuated_identifier(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(122, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dot_punctuated_identifier(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_end_eol(223=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(247, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(108, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(106, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(105, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(103, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(101, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(100, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(99, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(98, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(167, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_list(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(262, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(209, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(223, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(219, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_grammar(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_kv_comma(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_comma(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(246, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_comma(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_comma(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_comma(168=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_comma(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_comma(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_kv_eol(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(168, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(179, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_eol(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(149, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_kv_list(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(217, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_list(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_kv_list(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_list(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_match_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(55, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_matched_base_orddict(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_base_orddict(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_base_orddict(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_base_orddict(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_base_orddict(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_matched_comma_expr(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(174, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_comma_expr(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(174, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_comma_expr(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(174, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_comma_expr(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(174, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_matched_expr(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(266, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(203, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(202, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(201, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(200, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(199, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(197, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(191, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(190, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(122, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(187, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(182, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_expr(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_matched_kv_comma(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_kv_comma(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_kv_comma(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_kv_comma(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_kv_comma(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_matched_op_expr(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matched_op_expr(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_max_expr(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_max_expr(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mult_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(54, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_multmult_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_multmult_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(53, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_open_bracket(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(122, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_bracket(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_open_curly(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(122, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(170, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(241, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_curly(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_open_paren(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(122, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_open_paren(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_or_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_or_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(52, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_orelse_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_orelse_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(51, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oror_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oror_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(50, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pipe_op(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pipe_op(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(49, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ref_identifier(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ref_identifier(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ref_op(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(204, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_tuple(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unary_op(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(122, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unary_op(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unmatched_expr(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_expr(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unmatched_op_expr(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unmatched_op_expr(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_var(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("src/elixir_parser.yrl", 63).
yeccpars2_0_(__Stack0) ->
 [begin
   [ nil ]
  end | __Stack0].

-compile({inline,yeccpars2_1_/1}).
-file("src/elixir_parser.yrl", 137).
yeccpars2_1_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , false )
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-file("src/elixir_parser.yrl", 59).
yeccpars2_15_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lists : reverse ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("src/elixir_parser.yrl", 66).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("src/elixir_parser.yrl", 125).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , [ ] )
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("src/elixir_parser.yrl", 126).
yeccpars2_20_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , false )
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-file("src/elixir_parser.yrl", 136).
yeccpars2_31_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_atom ( __1 )
  end | __Stack].

-file("src/elixir_parser.erl", 7264).
-compile({inline,yeccpars2_38_/1}).
-file("src/elixir_parser.yrl", 134).
yeccpars2_38_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ? exprs ( __1 )
  end | __Stack].

-file("src/elixir_parser.erl", 7273).
-compile({inline,yeccpars2_42_/1}).
-file("src/elixir_parser.yrl", 135).
yeccpars2_42_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { element ( 4 , __1 ) , ? line ( __1 ) , ? exprs ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("src/elixir_parser.yrl", 141).
yeccpars2_44_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_string ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("src/elixir_parser.yrl", 161).
yeccpars2_46_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("src/elixir_parser.yrl", 106).
yeccpars2_47_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_special_op ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("src/elixir_parser.yrl", 89).
yeccpars2_48_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_special_op ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-file("src/elixir_parser.yrl", 203).
yeccpars2_80_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-file("src/elixir_parser.yrl", 220).
yeccpars2_81_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-file("src/elixir_parser.yrl", 217).
yeccpars2_82_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-file("src/elixir_parser.yrl", 209).
yeccpars2_83_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_84_/1}).
-file("src/elixir_parser.yrl", 215).
yeccpars2_84_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_85_/1}).
-file("src/elixir_parser.yrl", 223).
yeccpars2_85_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-file("src/elixir_parser.yrl", 206).
yeccpars2_86_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-file("src/elixir_parser.yrl", 212).
yeccpars2_87_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_88_/1}).
-file("src/elixir_parser.yrl", 197).
yeccpars2_88_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-file("src/elixir_parser.yrl", 185).
yeccpars2_89_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_90_/1}).
-file("src/elixir_parser.yrl", 175).
yeccpars2_90_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-file("src/elixir_parser.yrl", 180).
yeccpars2_91_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-file("src/elixir_parser.yrl", 170).
yeccpars2_92_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-file("src/elixir_parser.yrl", 179).
yeccpars2_93_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("src/elixir_parser.yrl", 169).
yeccpars2_94_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-file("src/elixir_parser.yrl", 184).
yeccpars2_95_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-file("src/elixir_parser.yrl", 174).
yeccpars2_96_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("src/elixir_parser.yrl", 200).
yeccpars2_97_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("src/elixir_parser.yrl", 76).
yeccpars2_98_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("src/elixir_parser.yrl", 78).
yeccpars2_99_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-file("src/elixir_parser.yrl", 84).
yeccpars2_100_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("src/elixir_parser.yrl", 82).
yeccpars2_101_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("src/elixir_parser.yrl", 80).
yeccpars2_102_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-file("src/elixir_parser.yrl", 87).
yeccpars2_103_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_expr_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-file("src/elixir_parser.yrl", 75).
yeccpars2_104_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-file("src/elixir_parser.yrl", 77).
yeccpars2_105_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-file("src/elixir_parser.yrl", 79).
yeccpars2_106_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-file("src/elixir_parser.yrl", 85).
yeccpars2_107_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_108_/1}).
-file("src/elixir_parser.yrl", 83).
yeccpars2_108_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-file("src/elixir_parser.yrl", 81).
yeccpars2_109_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_110_/1}).
-file("src/elixir_parser.yrl", 86).
yeccpars2_110_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-file("src/elixir_parser.erl", 7562).
-compile({inline,yeccpars2_126_/1}).
-file("src/elixir_parser.yrl", 251).
yeccpars2_126_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { '.' , ? line ( __2 ) , [ __1 ] }
  end | __Stack].

-compile({inline,yeccpars2_127_/1}).
-file("src/elixir_parser.yrl", 236).
yeccpars2_127_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_129_/1}).
-file("src/elixir_parser.yrl", 93).
yeccpars2_129_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-file("src/elixir_parser.yrl", 125).
yeccpars2_130_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , [ ] )
  end | __Stack].

-compile({inline,yeccpars2_133_/1}).
-file("src/elixir_parser.yrl", 126).
yeccpars2_133_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , false )
  end | __Stack].

-compile({inline,yeccpars2_137_/1}).
-file("src/elixir_parser.yrl", 106).
yeccpars2_137_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_special_op ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-file("src/elixir_parser.yrl", 121).
yeccpars2_139_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_141_/1}).
-file("src/elixir_parser.yrl", 118).
yeccpars2_141_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_143_/1}).
-file("src/elixir_parser.yrl", 304).
yeccpars2_143_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_kv_block ( __1 , [ ] , [ ] )
  end | __Stack].

-compile({inline,yeccpars2_144_/1}).
-file("src/elixir_parser.yrl", 305).
yeccpars2_144_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_kv_block ( __1 , __2 , [ ] )
  end | __Stack].

-compile({inline,yeccpars2_147_/1}).
-file("src/elixir_parser.yrl", 67).
yeccpars2_147_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_148_/1}).
-file("src/elixir_parser.yrl", 163).
yeccpars2_148_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-file("src/elixir_parser.erl", 7659).
-compile({inline,yeccpars2_150_/1}).
-file("src/elixir_parser.yrl", 285).
yeccpars2_150_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { '[:]' , ? line ( hd ( __1 ) ) , __1 }
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-file("src/elixir_parser.yrl", 262).
yeccpars2_151_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-file("src/elixir_parser.yrl", 265).
yeccpars2_152_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lists : reverse ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-file("src/elixir_parser.yrl", 266).
yeccpars2_154_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-file("src/elixir_parser.yrl", 269).
yeccpars2_155_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-file("src/elixir_parser.yrl", 277).
yeccpars2_157_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-file("src/elixir_parser.yrl", 270).
yeccpars2_158_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_161_/1}).
-file("src/elixir_parser.yrl", 153).
yeccpars2_161_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_164_/1}).
-file("src/elixir_parser.yrl", 148).
yeccpars2_164_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-file("src/elixir_parser.yrl", 263).
yeccpars2_165_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_166_/1}).
-file("src/elixir_parser.yrl", 267).
yeccpars2_166_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : reverse ( [ __3 | __1 ] )
  end | __Stack].

-file("src/elixir_parser.erl", 7748).
-compile({inline,yeccpars2_167_/1}).
-file("src/elixir_parser.yrl", 279).
yeccpars2_167_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ { ? exprs ( __1 ) , __2 } ]
  end | __Stack].

-file("src/elixir_parser.erl", 7757).
-compile({inline,yeccpars2_169_/1}).
-file("src/elixir_parser.yrl", 280).
yeccpars2_169_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { ? exprs ( __1 ) , __2 } | __4 ]
  end | __Stack].

-compile({inline,yeccpars2_170_/1}).
-file("src/elixir_parser.yrl", 127).
yeccpars2_170_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_171_/1}).
-file("src/elixir_parser.yrl", 117).
yeccpars2_171_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 , __3 )
  end | __Stack].

-file("src/elixir_parser.erl", 7782).
-compile({inline,yeccpars2_172_/1}).
-file("src/elixir_parser.yrl", 286).
yeccpars2_172_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { '[:]' , ? line ( hd ( __1 ) ) , __1 }
  end | __Stack].

-compile({inline,yeccpars2_173_/1}).
-file("src/elixir_parser.yrl", 255).
yeccpars2_173_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_174_/1}).
-file("src/elixir_parser.yrl", 258).
yeccpars2_174_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lists : reverse ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_175_/1}).
-file("src/elixir_parser.yrl", 259).
yeccpars2_175_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_177_/1}).
-file("src/elixir_parser.yrl", 124).
yeccpars2_177_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_maybe_curly_identifier ( __1 , __2 )
  end | __Stack].

-file("src/elixir_parser.erl", 7823).
-compile({inline,yeccpars2_178_/1}).
-file("src/elixir_parser.yrl", 282).
yeccpars2_178_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ { ? exprs ( __1 ) , __2 } ]
  end | __Stack].

-file("src/elixir_parser.erl", 7832).
-compile({inline,yeccpars2_180_/1}).
-file("src/elixir_parser.yrl", 283).
yeccpars2_180_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { ? exprs ( __1 ) , __2 } | __4 ]
  end | __Stack].

-compile({inline,yeccpars2_182_/1}).
-file("src/elixir_parser.yrl", 256).
yeccpars2_182_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_183_/1}).
-file("src/elixir_parser.yrl", 260).
yeccpars2_183_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : reverse ( [ __3 | __1 ] )
  end | __Stack].

-compile({inline,yeccpars2_184_/1}).
-file("src/elixir_parser.yrl", 122).
yeccpars2_184_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_185_/1}).
-file("src/elixir_parser.yrl", 116).
yeccpars2_185_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_186_/1}).
-file("src/elixir_parser.yrl", 123).
yeccpars2_186_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_maybe_curly_identifier ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_187_/1}).
-file("src/elixir_parser.yrl", 105).
yeccpars2_187_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_unary_op ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_188_/1}).
-file("src/elixir_parser.yrl", 95).
yeccpars2_188_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_189_/1}).
-file("src/elixir_parser.yrl", 101).
yeccpars2_189_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_190_/1}).
-file("src/elixir_parser.yrl", 99).
yeccpars2_190_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_191_/1}).
-file("src/elixir_parser.yrl", 97).
yeccpars2_191_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_192_/1}).
-file("src/elixir_parser.yrl", 104).
yeccpars2_192_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_expr_op ( __2 , __1 , __3 )
  end | __Stack].

-file("src/elixir_parser.erl", 7929).
-compile({inline,yeccpars2_193_/1}).
-file("src/elixir_parser.yrl", 243).
yeccpars2_193_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '.' , ? line ( __2 ) , [ __1 , __3 ] }
  end | __Stack].

-file("src/elixir_parser.erl", 7938).
-compile({inline,yeccpars2_194_/1}).
-file("src/elixir_parser.yrl", 240).
yeccpars2_194_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '.' , ? line ( __2 ) , [ __1 , __3 ] }
  end | __Stack].

-file("src/elixir_parser.erl", 7947).
-compile({inline,yeccpars2_195_/1}).
-file("src/elixir_parser.yrl", 246).
yeccpars2_195_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '.' , ? line ( __2 ) , [ __1 , __3 ] }
  end | __Stack].

-file("src/elixir_parser.erl", 7956).
-compile({inline,yeccpars2_196_/1}).
-file("src/elixir_parser.yrl", 249).
yeccpars2_196_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '.' , ? line ( __2 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_197_/1}).
-file("src/elixir_parser.yrl", 92).
yeccpars2_197_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_198_/1}).
-file("src/elixir_parser.yrl", 94).
yeccpars2_198_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_199_/1}).
-file("src/elixir_parser.yrl", 96).
yeccpars2_199_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_200_/1}).
-file("src/elixir_parser.yrl", 102).
yeccpars2_200_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_201_/1}).
-file("src/elixir_parser.yrl", 100).
yeccpars2_201_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_202_/1}).
-file("src/elixir_parser.yrl", 98).
yeccpars2_202_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_203_/1}).
-file("src/elixir_parser.yrl", 103).
yeccpars2_203_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_op ( __2 , __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_206_/1}).
-file("src/elixir_parser.yrl", 228).
yeccpars2_206_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-file("src/elixir_parser.erl", 8029).
-compile({inline,yeccpars2_207_/1}).
-file("src/elixir_parser.yrl", 231).
yeccpars2_207_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '::' , ? line ( __2 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_208_/1}).
-file("src/elixir_parser.yrl", 194).
yeccpars2_208_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_209_/1}).
-file("src/elixir_parser.yrl", 60).
yeccpars2_209_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   lists : reverse ( __2 )
  end | __Stack].

-compile({inline,yeccpars2_210_/1}).
-file("src/elixir_parser.yrl", 62).
yeccpars2_210_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : reverse ( __2 )
  end | __Stack].

-compile({inline,yeccpars2_211_/1}).
-file("src/elixir_parser.yrl", 239).
yeccpars2_211_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_212_/1}).
-file("src/elixir_parser.yrl", 121).
yeccpars2_212_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_214_/1}).
-file("src/elixir_parser.yrl", 114).
yeccpars2_214_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_216_/1}).
-file("src/elixir_parser.yrl", 291).
yeccpars2_216_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-file("src/elixir_parser.erl", 8094).
-compile({inline,yeccpars2_220_/1}).
-file("src/elixir_parser.yrl", 296).
yeccpars2_220_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { ? exprs ( __1 ) , __2 } ]
  end | __Stack].

-file("src/elixir_parser.erl", 8103).
-compile({inline,yeccpars2_221_/1}).
-file("src/elixir_parser.yrl", 297).
yeccpars2_221_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { ? exprs ( __1 ) , __2 } | __4 ]
  end | __Stack].

-compile({inline,yeccpars2_222_/1}).
-file("src/elixir_parser.yrl", 300).
yeccpars2_222_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_kv_block ( __1 , [ ] , __3 )
  end | __Stack].

-compile({inline,yeccpars2_224_/1}).
-file("src/elixir_parser.yrl", 299).
yeccpars2_224_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_kv_block ( __1 , [ ] , [ ] )
  end | __Stack].

-compile({inline,yeccpars2_225_/1}).
-file("src/elixir_parser.yrl", 301).
yeccpars2_225_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_kv_block ( __1 , __2 , [ ] )
  end | __Stack].

-compile({inline,yeccpars2_229_/1}).
-file("src/elixir_parser.yrl", 294).
yeccpars2_229_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_230_/1}).
-file("src/elixir_parser.yrl", 302).
yeccpars2_230_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_kv_block ( __1 , __2 , __4 )
  end | __Stack].

-compile({inline,yeccpars2_231_/1}).
-file("src/elixir_parser.yrl", 156).
yeccpars2_231_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_232_/1}).
-file("src/elixir_parser.yrl", 190).
yeccpars2_232_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_233_/1}).
-file("src/elixir_parser.yrl", 188).
yeccpars2_233_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_234_/1}).
-file("src/elixir_parser.yrl", 151).
yeccpars2_234_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_235_/1}).
-file("src/elixir_parser.yrl", 192).
yeccpars2_235_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_236_/1}).
-file("src/elixir_parser.yrl", 127).
yeccpars2_236_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_237_/1}).
-file("src/elixir_parser.yrl", 113).
yeccpars2_237_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_238_/1}).
-file("src/elixir_parser.yrl", 112).
yeccpars2_238_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , [ ] , __2 )
  end | __Stack].

-compile({inline,yeccpars2_239_/1}).
-file("src/elixir_parser.yrl", 124).
yeccpars2_239_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_maybe_curly_identifier ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_240_/1}).
-file("src/elixir_parser.yrl", 111).
yeccpars2_240_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-file("src/elixir_parser.yrl", 122).
yeccpars2_241_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_242_/1}).
-file("src/elixir_parser.yrl", 109).
yeccpars2_242_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_243_/1}).
-file("src/elixir_parser.yrl", 123).
yeccpars2_243_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_maybe_curly_identifier ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_244_/1}).
-file("src/elixir_parser.yrl", 110).
yeccpars2_244_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_identifier ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_245_/1}).
-file("src/elixir_parser.yrl", 61).
yeccpars2_245_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   lists : reverse ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_248_/1}).
-file("src/elixir_parser.yrl", 309).
yeccpars2_248_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_250_/1}).
-file("src/elixir_parser.yrl", 311).
yeccpars2_250_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_253_/1}).
-file("src/elixir_parser.yrl", 158).
yeccpars2_253_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_254_/1}).
-file("src/elixir_parser.yrl", 272).
yeccpars2_254_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_args ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_256_/1}).
-file("src/elixir_parser.yrl", 312).
yeccpars2_256_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __4 ]
  end | __Stack].

-compile({inline,yeccpars2_257_/1}).
-file("src/elixir_parser.yrl", 310).
yeccpars2_257_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   sort_kv ( __2 )
  end | __Stack].

-file("src/elixir_parser.erl", 8320).
-compile({inline,yeccpars2_259_/1}).
-file("src/elixir_parser.yrl", 316).
yeccpars2_259_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { '{}' , ? line ( __1 ) , [ ] }
  end | __Stack].

-file("src/elixir_parser.erl", 8329).
-compile({inline,yeccpars2_260_/1}).
-file("src/elixir_parser.yrl", 317).
yeccpars2_260_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '{}' , ? line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_263_/1}).
-file("src/elixir_parser.yrl", 131).
yeccpars2_263_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_block ( [ ] )
  end | __Stack].

-compile({inline,yeccpars2_264_/1}).
-file("src/elixir_parser.yrl", 132).
yeccpars2_264_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_block ( __2 )
  end | __Stack].

-compile({inline,yeccpars2_266_/1}).
-file("src/elixir_parser.yrl", 105).
yeccpars2_266_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_unary_op ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_267_/1}).
-file("src/elixir_parser.yrl", 88).
yeccpars2_267_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_unary_op ( __1 , __2 )
  end | __Stack].


-file("src/elixir_parser.yrl", 452).
