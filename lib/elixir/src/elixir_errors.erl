%% A bunch of helpers to help to deal with errors in Elixir source code.
%% This is not exposed in the Elixir language.
%%
%% Note that this is also called by the Erlang backend, so we also support
%% the line number to be none (as it may happen in some erlang errors).
-module(elixir_errors).
-export([compile_error/1, compile_error/3, parse_error/5]).
-export([function_error/4, module_error/4, file_error/4]).
-export([erl_warn/3, file_warn/4]).
-export([print_diagnostic/1, emit_diagnostic/5]).
-export([fancy_lexer_exception_snippet/5, fancy_lexer_exception/5]).
-export([fancy_warning_group/2]).
-export([print_warning/1, print_warning/3]).
-include("elixir.hrl").
-type location() :: non_neg_integer() | {non_neg_integer(), non_neg_integer()}.

%% Diagnostic API

%% TODO: Remove me on Elixir v2.0.
print_warning(Location, File, Message) ->
  print_warning([Message, file_format(Location, File), $\n]).

%% Used by parallel checker as it groups warnings.
print_warning(Message) ->
  io:put_chars(standard_error, [prefix(warning), Message, $\n]).

print_diagnostic(Diagnostic) ->
  Output = case elixir_config:get(fancy_diagnostics) of 
    true -> fancy_diagnostic_formatter(Diagnostic);
    false -> standard_diagnostic_formatter(Diagnostic)
  end,
  
  io:put_chars(standard_error, Output),
  Diagnostic.

standard_diagnostic_formatter(Diagnostic) -> 
  #{severity := Severity, message := Message, stacktrace := Stacktrace} = Diagnostic,

  Location =
    case (Stacktrace =:= []) orelse elixir_config:is_bootstrap() of
      true ->
        #{position := Position, file := File} = Diagnostic,
        case File of 
          nil -> [];
          File -> ["\n  ", file_format(Position, File)]
        end;

      false ->
        [["\n  ", 'Elixir.Exception':format_stacktrace_entry(E)] || E <- Stacktrace]
    end,
 
  [prefix(Severity), Message, Location, "\n\n"].

fancy_lexer_exception(File, LineNumber, Column, Message, ShowLine) ->
  Result = case filelib:is_regular(File) andalso ShowLine of 
             true -> line_column_diagnostic({LineNumber, Column}, File, Message, error);
             false -> no_line_diagnostic(LineNumber, File, Message, error)
           end,
  unicode:characters_to_binary(Result).

fancy_diagnostic_formatter(Diagnostic) -> 
  #{position := Position, file := File, message := Message, severity := Severity} = Diagnostic,

  Result = case {Position, filelib:is_regular(File)} of 
    {_, false} -> no_line_diagnostic(Position, File, Message, Severity);
    {{_, _}, true} -> line_column_diagnostic(Position, File, Message, Severity);
    {Line, true} -> line_only_diagnostic(Line, File, Message, Severity)
  end,

  [Result, "\n\n"].

fancy_lexer_exception_snippet(File, LineNumber, Column, Description, Snippet) ->
  #{content := Content, offset := Offset} = Snippet,

  LineDigits = get_line_number_digits(LineNumber),
  Spacing = n_spaces(LineDigits + 1),

  Formatted = io_lib:format(
    " ~ts┌─ ~ts~ts\n"
    " ~ts│\n"
    " ~p │ ~ts\n"
    " ~ts│ ~ts\n"
    " ~ts│\n"
    " ~ts│ ~ts\n"
    " ~ts│",
    [
     Spacing, prefix(error), file_format({LineNumber, Column}, File),
     Spacing,
     LineNumber, Content,
     Spacing, highlight_below_line(Content, Offset, error),
     Spacing,
     Spacing, format_message(Description, LineDigits, true), 
     Spacing
    ]),

  unicode:characters_to_binary(Formatted).

fancy_warning_group(Message, [FirstDiagnostic | Rest]) -> 
  #{file := File, position := Position} = FirstDiagnostic,

  LineNumber = extract_line(Position),
  LineDigits = get_line_number_digits(LineNumber),
  Spacing = n_spaces(LineDigits + 1),

  Stack = [["  ", 'Elixir.Exception':format_stacktrace_entry(H), "\n"] 
           || #{stacktrace := [H]} <- Rest],

  Output = case filelib:is_regular(File) of 
    true -> 
      Line = get_file_line(File, LineNumber),
      {TrimmedLine, _} = trim_file_line(Line),

      io_lib:format(
        " ~ts┌─ ~ts~ts\n"
        " ~ts│\n"
        " ~p │ ~ts\n"
        " ~ts│ ~ts\n"
        " ~ts│\n"
        " ~ts│ ~ts\n"
        " ~ts│\n"
        " ~ts│ Invalid call also found at ~b other locations:~n"
        " ~ts│ ~ts\n\n",
        [
         Spacing, prefix(warning), file_format(Position, File),
         Spacing,
         LineNumber, TrimmedLine,
         Spacing, highlight_below_line(TrimmedLine, 0, warning),
         Spacing,
         Spacing, Message, 
         Spacing,
         Spacing, length(Stack),
         Spacing, fancify_newlines(list_to_binary(Stack), LineDigits)
        ]
       );
    false ->
      io_lib:format(
        " ~ts┌─ ~ts~ts\n"
        " ~ts│\n"
        " ~ts│ ~ts\n"
        " ~ts│\n"
        " ~ts│ Invalid call also found at ~b other locations:~n"
        " ~ts│ ~ts\n\n",
        [
         Spacing, prefix(warning), file_format(Position, File),
         Spacing,
         Spacing, Message, 
         Spacing,
         Spacing, length(Stack),
         Spacing, fancify_newlines(list_to_binary(Stack), LineDigits)
        ]
      )
  end,
  io:put_chars(standard_error, Output).

fancify_newlines(Message, NDigits) -> 
  LineSeparator = unicode:characters_to_binary(["\n ", n_spaces(NDigits), " │ "]),
  binary:replace(Message, [<<"\n">>], LineSeparator, [global]).

extract_line({L, _}) -> L;
extract_line(L) -> L.

no_line_diagnostic(Position, File, Message, Severity) -> 
  LineNumber = extract_line(Position),
  LineDigits = get_line_number_digits(LineNumber),
  Spacing = n_spaces(LineDigits + 1),
  io_lib:format(
    " ~ts┌─ ~ts~ts\n"
    " ~ts│\n"
    " ~p │ ~ts\n"
    " ~ts│",
    [
     Spacing, prefix(Severity), file_format(Position, File),
     Spacing,
     LineNumber, format_message(Message, LineDigits, false), 
     Spacing
    ]
   ).

line_column_diagnostic(Position, File, Message, Severity) ->
  {LineNumber, Column} = Position,
  Line = get_file_line(File, LineNumber),
  {TrimmedLine, TotalLeading} = trim_file_line(Line),
  LineDigits = get_line_number_digits(LineNumber),
  Spacing = n_spaces(LineDigits + 1),

  io_lib:format(
    " ~ts┌─ ~ts~ts\n"
    " ~ts│\n"
    " ~p │ ~ts\n"
    " ~ts│ ~ts\n"
    " ~ts│\n"
    " ~ts│ ~ts\n"
    " ~ts│",
    [
     Spacing, prefix(Severity), file_format(Position, File),
     Spacing,
     LineNumber, TrimmedLine, 
     Spacing, highlight_below_line(TrimmedLine, Column - TotalLeading, Severity),
     Spacing,
     Spacing, format_message(Message, LineDigits, false),
     Spacing
    ]
  ).

line_only_diagnostic(LineNumber, File, Message, Severity) ->
  Line = get_file_line(File, LineNumber),
  {TrimmedLine, _} = trim_file_line(Line),
  LineDigits = get_line_number_digits(LineNumber),
  Spacing = n_spaces(LineDigits + 1),

  io_lib:format(
    " ~ts┌─ ~ts~ts\n"
    " ~ts│\n"
    " ~p │ ~ts\n"
    " ~ts│ ~ts\n"
    " ~ts│\n"
    " ~ts│ ~ts\n"
    " ~ts│",
    [ 
     Spacing, prefix(Severity), file_format(LineNumber, File),
     Spacing, 
     LineNumber, TrimmedLine,
     Spacing, highlight_below_line(TrimmedLine, Severity),
     Spacing,
     Spacing, format_message(Message, LineDigits, false),
     Spacing
    ]
 ).

highlight_below_line(Line, Column, Severity) ->
  ErrorLength = match_line_error(Line, Column),
  case Severity of
    warning ->  highlight([n_spaces(Column), lists:duplicate(ErrorLength, "~")], warning);
    error -> highlight([n_spaces(Column), lists:duplicate(ErrorLength, "^")], error)
  end.

highlight_below_line(Line, Severity) ->
  Length = string:length(Line),
  case Severity of 
    warning -> highlight(lists:duplicate(Length, "~"), warning);
    error -> highlight(lists:duplicate(Length, "^"), error)
  end.

get_line_number_digits(Number) -> do_get_line_number_digits(Number, 1).

do_get_line_number_digits(Number, Acc) when Number < 10 -> Acc;
do_get_line_number_digits(Number, Acc) -> 
  do_get_line_number_digits(Number div 10, Acc + 1).

match_line_error(Line, Column) ->
  TermRegex = "[A-Za-z_\.\{\}\(\)&@]*",
  {ok, Re} = re:compile(TermRegex),
  Tail = string:slice(Line, Column),
 {match, [MatchingTerm]} = re:run(Tail, Re, [{capture, all, binary}]),

  case MatchingTerm of 
    % If we cannot consume anything, return a single ^ at column position
    <<>> -> 1;
    _ -> string:length(MatchingTerm)
  end.

get_file_line(File, LineNumber) -> 
  {ok, IoDevice} = file:open(File, [read]),

  LineCollector = fun 
                    (I, nil) when I == LineNumber - 1 -> 
                      {ok, Line} = file:read_line(IoDevice),
                      string:trim(Line, trailing);
                    (_, nil) -> 
                      {ok, _} = file:read_line(IoDevice),
                      nil;
                    (_, Line) -> 
                      Line
                  end,

  lists:foldl(LineCollector, nil, lists:seq(0, LineNumber)).

trim_file_line(Line) -> 
  Trimmed = string:trim(Line, leading),
  {Trimmed, string:length(Line) - string:length(Trimmed) + 1}.

emit_diagnostic(Severity, Position, File, Message, Stacktrace) ->
  Diagnostic = #{
    severity => Severity,
    file => File,
    position => Position,
    message => unicode:characters_to_binary(Message),
    stacktrace => Stacktrace
  },

  case get(elixir_code_diagnostics) of
    undefined -> print_diagnostic(Diagnostic);
    {Tail, true} -> put(elixir_code_diagnostics, {[print_diagnostic(Diagnostic) | Tail], true});
    {Tail, false} -> put(elixir_code_diagnostics, {[Diagnostic | Tail], false})
  end,

  case get(elixir_compiler_info) of
    undefined -> ok;
    {CompilerPid, _} -> CompilerPid ! {diagnostic, Diagnostic}
  end,

  ok.

%% Compilation error/warn handling.

%% Low-level warning, should be used only from Erlang passes.
-spec erl_warn(location() | none, unicode:chardata(), unicode:chardata()) -> ok.
erl_warn(none, File, Warning) ->
  erl_warn(0, File, Warning);
erl_warn(Location, File, Warning) when is_binary(File) ->
  emit_diagnostic(warning, Location, File, Warning, []).

-spec file_warn(list(), binary() | #{file := binary(), _ => _}, module(), any()) -> ok.
file_warn(Meta, File, Module, Desc) when is_list(Meta), is_binary(File) ->
  file_warn(Meta, #{file => File}, Module, Desc);
file_warn(Meta, E, Module, Desc) when is_list(Meta) ->
  % Skip warnings during bootstrap, they will be reported during recompilation
  case elixir_config:is_bootstrap() of
    true -> ok;
    false ->
      {EnvPosition, EnvFile, EnvStacktrace} = env_format(Meta, E),
      Message = Module:format_error(Desc),
      emit_diagnostic(warning, EnvPosition, EnvFile, Message, EnvStacktrace)
  end.

-spec file_error(list(), binary() | #{file := binary(), _ => _}, module(), any()) -> no_return().
file_error(Meta, File, Module, Desc) when is_list(Meta), is_binary(File) ->
  file_error(Meta, #{file => File}, Module, Desc);
file_error(Meta, Env, Module, Desc) when is_list(Meta) ->
  print_error(Meta, Env, Module, Desc),
  compile_error(Env).

%% A module error is one where it can continue if there is a module
%% being compiled. If there is no module, it is a regular file_error.
-spec module_error(list(), #{file := binary(), module => module() | nil, _ => _}, module(), any()) -> ok.
module_error(Meta, #{module := EnvModule} = Env, Module, Desc) when EnvModule /= nil ->
  print_error(Meta, Env, Module, Desc),
  case elixir_module:taint(EnvModule) of
    true -> ok;
    false -> compile_error(Env)
  end;
module_error(Meta, Env, Module, Desc) ->
  file_error(Meta, Env, Module, Desc).

%% A function error is one where it can continue if there is a function
%% being compiled. If there is no function, it is falls back to file_error.
-spec function_error(list(), #{file := binary(), function => {term(), term()} | nil, _ => _}, module(), any()) -> ok.
function_error(Meta, #{function := {_, _}} = Env, Module, Desc) ->
  module_error(Meta, Env, Module, Desc);
function_error(Meta, Env, Module, Desc) ->
  file_error(Meta, Env, Module, Desc).

print_error(Meta, Env, Module, Desc) ->
  {EnvPosition, EnvFile, EnvStacktrace} = env_format(Meta, Env),
  Message = Module:format_error(Desc),
  emit_diagnostic(error, EnvPosition, EnvFile, Message, EnvStacktrace),
  ok.

%% Compilation error.

-spec compile_error(#{file := binary(), _ => _}) -> no_return().
compile_error(#{module := Module, file := File}) when Module /= nil ->
  Inspected = elixir_aliases:inspect(Module),
  Message = io_lib:format("cannot compile module ~ts (errors have been logged)", [Inspected]),
  compile_error([], File, Message);
compile_error(#{file := File}) ->
  compile_error([], File, "cannot compile file (errors have been logged)").

-spec compile_error(list(), binary(), binary() | unicode:charlist()) -> no_return().
compile_error(Meta, File, Message) when is_binary(Message) ->
  {File, Position} = meta_location(Meta, File),
  raise('Elixir.CompileError', Message, [{file, File} | Position]);
compile_error(Meta, File, Message) when is_list(Message) ->
  {File, Position} = meta_location(Meta, File),
  raise('Elixir.CompileError', elixir_utils:characters_to_binary(Message), [{file, File} | Position]).

%% Tokenization parsing/errors.

-spec parse_error(elixir:keyword(), binary() | {binary(), binary()},
                  binary(), binary(), {unicode:charlist(), integer(), integer()}) -> no_return().
parse_error(Location, File, Error, <<>>, Input) ->
  Message = case Error of
    <<"syntax error before: ">> -> <<"syntax error: expression is incomplete">>;
    _ -> <<Error/binary>>
  end,
  raise_snippet(Location, File, Input, 'Elixir.TokenMissingError', Message);

%% Show a nicer message for end of line
parse_error(Location, File, <<"syntax error before: ">>, <<"eol">>, Input) ->
  raise_snippet(Location, File, Input, 'Elixir.SyntaxError',
        <<"unexpectedly reached end of line. The current expression is invalid or incomplete">>);

%% Show a nicer message for keywords pt1 (Erlang keywords show up wrapped in single quotes)
parse_error(Location, File, <<"syntax error before: ">>, Keyword, Input)
    when Keyword == <<"'not'">>;
         Keyword == <<"'and'">>;
         Keyword == <<"'or'">>;
         Keyword == <<"'when'">>;
         Keyword == <<"'after'">>;
         Keyword == <<"'catch'">>;
         Keyword == <<"'end'">> ->
  raise_reserved(Location, File, Input, binary_part(Keyword, 1, byte_size(Keyword) - 2));

%% Show a nicer message for keywords pt2 (Elixir keywords show up as is)
parse_error(Location, File, <<"syntax error before: ">>, Keyword, Input)
    when Keyword == <<"fn">>;
         Keyword == <<"else">>;
         Keyword == <<"rescue">>;
         Keyword == <<"true">>;
         Keyword == <<"false">>;
         Keyword == <<"nil">>;
         Keyword == <<"in">> ->
  raise_reserved(Location, File, Input, Keyword);

%% Produce a human-readable message for errors before a sigil
parse_error(Location, File, <<"syntax error before: ">>, <<"{sigil,", _Rest/binary>> = Full, Input) ->
  {sigil, _, Sigil, [Content | _], _, _, _} = parse_erl_term(Full),
  Content2 = case is_binary(Content) of
    true -> Content;
    false -> <<>>
  end,
  SigilName = list_to_binary(Sigil),
  Message = <<"syntax error before: sigil \~", SigilName/binary, " starting with content '", Content2/binary, "'">>,
  raise_snippet(Location, File, Input, 'Elixir.SyntaxError', Message);

%% Binaries (and interpolation) are wrapped in [<<...>>]
parse_error(Location, File, Error, <<"[", _/binary>> = Full, Input) when is_binary(Error) ->
  Term = case parse_erl_term(Full) of
    [H | _] when is_binary(H) -> <<$", H/binary, $">>;
    _ -> <<$">>
  end,
  raise_snippet(Location, File, Input, 'Elixir.SyntaxError', <<Error/binary, Term/binary>>);

%% Given a string prefix and suffix to insert the token inside the error message rather than append it
parse_error(Location, File, {ErrorPrefix, ErrorSuffix}, Token, Input) when is_binary(ErrorPrefix), is_binary(ErrorSuffix), is_binary(Token) ->
  Message = <<ErrorPrefix/binary, Token/binary, ErrorSuffix/binary >>,
  raise_snippet(Location, File, Input, 'Elixir.SyntaxError', Message);

%% Misplaced char tokens (for example, {char, _, 97}) are translated by Erlang into
%% the char literal (i.e., the token in the previous example becomes $a),
%% because {char, _, _} is a valid Erlang token for an Erlang char literal. We
%% want to represent that token as ?a in the error, according to the Elixir
%% syntax.
parse_error(Location, File, <<"syntax error before: ">>, <<$$, Char/binary>>, Input) ->
  Message = <<"syntax error before: ?", Char/binary>>,
  raise_snippet(Location, File, Input, 'Elixir.SyntaxError', Message);

%% Everything else is fine as is
parse_error(Location, File, Error, Token, Input) when is_binary(Error), is_binary(Token) ->
  Message = <<Error/binary, Token/binary>>,
  raise_snippet(Location, File, Input, 'Elixir.SyntaxError', Message).

parse_erl_term(Term) ->
  {ok, Tokens, _} = erl_scan:string(binary_to_list(Term)),
  {ok, Parsed} = erl_parse:parse_term(Tokens ++ [{dot, 1}]),
  Parsed.

raise_reserved(Location, File, Input, Keyword) ->
  raise_snippet(Location, File, Input, 'Elixir.SyntaxError',
        <<"syntax error before: ", Keyword/binary, ". \"", Keyword/binary, "\" is a "
          "reserved word in Elixir and therefore its usage is limited. For instance, "
          "it can't be used as a variable or be defined nor invoked as a regular function">>).

raise_snippet(Location, File, Input, Kind, Message) when is_binary(File) ->
  {InputString, StartLine, StartColumn} = Input,
  Snippet = snippet(InputString, Location, StartLine, StartColumn),
  raise(Kind, Message, [{file, File}, {snippet, Snippet} | Location]).

snippet(InputString, Location, StartLine, StartColumn) ->
  {line, Line} = lists:keyfind(line, 1, Location),
  case lists:keyfind(column, 1, Location) of
    {column, Column} ->
      Lines = string:split(InputString, "\n", all),
      Snippet = (lists:nth(Line - StartLine + 1, Lines)),
      Offset = if Line == StartLine -> Column - StartColumn; true -> Column - 1 end,
      case string:trim(Snippet, leading) of
        [] -> nil;
        _ -> #{content => elixir_utils:characters_to_binary(Snippet), offset => Offset}
      end;

    false ->
      nil
  end.

%% Helpers

prefix(warning) -> highlight(<<"warning: ">>, warning);
prefix(error) -> highlight(<<"error: ">>, error).

highlight(Message, Severity) ->
  case {Severity, application:get_env(elixir, ansi_enabled, false)} of
    {warning, true} -> yellow(Message);
    {error, true} -> red(Message);
    _ -> Message
  end.

yellow(Msg) -> io_lib:format("\e[33m~ts\e[0m", [Msg]).
red(Msg) -> io_lib:format("\e[31m~ts\e[0m", [Msg]).

env_format(Meta, #{file := EnvFile} = E) ->
  {File, Position} = meta_location(Meta, EnvFile),
  Line = ?line(Position),

  Stacktrace =
    case E of
      #{function := {Name, Arity}, module := Module} ->
        [{Module, Name, Arity, [{file, elixir_utils:relative_to_cwd(File)} | Position ]}];
      #{module := Module} when Module /= nil ->
        [{Module, '__MODULE__', 0, [{file, elixir_utils:relative_to_cwd(File)} | Position]}];
      #{} ->
        []
    end,

  case lists:keyfind(column, 1, Position) of
    {column, Column} -> {{Line, Column}, File, Stacktrace};
    _ -> {Line, File, Stacktrace}
  end.

format_message(Message, NDigits, ReplaceNewlines) ->
  Lines = wrap_message(Message, 80),
  LineSeparator = unicode:characters_to_binary(["\n ", n_spaces(NDigits), " │ "]),
  Joined = join_binary(Lines, LineSeparator),
  case ReplaceNewlines of
    true -> binary:replace(Joined, [<<"\n">>], LineSeparator, [global]);
    false -> Joined
  end.

wrap_message(Message, LineLength) ->
  Words = binary:split(Message, <<" ">>, [global]),
  wrap_lines(Words, LineLength).

wrap_lines(Words, Limit) -> do_wrap_lines(Words, Limit, 0, [], []).

do_wrap_lines([], _, Count, CurrentLine, Lines) when Count > 0 -> 
  lists:reverse([join_words(lists:reverse(CurrentLine)) | Lines]);
do_wrap_lines([], _, _, _, Lines) -> lists:reverse(Lines);
do_wrap_lines([Word | Rest], Limit, Count, CurrentLine, Lines) -> 
  case string:length(Word) of 
    Length when Length + Count > Limit -> 
      Line = [Word | CurrentLine],
      do_wrap_lines(Rest, Limit, Length + 1, [], [join_words(lists:reverse(Line)) | Lines]);
    Length -> 
      do_wrap_lines(Rest, Limit, Count + Length + 1, [Word | CurrentLine], Lines)
  end.

join_words(Lines) -> join_binary(Lines, <<" ">>).

join_binary([], _) ->
    <<>>;
join_binary([H | T], Separator) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary, Separator/binary, Value/binary>> end, H, T).

n_spaces(N) -> lists:duplicate(N, " ").

file_format(_, nil) ->
  "";
file_format({0, _Column}, File) ->
  elixir_utils:relative_to_cwd(File);
file_format({Line, Column}, File) ->
  io_lib:format("~ts:~w:~w", [elixir_utils:relative_to_cwd(File), Line, Column]);
file_format(0, File) ->
  elixir_utils:relative_to_cwd(File);
file_format(Line, File) ->
  io_lib:format("~ts:~w", [elixir_utils:relative_to_cwd(File), Line]).

meta_location(Meta, File) ->
  case elixir_utils:meta_keep(Meta) of
    {F, L} -> {F, [{line, L}]};
    nil    -> {File, maybe_add_col([{line, ?line(Meta)}], Meta)}
  end.

maybe_add_col(Position, Meta) ->
  case lists:keyfind(column, 1, Meta) of 
    {column, Col} when is_integer(Col) -> [{column, Col} | Position];
    false -> Position
  end.

raise(Kind, Message, Opts) when is_binary(Message) ->
  Stacktrace = try throw(ok) catch _:_:Stack -> Stack end,
  Exception = Kind:exception([{description, Message} | Opts]),
  erlang:raise(error, Exception, tl(Stacktrace)).
