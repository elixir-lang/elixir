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
-export([format_snippet/4, format_snippet/5]).
-export([print_warning/2, print_warning/3]).
-include("elixir.hrl").
-type location() :: non_neg_integer() | {non_neg_integer(), non_neg_integer()}.

%% Diagnostic API

%% TODO: Remove me on Elixir v2.0.
print_warning(Position, File, Message) ->
  Output = format_warning(Position, File, Message, []),
  io:put_chars(standard_error, [Output, $\n]).

%% Used by parallel checker as it groups warnings.
print_warning(Message, [Diagnostic]) ->
  #{file := File, position := Position, stacktrace := S} = Diagnostic,
  Output = format_warning(Position, File, Message, S),
  io:put_chars(standard_error, [Output, $\n]);

% Warning groups
print_warning(Message, [FirstDiagnostic | Rest]) ->
  #{position := Position, file := File, stacktrace := S} = FirstDiagnostic,
  FormattedWarning = format_warning(Position, File, Message, S),
  LineNumber = extract_line(Position),
  LineDigits = get_line_number_digits(LineNumber, 1),

  StacktracePadding = case filelib:is_regular(File) of
                        % Match indentation with the formatted warning
                        % When we have the line available, we add 4 spaces, else 2
                        true -> LineDigits + 4;
                        false -> LineDigits + 2
                      end,

  WarnLocations = [[n_spaces(StacktracePadding), 'Elixir.Exception':format_stacktrace_entry(H), "\n"]
                  || #{stacktrace := [H]} <- Rest],

  LocationsPlural = case length(WarnLocations) of
                      1 -> "location";
                      _ -> "locations"
                    end,

  Output = io_lib:format(
    "~ts\n"
    "~tsInvalid call also found at ~b other ~ts:~n"
    "~ts\n",
    [
      FormattedWarning,
      n_spaces(StacktracePadding - 2), length(WarnLocations), LocationsPlural,
      WarnLocations
    ]
  ),

  io:put_chars(standard_error, Output).

format_warning(Position, File, Message, Stacktrace) ->
  Result = case {Position, filelib:is_regular(File)} of
    {{Line, Col}, true} ->
      format_line_column_diagnostic({Line, Col}, File, Message, Stacktrace, warning);
    {Line, true} ->
      format_line_diagnostic(Line, File, Message, Stacktrace, warning);
    {_, false} ->
      format_nofile_diagnostic(Position, File, Message, Stacktrace, warning)
  end,

  [Result, $\n].

print_diagnostic(#{severity := Severity, message := Message, stacktrace := Stacktrace} = Diagnostic) ->
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
  io:put_chars(standard_error, [prefix(Severity), Message, Location, "\n\n"]),
  Diagnostic.

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

format_line_column_diagnostic(Position, File, Message, Stacktrace, Severity) ->
  {LineNumber, Column} = Position,
  Line = get_file_line(File, LineNumber),
  {FormattedLine, ColumnsTrimmed} = format_line(unicode:characters_to_binary(Line)),
  LineDigits = get_line_number_digits(LineNumber, 1),
  Spacing = n_spaces(LineDigits + 1),
  Location = format_location(Position, File, Stacktrace),

  io_lib:format(
    " ~ts┌─ ~ts~ts\n"
    " ~ts│\n"
    " ~p │ ~ts\n"
    " ~ts│~ts\n"
    " ~ts│\n"
    " ~ts~ts",
    [
     Spacing, prefix(Severity), Location,
     Spacing,
     LineNumber, FormattedLine,
     Spacing, highlight_at_position(Column - ColumnsTrimmed, Severity),
     Spacing,
     Spacing, format_message(Message, LineDigits, 2)
    ]
  ).

format_line_diagnostic(LineNumber, File, Message, Stacktrace, Severity) ->
  Line = get_file_line(File, LineNumber),
  {FormattedLine, _} = format_line(unicode:characters_to_binary(Line)),
  LineDigits = get_line_number_digits(LineNumber, 1),
  Spacing = n_spaces(LineDigits + 1),
  Location = format_location(LineNumber, File, Stacktrace),

  io_lib:format(
    " ~ts┌─ ~ts~ts\n"
    " ~ts│\n"
    " ~p │ ~ts\n"
    " ~ts│ ~ts\n"
    " ~ts│\n"
    " ~ts~ts",
    [
     Spacing, prefix(Severity), Location,
     Spacing,
     LineNumber, FormattedLine,
     Spacing, highlight_below_line(FormattedLine, Severity),
     Spacing,
     Spacing, format_message(Message, LineDigits, 2)
    ]
 ).

% Formatting used when we cannot access `File` to read the line
format_nofile_diagnostic(Position, File, Message, Stacktrace, Severity) ->
  Location = format_location(Position, File, Stacktrace),

  io_lib:format(
    " ┌─ ~ts~ts\n"
    " ~ts",
    [
     prefix(Severity), Location,
     format_message(Message, 0, 1)
    ]
   ).

format_location(Position, File, Stacktrace) ->
  case Stacktrace of
    [] -> file_format(Position, File);
    [E] -> 'Elixir.Exception':format_stacktrace_entry(E)
  end.

extract_line({L, _}) -> L;
extract_line(L) -> L.

get_file_line(File, LineNumber) ->
  {ok, IoDevice} = file:open(File, [read, {encoding, unicode}]),
  LineCollector = fun
                    (I, nil) when I == LineNumber - 1 ->
                      io:get_line(IoDevice, "");
                    (_, nil) ->
                      io:get_line(IoDevice, ""),
                      nil;
                    (_, Line) ->
                      Line
                  end,
  Line = lists:foldl(LineCollector, nil, lists:seq(0, LineNumber)),
  NoNewline = string:replace(Line, "\n", ""),
  ok = file:close(IoDevice),
  NoNewline.

%% Format snippets

format_snippet(File, LineNumber, Column, Description, Snippet) ->
  #{content := Content, offset := Offset} = Snippet,
  LineDigits = get_line_number_digits(LineNumber, 1),
  Spacing = n_spaces(LineDigits + 1),
  {FormattedSnippet, ColumnsTrimmed} = format_line(Content),

  Formatted = io_lib:format(
    " ~ts┌─ ~ts~ts\n"
    " ~ts│\n"
    " ~p │ ~ts\n"
    " ~ts│ ~ts\n"
    " ~ts│\n"
    " ~ts~ts",
    [
     Spacing, prefix(error), file_format({LineNumber, Column}, File),
     Spacing,
     LineNumber, FormattedSnippet,
     Spacing, highlight_at_position(Offset - ColumnsTrimmed, error),
     Spacing,
     Spacing, format_message(Description, LineDigits, 2)
    ]),

  unicode:characters_to_binary(Formatted).

format_snippet(File, LineNumber, Column, Message) ->
   Formatted = format_nofile_diagnostic({LineNumber, Column}, File, Message, [], error),
   % Left pad so we stay aligned with "** (Exception)" banner
   Padded = ["   ", string:replace(Formatted, "\n", "\n   ")],
   unicode:characters_to_binary(Padded).

format_line(Line) ->
  case trim_line(Line, 0) of
    {Trimmed, SpacesMatched} when SpacesMatched >= 27 ->
      ColumnsTrimmed = SpacesMatched - 22,
      {["...", n_spaces(19), Trimmed], ColumnsTrimmed};

    {_, _} ->
      {Line, 0}
  end.

trim_line(<<$\s, Rest/binary>>, Count) -> trim_line(Rest, Count + 1);
trim_line(<<$\t, Rest/binary>>, Count) -> trim_line(Rest, Count + 8);
trim_line(Rest, Count) -> {Rest, Count}.

format_message(Message, NDigits, PaddingSize) ->
  Padding = list_to_binary([$\n, n_spaces(NDigits + PaddingSize)]),
  Bin = unicode:characters_to_binary(Message),
  binary:replace(Bin, <<"\n">>, Padding, [global]).

highlight_at_position(Column, Severity) ->
  case Severity of
    warning ->  highlight([n_spaces(Column), $~], warning);
    error -> highlight([n_spaces(Column), $^], error)
  end.

highlight_below_line(Line, Severity) ->
  % Don't highlight leading whitespaces in line
  {ok, Re} = re:compile("\s*", [unicode]),
  {match, [{_Start, SpacesMatched}]} = re:run(Line, Re, [{capture, all, index}]),

  Length = string:length(Line),
  Highlight = case Severity of
    warning -> highlight(lists:duplicate(Length - SpacesMatched, "~"), warning);
    error -> highlight(lists:duplicate(Length - SpacesMatched, "^"), error)
  end,

  [n_spaces(SpacesMatched), Highlight].

get_line_number_digits(Number, Acc) when Number < 10 -> Acc;
get_line_number_digits(Number, Acc) ->
  get_line_number_digits(Number div 10, Acc + 1).

n_spaces(N) -> lists:duplicate(N, " ").

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
  Snippet = snippet_line(InputString, Location, StartLine, StartColumn),
  raise(Kind, Message, [{file, File}, {snippet, Snippet} | Location]).

snippet_line(InputString, Location, StartLine, StartColumn) ->
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

file_format(_, nil) ->
  "";
file_format({0, _Column}, File) ->
  elixir_utils:relative_to_cwd(File);
file_format({Line, nil}, File) ->
  file_format(Line, File);
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
