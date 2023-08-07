%% A bunch of helpers to help to deal with errors in Elixir source code.
%% This is not exposed in the Elixir language.
%%
%% Note that this is also called by the Erlang backend, so we also support
%% the line number to be none (as it may happen in some erlang errors).
-module(elixir_errors).
-export([compile_error/1, compile_error/3, parse_error/5]).
-export([function_error/4, module_error/4, file_error/4]).
-export([format_snippet/7]).
-export([erl_warn/3, file_warn/4]).
-export([print_diagnostic/2, emit_diagnostic/6]).
-export([print_warning/2, print_warning/3]).
-export([print_warning_group/2]).
-include("elixir.hrl").
-type location() :: non_neg_integer() | {non_neg_integer(), non_neg_integer()}.

%% Diagnostic API

%% TODO: Remove me on Elixir v2.0.
%% Called by deprecated Kernel.ParallelCompiler.print_warning.
print_warning(Position, File, Message) ->
  Output = format_snippet(Position, File, Message, nil, warning, [], nil),
  io:put_chars(standard_error, [Output, $\n, $\n]).

%% Called by Module.ParallelChecker.
print_warning(Message, Diagnostic) ->
  #{file := File, position := Position, stacktrace := S} = Diagnostic,
  Snippet = get_snippet(File, Position),
  Length = get_length(Diagnostic),
  Output = format_snippet(Position, File, Message, Snippet, warning, S, Length),
  io:put_chars(standard_error, [Output, $\n, $\n]).

%% Called by Module.ParallelChecker.
print_warning_group(Message, [Diagnostic | Others]) ->
  #{file := File, position := Position, stacktrace := S} = Diagnostic,
  Snippet = get_snippet(File, Position),
  Length = get_length(Diagnostic),
  Formatted = format_snippet(Position, File, Message, Snippet, warning, S, Length),
  LineNumber = extract_line(Position),
  LineDigits = get_line_number_digits(LineNumber, 1),
  Padding = case Snippet of
    nil -> 0;
    _ -> max(4, LineDigits + 2)
  end,
  Locations = [["\n", n_spaces(Padding), "└─ ", 'Elixir.Exception':format_stacktrace_entry(ES)] || #{stacktrace := [ES]} <- Others],
  io:put_chars(standard_error, [Formatted, Locations, $\n, $\n]).

get_length(#{length := nil}) -> 1;
get_length(#{length := Length}) -> Length;
get_length(_) -> 1.

get_snippet(nil, _Position) ->
  nil;
get_snippet(File, Position) ->
  Line = extract_line(Position),
  case filelib:is_regular(File) of
    true -> get_file_line(File, Line);
    false -> nil
  end.

print_diagnostic(#{severity := Severity, message := M, stacktrace := Stacktrace, position := P, file := F} = Diagnostic, ReadSnippet) ->
  Snippet =
    case ReadSnippet of
      true -> get_snippet(F, P);
      false -> nil
    end,

  Length = get_length(Diagnostic),
  Output = format_snippet(P, F, M, Snippet, Severity, Stacktrace, Length),

  MaybeStack =
    case (F /= nil) orelse elixir_config:is_bootstrap() of
      true -> [];
      false -> [["\n  ", 'Elixir.Exception':format_stacktrace_entry(E)] || E <- Stacktrace]
    end,

  io:put_chars(standard_error, [Output, MaybeStack, $\n, $\n]),
  Diagnostic.

emit_diagnostic(Severity, Position, File, Message, Stacktrace, ReadSnippet) ->
  Diagnostic = #{
    severity => Severity,
    file => File,
    position => Position,
    message => unicode:characters_to_binary(Message),
    stacktrace => Stacktrace,
    length => 1
  },

  case get(elixir_code_diagnostics) of
    undefined -> print_diagnostic(Diagnostic, ReadSnippet);
    {Tail, true} -> put(elixir_code_diagnostics, {[print_diagnostic(Diagnostic, ReadSnippet) | Tail], true});
    {Tail, false} -> put(elixir_code_diagnostics, {[Diagnostic | Tail], false})
  end,

  case get(elixir_compiler_info) of
    undefined -> ok;
    {CompilerPid, _} -> CompilerPid ! {diagnostic, Diagnostic}
  end,

  ok.

extract_line({L, _}) -> L;
extract_line(L) -> L.

extract_column({_, C}) -> C;
extract_column(_) -> nil.

get_file_line(_, 0) -> nil;
get_file_line(File, LineNumber) ->
  {ok, IoDevice} = file:open(File, [read, {encoding, unicode}]),
  Line = do_get_file_line(IoDevice, LineNumber),
  NoNewline = binary:replace(Line, <<"\n">>, <<>>),
  ok = file:close(IoDevice),
  NoNewline.

do_get_file_line(IoDevice, 1) ->
  Line = io:get_line(IoDevice, ""),
  unicode:characters_to_binary(Line);
do_get_file_line(IoDevice, N) ->
  io:get_line(IoDevice, ""),
  do_get_file_line(IoDevice, N -1).

%% Format snippets
%% "Snippet" here refers to the source code line where the diagnostic/error occured

format_snippet(_Position, nil, Message, nil, Severity, _Stacktrace, _Length) ->
  Formatted = [prefix(Severity), Message],
  unicode:characters_to_binary(Formatted);

format_snippet(Position, File, Message, nil, Severity, Stacktrace, _Length) ->
  Location = location_format(Position, File, Stacktrace),

  Formatted = io_lib:format(
    "~ts~ts\n"
    "└─ ~ts",
    [prefix(Severity), Message, Location]
   ),

  unicode:characters_to_binary(Formatted);

format_snippet(Position, File, Message, Snippet, Severity, Stacktrace, Length) ->
  {Content, Column} =
    case Snippet of
      S when is_map(Snippet) ->
        #{content := C, offset := O} = S,
        {C, O};

      _ when is_binary(Snippet) ->
        {Snippet, extract_column(Position)}
    end,

  LineNumber = extract_line(Position),
  LineDigits = get_line_number_digits(LineNumber, 1),
  Spacing = n_spaces(max(2, LineDigits) + 1),
  LineNumberSpacing = if LineDigits =:= 1 -> 1; true -> 0 end,
  {FormattedLine, ColumnsTrimmed} = format_line(Content),
  Location = location_format(Position, File, Stacktrace),

  Highlight =
    case Column of
      nil -> highlight_below_line(FormattedLine, Severity);
      _ -> highlight_at_position(Column - ColumnsTrimmed, Severity, Length)
    end,

  Formatted = io_lib:format(
    " ~ts~ts~ts\n"
    " ~ts│\n"
    " ~ts~p │ ~ts\n"
    " ~ts│ ~ts\n"
    " ~ts│\n"
    " ~ts└─ ~ts",
    [
     Spacing, prefix(Severity), format_message(Message, LineDigits, 2 + LineNumberSpacing),
     Spacing,
     n_spaces(LineNumberSpacing), LineNumber, FormattedLine,
     Spacing, Highlight,
     Spacing,
     Spacing, Location
    ]),

  unicode:characters_to_binary(Formatted).

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
  pad_line(binary:split(Bin, <<"\n">>, [global]), Padding).

pad_line([Last], _Padding) -> [Last];
pad_line([First, <<"">> | Rest], Padding) -> [First, "\n" | pad_line([<<"">> | Rest], Padding)];
pad_line([First | Rest], Padding) -> [First, Padding | pad_line(Rest, Padding)].

highlight_at_position(Column, Severity, Length) ->
  Spacing = n_spaces(max(Column - 1, 0)),
  case Severity of
    warning ->  highlight([Spacing, lists:duplicate(Length, $~)], warning);
    error -> highlight([Spacing, lists:duplicate(Length, $^)], error)
  end.

highlight_below_line(Line, Severity) ->
  % Don't highlight leading whitespaces in line
  {_, SpacesMatched} = trim_line(Line, 0),

  Length = string:length(Line),
  Highlight = case Severity of
    warning -> highlight(lists:duplicate(Length - SpacesMatched, $~), warning);
    error -> highlight(lists:duplicate(Length - SpacesMatched, $^), error)
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
  emit_diagnostic(warning, Location, File, Warning, [], true).

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
      emit_diagnostic(warning, EnvPosition, EnvFile, Message, EnvStacktrace, true)
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
  emit_diagnostic(error, EnvPosition, EnvFile, Message, EnvStacktrace, true),
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
  {sigil, _, Atom, [Content | _], _, _, _} = parse_erl_term(Full),
  Content2 = case is_binary(Content) of
    true -> Content;
    false -> <<>>
  end,

  % :static_atoms_encoder might encode :sigil_ atoms as arbitrary terms
  MaybeSigil = case is_atom(Atom) of
    true -> case atom_to_binary(Atom) of
      <<"sigil_", Chars/binary>> -> <<"\~", Chars/binary, " ">>;
      _ -> <<>>
    end;
    false -> <<>>
  end,

  Message = <<"syntax error before: sigil ", MaybeSigil/binary, "starting with content '", Content2/binary, "'">>,
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
  {InputString, StartLine, _} = Input,
  Snippet = snippet_line(InputString, Location, StartLine),
  raise(Kind, Message, [{file, File}, {snippet, Snippet} | Location]).

snippet_line(InputString, Location, StartLine) ->
  {line, Line} = lists:keyfind(line, 1, Location),
  case lists:keyfind(column, 1, Location) of
    {column, Column} ->
      Lines = string:split(InputString, "\n", all),
      Snippet = (lists:nth(Line - StartLine + 1, Lines)),
      case string:trim(Snippet, leading) of
        [] -> nil;
        _ -> #{content => elixir_utils:characters_to_binary(Snippet), offset => Column}
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

%% We prefer the stacktrace, if available, as it also contains module/function.
location_format(_Position, _File, [E | _]) ->
  'Elixir.Exception':format_stacktrace_entry(E);
location_format(Position, File, []) ->
  file_format(Position, File).

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
