%% A bunch of helpers to help to deal with errors in Elixir source code.
%% This is not exposed in the Elixir language.
%%
%% Note that this is also called by the Erlang backend, so we also support
%% the line number to be none (as it may happen in some erlang errors).
-module(elixir_errors).
-export([compile_error/3, compile_error/4,  form_error/4, parse_error/5]).
-export([warning_prefix/0, erl_warn/3, print_warning/3, log_and_print_warning/4, form_warn/4]).
-include("elixir.hrl").
-type location() :: non_neg_integer() | {non_neg_integer(), non_neg_integer()}.

%% Low-level warning, should be used only from Erlang passes.
-spec erl_warn(location() | none, unicode:chardata(), unicode:chardata()) -> ok.
erl_warn(none, File, Warning) ->
  erl_warn(0, File, Warning);
erl_warn(Location, File, Warning) when is_binary(File) ->
  send_warning(Location, File, Warning),
  print_warning(Location, File, Warning).

-spec print_warning(location(), unicode:chardata(), unicode:chardata()) -> ok.
print_warning(Location, File, Warning) ->
  print_warning([Warning, "\n  ", file_format(Location, File), $\n]).

-spec log_and_print_warning(location(), unicode:chardata() | nil, unicode:chardata(), unicode:chardata()) -> ok.
log_and_print_warning(Location, File, LogMessage, PrintMessage) when is_binary(File) or (File == nil) ->
  send_warning(Location, File, LogMessage),
  print_warning(PrintMessage).

-spec warning_prefix() -> binary().
warning_prefix() ->
  case application:get_env(elixir, ansi_enabled) of
    {ok, true} -> <<"\e[33mwarning: \e[0m">>;
    _ -> <<"warning: ">>
  end.

%% General forms handling.

-spec form_error(list(), binary() | #{file := binary(), _ => _}, module(), any()) -> no_return().
form_error(Meta, #{file := File}, Module, Desc) ->
  compile_error(Meta, File, Module:format_error(Desc));
form_error(Meta, File, Module, Desc) ->
  compile_error(Meta, File, Module:format_error(Desc)).

-spec form_warn(list(), binary() | #{file := binary(), _ => _}, module(), any()) -> ok.
form_warn(Meta, File, Module, Desc) when is_list(Meta), is_binary(File) ->
  form_warn(Meta, #{file => File}, Module, Desc);
form_warn(Meta, #{file := File} = E, Module, Desc) when is_list(Meta) ->
  % Skip warnings during bootstrap, they will be reported during recompilation
  case elixir_config:is_bootstrap() of
    true -> ok;
    false -> do_form_warn(Meta, File, E, Module:format_error(Desc))
  end.

do_form_warn(Meta, GivenFile, E, Warning) ->
  [{file, File}, {line, Line}] = meta_location(Meta, GivenFile),

  Location =
    case E of
      #{function := {Name, Arity}, module := Module} ->
        [file_format(Line, File), ": ", 'Elixir.Exception':format_mfa(Module, Name, Arity)];
      #{module := Module} when Module /= nil ->
        [file_format(Line, File), ": ", elixir_aliases:inspect(Module)];
      #{} ->
        file_format(Line, File)
    end,

  log_and_print_warning(Line, File, Warning, [Warning, "\n  ", Location, $\n]).

%% Compilation error.

-spec compile_error(list(), binary(), binary() | unicode:charlist()) -> no_return().
-spec compile_error(list(), binary(), string(), list()) -> no_return().

compile_error(Meta, File, Message) when is_binary(Message) ->
  MetaLocation = meta_location(Meta, File),
  raise('Elixir.CompileError', Message, MetaLocation);
compile_error(Meta, File, Message) when is_list(Message) ->
  MetaLocation = meta_location(Meta, File),
  raise('Elixir.CompileError', elixir_utils:characters_to_binary(Message), MetaLocation).

compile_error(Meta, File, Format, Args) when is_list(Format)  ->
  compile_error(Meta, File, io_lib:format(Format, Args)).

%% Tokenization parsing/errors.
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
  Message = <<"syntax error before: sigil \~", Sigil, " starting with content '", Content2/binary, "'">>,
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

%% Helpers

print_warning(Message) ->
  io:put_chars(standard_error, [warning_prefix(), Message, $\n]),
  ok.

send_warning(Line, File, Message) ->
  case get(elixir_compiler_info) of
    undefined -> ok;
    {CompilerPid, _} -> CompilerPid ! {warning, File, Line, Message}
  end,
  ok.

file_format({0, _Column}, File) ->
  io_lib:format("~ts", [elixir_utils:relative_to_cwd(File)]);
file_format({Line, Column}, File) ->
  io_lib:format("~ts:~w:~w", [elixir_utils:relative_to_cwd(File), Line, Column]);
file_format(0, File) ->
  io_lib:format("~ts", [elixir_utils:relative_to_cwd(File)]);
file_format(Line, File) ->
  io_lib:format("~ts:~w", [elixir_utils:relative_to_cwd(File), Line]).

meta_location(Meta, File) ->
  case elixir_utils:meta_keep(Meta) of
    {F, L} -> [{file, F}, {line, L}];
    nil    -> [{file, File}, {line, ?line(Meta)}]
  end.

raise(Kind, Message, Opts) when is_binary(Message) ->
  Stacktrace = try throw(ok) catch _:_:Stack -> Stack end,
  Exception = Kind:exception([{description, Message} | Opts]),
  erlang:raise(error, Exception, tl(Stacktrace)).
