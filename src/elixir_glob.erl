%% Modified version of filelib:wildcard that can handle "**"
%% and automatically skips directories starting with "." by default.

%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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

-module(elixir_glob).
-compile({no_auto_import,[error/1]}).
-export([wildcard/2]).

wildcard(Pattern, Mod) when is_binary(Pattern) ->
  Compiled = do_compile_wildcard(binary_to_list(Pattern)),
  Results = do_wildcard_comp(Compiled, Mod),
  [list_to_binary(Result) || Result <- lists:sort(Results)].

do_wildcard_comp({compiled_wildcard,{exists,File}}, Mod) ->
  case file:read_file_info(File) of
    {ok,_} -> [File];
    _ -> []
  end;

do_wildcard_comp({compiled_wildcard,[Base|Rest]}, Mod) ->
  do_wildcard_1([Base], Rest, Mod).

%%% Pattern matching using a compiled wildcard.

% Receives files and patterns
do_wildcard_1(Files, Pattern, Mod) ->
    do_wildcard_2(Files, Pattern, [], Mod).

% Loop files
do_wildcard_2([File|Rest], Pattern, Result, Mod) ->
  NewResult = do_wildcard_3(File, Pattern, Result, Mod),
  do_wildcard_2(Rest, Pattern, NewResult, Mod);
do_wildcard_2([], _, Result, _Mod) ->
  Result.

% If a file, add it to the results list, if a directory,
% expand it and match patterns. If a double star is found
% at the end, convert it to an accept to avoid looping
% recursively.
do_wildcard_3(Base, [[double_star]], Result, Mod) ->
  do_wildcard_3(Base, [[accept]], Result, Mod);
do_wildcard_3(Base, [[double_star]|Rest], Result, Mod) ->
  case do_list_dir(Base, Mod) of
    {ok, Files} ->
      do_double_star(Base, Files, Rest, Result, length(Rest), Mod);
    _ ->
      Result
  end;

do_wildcard_3(Base, [Pattern|Rest], Result, Mod) ->
  case do_list_dir(Base, true) of
    {ok, Files} ->
      Matches = wildcard_4(Pattern, Files, Base, [], Mod),
      do_wildcard_2(Matches, Rest, Result, Mod);
    _ ->
      Result
  end;
do_wildcard_3(Base, [], Result, _Mod) ->
    [Base|Result].

% Check if the pattern match for the list of files.
wildcard_4(Pattern, [File|Rest], Base, Result, Mod) ->
  NewResult = case wildcard_5(Pattern, File, Mod) of
    true  -> [join(Base, File)|Result];
    false -> Result
  end,
  wildcard_4(Pattern, Rest, Base, NewResult, Mod);
wildcard_4(_Patt, [], _Base, Result, _Mod) ->
  Result.

% Match the pattern with the file.
wildcard_5([question|Rest1], [_|Rest2], Mod) ->
  wildcard_5(Rest1, Rest2, Mod);
wildcard_5([accept], [$.|_], false) ->
  false; % Skip if false and it starts with .
wildcard_5([accept], _, _Mod) ->
  true;
wildcard_5([double_star], _, _Mod) ->
  true;
wildcard_5([star|Rest], [$.|_], false) ->
  false;
wildcard_5([star|Rest], File, Mod) ->
  do_star(Rest, File, Mod);
wildcard_5([{one_of, Ordset}|Rest], [C|File], Mod) ->
  case ordsets:is_element(C, Ordset) of
    true  -> wildcard_5(Rest, File, Mod);
    false -> false
  end;
wildcard_5([{alt, Alts}], File, Mod) ->
  do_alt(Alts, File, Mod);
wildcard_5([C|Rest1], [C|Rest2], Mod) when is_integer(C) ->
  wildcard_5(Rest1, Rest2, Mod);
wildcard_5([X|_], [Y|_], _Mod) when is_integer(X), is_integer(Y) ->
  false;
wildcard_5([], [], _Mod) ->
  true;
wildcard_5([], [_|_], _Mod) ->
  false;
wildcard_5([_|_], [], _Mod) ->
  false.

%%% Handle specifics patterns

% Handle double stars by expand directories recursively until
% files are found. And when a file is found, check if the pattern
% matches.
do_double_star(Base, [H|T], Rest, Result, MatchLength, Mod) ->
  Full = join(Base, H),
  Split = filename:split(H),
  CurrentLength = length(Split),

  % Check if the current part belongs to the list
  PartResult = case CurrentLength >= MatchLength of
    true  ->
      Parts = lists:nthtail(CurrentLength - MatchLength, Split),
      case do_part_match(Parts, Rest, Mod) of
        true  -> [Full|Result];
        false -> Result
      end;
    false -> Result
  end,

  % If it is a directory, expand it further.
  FinalResult = case do_list_dir(Full, Mod) of
    {ok, Files} ->
      JoinedFiles = [join(H, File) || File <- Files],
      do_double_star(Base, JoinedFiles, Rest, PartResult, MatchLength, Mod);
    _ -> PartResult
  end,
  do_double_star(Base, T, Rest, FinalResult, MatchLength, Mod);

do_double_star(_Base, [], _Rest, Result, _MatchLength, _Mod) ->
  Result.

do_part_match([PartH|PartT], [PatternH|PatternT], Mod) ->
  case wildcard_5(PatternH, PartH, Mod) of
    true  -> do_part_match(PartT, PatternT, Mod);
    false -> false
  end;
do_part_match([], [], _Mod) ->
  true.

do_star(Pattern, [X|Rest], Mod) ->
  case wildcard_5(Pattern, [X|Rest], Mod) of
    true  -> true;
    false -> do_star(Pattern, Rest, Mod)
  end;
do_star(Pattern, [], Mod) ->
  wildcard_5(Pattern, [], Mod).

do_alt([Alt|Rest], File, Mod) ->
  case wildcard_5(Alt, File, Mod) of
    true  -> true;
    false -> do_alt(Rest, File, Mod)
  end;
do_alt([], _File, _Mod) ->
  false.

do_list_dir(current, Bool) -> do_list_dir(".", Bool);
do_list_dir(Dir, Bool)     ->
  case file:list_dir(Dir) of
    {ok,Files} -> {ok,convert_list(Files, [], Bool)};
    Else -> Else
  end.

convert_list([H|T], Acc, Bool) when is_binary(H) ->
  convert_list([binary_to_list(H)|T], Acc, Bool);
convert_list([H|T], Acc, false) when hd(H) == $. ->
  convert_list(T, Acc, false);
convert_list([H|T], Acc, Bool) ->
  convert_list(T, [H|Acc], Bool);
convert_list([], Acc, _Bool) ->
  Acc.

join(current, File) -> File;
join(Base, File) -> filename:join(Base, File).

%%% Compiling a wildcard.

do_compile_wildcard(Pattern) ->
  {compiled_wildcard,compile_wildcard_1(Pattern)}.

compile_wildcard_1(Pattern) ->
  [Root|Rest] = filename:split(Pattern),
  case filename:pathtype(Root) of
    relative ->
      compile_wildcard_2([Root|Rest], current);
    _ ->
      compile_wildcard_2(Rest, [Root])
  end.

compile_wildcard_2([Part|Rest], Root) ->
  case compile_part(Part) of
    Part ->
      compile_wildcard_2(Rest, join(Root, Part));
    Pattern ->
      compile_wildcard_3(Rest, [Pattern,Root])
  end;
compile_wildcard_2([], Root) -> {exists,Root}.

compile_wildcard_3([Part|Rest], Result) ->
  compile_wildcard_3(Rest, [compile_part(Part)|Result]);
compile_wildcard_3([], Result) ->
  lists:reverse(Result).

compile_part(Part) ->
  compile_part(Part, false, []).

compile_part_to_sep(Part) ->
  compile_part(Part, true, []).

compile_part([], true, _) ->
  error(missing_delimiter);
compile_part([$,|Rest], true, Result) ->
  {ok, $,, lists:reverse(Result), Rest};
compile_part([$}|Rest], true, Result) ->
  {ok, $}, lists:reverse(Result), Rest};
compile_part([$?|Rest], Upto, Result) ->
  compile_part(Rest, Upto, [question|Result]);
compile_part([$*,$*], Upto, Result) ->
  compile_part([], Upto, [double_star|Result]);
compile_part([$*,$*|Rest], Upto, Result) ->
  compile_part(Rest, Upto, [star|Result]);
compile_part([$*], Upto, Result) ->
  compile_part([], Upto, [accept|Result]);
compile_part([$*|Rest], Upto, Result) ->
  compile_part(Rest, Upto, [star|Result]);
compile_part([$[|Rest], Upto, Result) ->
  case compile_charset(Rest, ordsets:new()) of
    {ok, Charset, Rest1} ->
      compile_part(Rest1, Upto, [Charset|Result]);
    error ->
      compile_part(Rest, Upto, [$[|Result])
  end;
compile_part([${|Rest], Upto, Result) ->
  case compile_alt(Rest) of
    {ok, Alt} ->
      lists:reverse(Result, [Alt]);
    error ->
      compile_part(Rest, Upto, [${|Result])
  end;
compile_part([X|Rest], Upto, Result) ->
  compile_part(Rest, Upto, [X|Result]);
compile_part([], _Upto, Result) ->
  lists:reverse(Result).

compile_charset([$]|Rest], Ordset) ->
  compile_charset1(Rest, ordsets:add_element($], Ordset));
compile_charset([$-|Rest], Ordset) ->
  compile_charset1(Rest, ordsets:add_element($-, Ordset));
compile_charset([], _Ordset) ->
  error;
compile_charset(List, Ordset) ->
  compile_charset1(List, Ordset).

compile_charset1([Lower, $-, Upper|Rest], Ordset) when Lower =< Upper ->
  compile_charset1(Rest, compile_range(Lower, Upper, Ordset));
compile_charset1([$]|Rest], Ordset) ->
  {ok, {one_of, Ordset}, Rest};
compile_charset1([X|Rest], Ordset) ->
  compile_charset1(Rest, ordsets:add_element(X, Ordset));
compile_charset1([], _Ordset) ->
  error.

compile_range(Lower, Current, Ordset) when Lower =< Current ->
  compile_range(Lower, Current-1, ordsets:add_element(Current, Ordset));
compile_range(_, _, Ordset) ->
  Ordset.

compile_alt(Pattern) ->
  compile_alt(Pattern, []).

compile_alt(Pattern, Result) ->
  case compile_part_to_sep(Pattern) of
    {ok, $,, AltPattern, Rest} ->
      compile_alt(Rest, [AltPattern|Result]);
    {ok, $}, AltPattern, Rest} ->
      NewResult = [AltPattern|Result],
      RestPattern = compile_part(Rest),
      {ok, {alt, [Alt++RestPattern || Alt <- NewResult]}};
    Pattern ->
      error
  end.

error(Reason) ->
  erlang:error({badpattern,Reason}).