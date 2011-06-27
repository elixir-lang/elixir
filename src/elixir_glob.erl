%% Modified version of filelib:wildcard that can handle **.

%%
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
-export([wildcard/1]).

wildcard(Pattern) when is_list(Pattern) ->
  do_wildcard(Pattern, file).

do_wildcard(Pattern, Mod) when is_list(Pattern) ->
  do_wildcard_comp(do_compile_wildcard(Pattern), Mod).

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
  do_double_star([Base], Rest, Result, length(Rest));
do_wildcard_3(Base, [Pattern|Rest], Result, Mod) ->
  case do_list_dir(Base, Mod) of
  	{ok, Files0} ->
	    Files = lists:sort(Files0),
	    Matches = wildcard_4(Pattern, Files, Base, []),
	    do_wildcard_2(Matches, Rest, Result, Mod);
  	_ ->
	    Result
  end;
do_wildcard_3(Base, [], Result, _Mod) ->
    [Base|Result].

% Check if the pattern match for the list of files.
wildcard_4(Pattern, [File|Rest], Base, Result) when is_binary(File) ->
  wildcard_4(Pattern, [binary_to_list(File)|Rest], Base, Result);
wildcard_4(Pattern, [File|Rest], Base, Result) ->
  NewResult = case wildcard_5(Pattern, File) of
  	true  -> [join(Base, File)|Result];
  	false -> Result
  end,
  wildcard_4(Pattern, Rest, Base, NewResult);
wildcard_4(_Patt, [], _Base, Result) ->
  Result.

% Match the pattern with the file.
wildcard_5([question|Rest1], [_|Rest2]) ->
  wildcard_5(Rest1, Rest2);
wildcard_5([accept], _) ->
  true;
wildcard_5([double_star], _) ->
  true;
wildcard_5([star|Rest], File) ->
  do_star(Rest, File);
wildcard_5([{one_of, Ordset}|Rest], [C|File]) ->
  case ordsets:is_element(C, Ordset) of
  	true  -> wildcard_5(Rest, File);
  	false -> false
  end;
wildcard_5([{alt, Alts}], File) ->
  do_alt(Alts, File);
wildcard_5([C|Rest1], [C|Rest2]) when is_integer(C) ->
  wildcard_5(Rest1, Rest2);
wildcard_5([X|_], [Y|_]) when is_integer(X), is_integer(Y) ->
  false;
wildcard_5([], []) ->
  true;
wildcard_5([], [_|_]) ->
  false;
wildcard_5([_|_], []) ->
  false.

%%% Handle specifics patterns

% Handle double stars by expand directories recursively until
% files are found. And when a file is found, check if the pattern
% matches.
do_double_star([H|T], Rest, Result, MatchLength) ->
  NewResult = case do_list_dir(H, file) of
	{ok, Files0} ->
	    Files = [join(H, File) || File <- lists:sort(Files0)],
	    do_double_star(Files, Rest, Result, MatchLength);
	_ ->
	  Split = filename:split(H),
	  CurrentLength = length(Split),
	  case CurrentLength >= MatchLength of
	    true  ->
	      Parts = lists:nthtail(CurrentLength - MatchLength, Split),
	      case do_part_match(Parts, Rest) of
	        true  -> [H|Result];
	        false -> Result
	      end;
      false -> Result
    end
  end,
  do_double_star(T, Rest, NewResult, MatchLength);

do_double_star([], _Rest, Result, _MatchLength) ->
  lists:reverse(Result).

do_part_match([PartH|PartT], [PatternH|PatternT]) ->
  case wildcard_5(PatternH, PartH) of
    true  -> do_part_match(PartT, PatternT);
    false -> false
  end;
do_part_match([], []) ->
  true.

do_star(Pattern, [X|Rest]) ->
  case wildcard_5(Pattern, [X|Rest]) of
  	true  -> true;
  	false -> do_star(Pattern, Rest)
  end;
do_star(Pattern, []) ->
  wildcard_5(Pattern, []).

do_alt([Alt|Rest], File) ->
  case wildcard_5(Alt, File) of
  	true  -> true;
  	false -> do_alt(Rest, File)
  end;
do_alt([], _File) ->
  false.

do_list_dir(current, Mod) -> file:list_dir(".");
do_list_dir(Dir, Mod) ->     file:list_dir(Dir).

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