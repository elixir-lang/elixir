-module(elixir_transform).
-export([parse_transform/2]).

parse_transform(Forms, Options) ->
    [PTLoaded, ELoaded] = [ code:ensure_loaded(M) || M <- [parse_trans, '__MAIN__-Module'] ],
    case {PTLoaded, ELoaded} of
        {{error, nofile}, _} ->
            io:format("Error: parse_trans was not found~n"),
            [];
        {_, {error, nofile}} ->
            io:format("Error: Elixir was not found~n"),
            [];
        _ ->
            {Forms1, _State} = parse_trans:transform(fun do_transform/4,
                                                     undefined,
                                                     Forms, Options),
            Result = parse_trans:revert(Forms1),
            %% io:format("~s~n",[[ erl_pp:form(F) || F <- Result]]),
            Result
    end.

do_transform(atom, {atom, Line, Atom}, _Context, State) ->
    List = atom_to_list(Atom),
    case string:to_lower(List) of 
        "elixir." ++ _ ->
            [_|Mods] = string:tokens(List,"."),
            {{atom, Line, '__MAIN__-Module':concat(Mods)},false, State};
        _ ->
            {{atom, Line, Atom},false,State}
    end;
do_transform(_Type, Form, _Context, State) ->
    {Form, true, State}.
