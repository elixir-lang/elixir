-module(atom_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

atom_with_punctuation_test() ->
  {'a?',[]} = elixir:eval(":a?"),
  {'a!',[]} = elixir:eval(":a!").

% atom_inspect_test() ->
%   {String,[]} = elixir:eval("'a.inspect"),
%   <<"'a">> = test_helper:unpack_string(String).
% 
% atom_to_s_test() ->
%   {String,[]} = elixir:eval("'a.to_s"),
%   <<"a">> = test_helper:unpack_string(String).
% 
% separators_atom_test() ->
%   {foo,[]} = elixir:eval("'\"foo\""),
%   {foo,[]} = elixir:eval("'[foo]"),
%   {foo,[]} = elixir:eval("'(foo)"),
%   {foo,[]} = elixir:eval("'{foo}").
% 
% separators_atom_with_interpolation_test() ->
%   {foo,[]} = elixir:eval("'\"f#{'o}o\""),
%   {foo,[]} = elixir:eval("'[f#{'o}o]"),
%   {foo,[]} = elixir:eval("'(f#{'o}o)"),
%   {foo,[]} = elixir:eval("'{f#{'o}o}").
% 
% quoted_atom_chars_are_escaped_test() ->
%   {'"',[]} = elixir:eval("'\"\\\"\"").