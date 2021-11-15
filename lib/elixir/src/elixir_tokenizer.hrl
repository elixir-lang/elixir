%% Numbers
-define(is_hex(S), (?is_digit(S) orelse (S >= $A andalso S =< $F) orelse (S >= $a andalso S =< $f))).
-define(is_bin(S), (S >= $0 andalso S =< $1)).
-define(is_octal(S), (S >= $0 andalso S =< $7)).

%% Digits and letters
-define(is_digit(S), (S >= $0 andalso S =< $9)).
-define(is_upcase(S), (S >= $A andalso S =< $Z)).
-define(is_downcase(S), (S >= $a andalso S =< $z)).

%% Others
-define(is_quote(S), (S =:= $" orelse S =:= $')).
-define(is_sigil(S), (S =:= $/ orelse S =:= $< orelse S =:= $" orelse S =:= $' orelse
                      S =:= $[ orelse S =:= $( orelse S =:= ${ orelse S =:= $|)).

%% Spaces
-define(is_horizontal_space(S), (S =:= $\s orelse S =:= $\t)).
-define(is_vertical_space(S), (S =:= $\r orelse S =:= $\n)).
-define(is_space(S), (?is_horizontal_space(S) orelse ?is_vertical_space(S))).

%% Bidirectional control
%% Retrieved from https://trojansource.codes/trojan-source.pdf
-define(bidi(C), C =:= 16#202A;
                 C =:= 16#202B;
                 C =:= 16#202D;
                 C =:= 16#202E;
                 C =:= 16#2066;
                 C =:= 16#2067;
                 C =:= 16#2068;
                 C =:= 16#202C;
                 C =:= 16#2069).
