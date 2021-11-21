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

%% Confusables
%% Processed from http://www.unicode.org/reports/tr39
-define(confusable(C), C =:= 16#3164;
                       C =:= 16#7FA;
                       C =:= 16#30FC;
                       C =:= 16#A7F7;
                       C =:= 16#1173;
                       C =:= 16#3161;
                       C =:= 16#A4F9;
                       C =:= 16#2D0;
                       C =:= 16#A4FD;
                       C =:= 16#1C3;
                       C =:= 16#2D51;
                       C =:= 16#294;
                       C =:= 16#97D;
                       C =:= 16#A6EB;
                       C =:= 16#A4F8;
                       C =:= 16#A78F;
                       C =:= 16#1427;
                       C =:= 16#1427;
                       C =:= 16#2B9;
                       C =:= 16#2C8;
                       C =:= 16#2CA;
                       C =:= 16#2CB;
                       C =:= 16#2BB;
                       C =:= 16#2BD;
                       C =:= 16#2BC;
                       C =:= 16#2BE;
                       C =:= 16#A78C;
                       C =:= 16#7F4;
                       C =:= 16#7F5;
                       C =:= 16#144A;
                       C =:= 16#16CC;
                       C =:= 16#2BA;
                       C =:= 16#2EE;
                       C =:= 16#5F2;
                       C =:= 16#1031F;
                       C =:= 16#3033;
                       C =:= 16#30CE;
                       C =:= 16#4E3F;
                       C =:= 16#4E36;
                       C =:= 16#30FD;
                       C =:= 16#A778;
                       C =:= 16#1029B;
                       C =:= 16#1438;
                       C =:= 16#16B2;
                       C =:= 16#304F;
                       C =:= 16#21FE8;
                       C =:= 16#1433;
                       C =:= 16#16F3F).
