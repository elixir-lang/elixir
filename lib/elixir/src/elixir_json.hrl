%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%% A lot of the macros below use multi-value comparisons where
%% range checks would have worked just fine. This is because
%% the compiler & JIT can emit better code in some cases when
%% multiple clauses are to be dispatched based on such sets
%% of values. They'll generate an efficient "jump table",
%% which gets to the correct clause in one go, rather
%% than going through a set of comparisons.
%% However, this might not always be the best way (see is_0_to_9),
%% so as always with any performance work - measure, don't guess!

-define(is_1_to_9(X),
    X =:= $1 orelse
    X =:= $2 orelse
    X =:= $3 orelse
    X =:= $4 orelse
    X =:= $5 orelse
    X =:= $6 orelse
    X =:= $7 orelse
    X =:= $8 orelse
    X =:= $9
).

-define(is_0_to_9(X), X >= $0 andalso X =< $9).

-define(is_ws(X), X =:= $\s; X =:= $\t; X =:= $\r; X =:= $\n).

-define(is_ascii_escape(Byte),
    Byte =:= 0 orelse
    Byte =:= 1 orelse
    Byte =:= 2 orelse
    Byte =:= 3 orelse
    Byte =:= 4 orelse
    Byte =:= 5 orelse
    Byte =:= 6 orelse
    Byte =:= 7 orelse
    Byte =:= 8 orelse
    Byte =:= 9 orelse
    Byte =:= 10 orelse
    Byte =:= 11 orelse
    Byte =:= 12 orelse
    Byte =:= 13 orelse
    Byte =:= 14 orelse
    Byte =:= 15 orelse
    Byte =:= 16 orelse
    Byte =:= 17 orelse
    Byte =:= 18 orelse
    Byte =:= 19 orelse
    Byte =:= 20 orelse
    Byte =:= 21 orelse
    Byte =:= 22 orelse
    Byte =:= 23 orelse
    Byte =:= 24 orelse
    Byte =:= 25 orelse
    Byte =:= 26 orelse
    Byte =:= 27 orelse
    Byte =:= 28 orelse
    Byte =:= 29 orelse
    Byte =:= 30 orelse
    Byte =:= 31 orelse
    Byte =:= 34 orelse
    Byte =:= 92
).
-define(is_ascii_plain(Byte),
    Byte =:= 32 orelse
    Byte =:= 33 orelse
    Byte =:= 35 orelse
    Byte =:= 36 orelse
    Byte =:= 37 orelse
    Byte =:= 38 orelse
    Byte =:= 39 orelse
    Byte =:= 40 orelse
    Byte =:= 41 orelse
    Byte =:= 42 orelse
    Byte =:= 43 orelse
    Byte =:= 44 orelse
    Byte =:= 45 orelse
    Byte =:= 46 orelse
    Byte =:= 47 orelse
    Byte =:= 48 orelse
    Byte =:= 49 orelse
    Byte =:= 50 orelse
    Byte =:= 51 orelse
    Byte =:= 52 orelse
    Byte =:= 53 orelse
    Byte =:= 54 orelse
    Byte =:= 55 orelse
    Byte =:= 56 orelse
    Byte =:= 57 orelse
    Byte =:= 58 orelse
    Byte =:= 59 orelse
    Byte =:= 60 orelse
    Byte =:= 61 orelse
    Byte =:= 62 orelse
    Byte =:= 63 orelse
    Byte =:= 64 orelse
    Byte =:= 65 orelse
    Byte =:= 66 orelse
    Byte =:= 67 orelse
    Byte =:= 68 orelse
    Byte =:= 69 orelse
    Byte =:= 70 orelse
    Byte =:= 71 orelse
    Byte =:= 72 orelse
    Byte =:= 73 orelse
    Byte =:= 74 orelse
    Byte =:= 75 orelse
    Byte =:= 76 orelse
    Byte =:= 77 orelse
    Byte =:= 78 orelse
    Byte =:= 79 orelse
    Byte =:= 80 orelse
    Byte =:= 81 orelse
    Byte =:= 82 orelse
    Byte =:= 83 orelse
    Byte =:= 84 orelse
    Byte =:= 85 orelse
    Byte =:= 86 orelse
    Byte =:= 87 orelse
    Byte =:= 88 orelse
    Byte =:= 89 orelse
    Byte =:= 90 orelse
    Byte =:= 91 orelse
    Byte =:= 93 orelse
    Byte =:= 94 orelse
    Byte =:= 95 orelse
    Byte =:= 96 orelse
    Byte =:= 97 orelse
    Byte =:= 98 orelse
    Byte =:= 99 orelse
    Byte =:= 100 orelse
    Byte =:= 101 orelse
    Byte =:= 102 orelse
    Byte =:= 103 orelse
    Byte =:= 104 orelse
    Byte =:= 105 orelse
    Byte =:= 106 orelse
    Byte =:= 107 orelse
    Byte =:= 108 orelse
    Byte =:= 109 orelse
    Byte =:= 110 orelse
    Byte =:= 111 orelse
    Byte =:= 112 orelse
    Byte =:= 113 orelse
    Byte =:= 114 orelse
    Byte =:= 115 orelse
    Byte =:= 116 orelse
    Byte =:= 117 orelse
    Byte =:= 118 orelse
    Byte =:= 119 orelse
    Byte =:= 120 orelse
    Byte =:= 121 orelse
    Byte =:= 122 orelse
    Byte =:= 123 orelse
    Byte =:= 124 orelse
    Byte =:= 125 orelse
    Byte =:= 126 orelse
    Byte =:= 127
).

-define(are_all_ascii_plain(B1, B2, B3, B4, B5, B6, B7, B8),
    (?is_ascii_plain(B1)) andalso
    (?is_ascii_plain(B2)) andalso
    (?is_ascii_plain(B3)) andalso
    (?is_ascii_plain(B4)) andalso
    (?is_ascii_plain(B5)) andalso
    (?is_ascii_plain(B6)) andalso
    (?is_ascii_plain(B7)) andalso
    (?is_ascii_plain(B8))
).
