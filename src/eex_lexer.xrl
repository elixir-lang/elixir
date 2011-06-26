Definitions.

NotOpening = ([^<]|<[^%\?])
NotClosing = ([^%\?]|[^%\?]>)

Rules.

<%#{NotClosing}*%> : { token, { comment, TokenLine, sublist(3,2,TokenChars,TokenLen) } }.
<%(%|\?|%=|%==|%#){NotClosing}*(%|\?)> : { token, { text, TokenLine, [$<|sublist(2,0,TokenChars,TokenLen)] } }.

<%=={NotClosing}*%> : { token, { start_end, TokenLine, '==', sublist(4,2,TokenChars,TokenLen) } }.
<%={NotClosing}*%> : { token, { start_end, TokenLine, '=', sublist(3,2,TokenChars,TokenLen) } }.
<%{NotClosing}*%> : { token, { start_end, TokenLine, '%', sublist(2,2,TokenChars,TokenLen) } }.

<%=={NotClosing}*\?> : { token, { start_mark, TokenLine, '==', sublist(4,2,TokenChars,TokenLen) } }.
<%={NotClosing}*\?> : { token, { start_mark, TokenLine, '=', sublist(3,2,TokenChars,TokenLen) } }.
<%{NotClosing}*\?> : { token, { start_mark, TokenLine, '%', sublist(2,2,TokenChars,TokenLen) } }.

<\?{NotClosing}*\?> : { token, { mark_mark, TokenLine, sublist(2,2,TokenChars,TokenLen) } }.
<\?{NotClosing}*%> : { token, { mark_end, TokenLine, sublist(2,2,TokenChars,TokenLen) } }.

{NotOpening}+ : { token, { text, TokenLine, TokenChars } }.

Erlang code.

sublist(Start, End, Chars, Length) ->
  lists:sublist(Chars, 1 + Start, Length - (Start + End)).