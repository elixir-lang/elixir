Nonterminals
    Elements
    Value
    IfBlock
    IfBraced
    IfExpression
    ElseBraced
    EndIfBraced.

Terminals
    close_tag
    endif_keyword
    if_keyword
    else_keyword
    open_tag
    not_keyword
    text.

Rootsymbol
    Elements.

Value -> '$empty' : [].

Elements -> '$empty' : [].
Elements -> Elements text : '$1' ++ ['$2'].
Elements -> Elements IfBlock : '$1' ++ ['$2'].

IfBlock -> IfBraced Elements ElseBraced Elements EndIfBraced : {ifelse, '$1', '$2', '$4'}.
IfBlock -> IfBraced Elements EndIfBraced : {'if', '$1', '$2'}.

IfBraced -> open_tag if_keyword IfExpression close_tag : '$3'.
IfExpression -> not_keyword IfExpression : {'not', '$2'}.
IfExpression -> Value : '$1'.

ElseBraced -> open_tag else_keyword close_tag.
EndIfBraced -> open_tag endif_keyword close_tag.
