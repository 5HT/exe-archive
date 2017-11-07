% Copyright Groupoid Infinity, Inc.

Definitions.

D = [0-9]
C = [a-zA-Z_]
A = [a-zA-Z_0-9\-\x{2074}-\x{208E}\x{2010}-\x{2191}\x{2193}-\x{2199}\x{2201}-\x{25FF}\x{3B1}-\x{3BA}\x{3BC}-\x{3FF}]
S = ([\t\s\r\n]|--.*)
BC = [\r\n]
Star    = \*
Unit    = \(\)
Slash   = \\
Dot     = \.
Comma   = \,
Arrow   = (\->|\→)
Forall  = (\\/|\∀)
Lambda  = (\\|\λ)
Curly   = [\{\}]
Angle   = [\<\>]
Parens  = [\(\)]
Square  = [\[\]]
Colon   = \:
Hash    = \#
Eq      = \=
Oper    = [\*\+\-\/]
VBar    = \|

Rules.

(data|record|.1|.2|split)            : {token,{list_to_atom(TokenChars),TokenLine}}.
(let|in|module|import|where)         : {token,{list_to_atom(TokenChars),TokenLine}}.
(spawn|send|receive)                 : {token,{list_to_atom(TokenChars),TokenLine}}.
({Curly}|{Parens}|{Angle}|{Square})  : {token,{list_to_atom(TokenChars),TokenLine}}.
({Dot}|{Comma}|{Eq}|{Colon})         : {token,{list_to_atom(TokenChars),TokenLine}}.
({Hash}|{VBar}|{Oper}|{Star})        : {token,{list_to_atom(TokenChars),TokenLine}}.

{D}+            : {token, {digits, TokenLine,list_to_integer(TokenChars)}}.
{A}+            : {token, {id,     TokenLine,TokenChars}}.
{Arrow}         : {token, {arrow,  TokenLine}}.
{Forall}        : {token, {forall, TokenLine}}.
{Lambda}        : {token, {lam,    TokenLine}}.
{Oper}+         : {token, {oper,   TokenLine,TokenChars}}.
({BC}+)         : {token, {skip,   TokenLine}}.
({S}+)          : skip_token.

Erlang code.
