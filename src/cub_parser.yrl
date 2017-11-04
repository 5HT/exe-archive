% Copyright Groupoid Infinity, Inc.

mod   -> 'module' id 'where' imp dec       : {module,'$2','$3','$4','$5'}.
imp   -> skip imp                          : '$2'.
imp   -> '$empty'                          : [].
imp   -> 'import' id imp                   : [{import,'$2'}|'$3'].
tele  -> '$empty'                          : [].
tele  -> '(' exp ':' exp ')' tele          : {tele,uncons('$2'),'$4','$6'}.
exp   -> app                               : '$1'.
exp   -> exp arrow exp                     : {arrow,'$1','$3'}.
exp   -> '(' exp ')'                       : '$2'.
exp   -> lam '(' exp ':' exp ')' arrow exp : {lam,uncons('$3'),'$5','$8'}.
exp   -> '(' exp ':' exp ')' arrow exp     : {pi,uncons('$2'),'$4','$7'}.
exp   -> '(' id ':' exp ')' '*' exp        : {sigma,'$2','$4','$7'}.
exp   -> id                                : '$1'.
app   -> exp exp                           : {app,'$1','$2'}.
dec   -> '$empty'                          : [].
dec   -> def skip dec                      : ['$1'|'$3'].
def   -> 'data' id tele '=' sum            : {data,'$2','$3','$5'}.
def   -> id tele ':' exp '=' exp           : {def,'$1','$2','$4','$6'}.
sum   -> '$empty'                          : [].
sum   -> id tele                           : {ctor,'$1','$2'}.
sum   -> id tele '|' sum                   : [{ctor,'$1','$2'}|'$4'].

Rootsymbol mod.
Left 100 app.
Nonterminals mod imp tele exp app dec def ids sum ids2 prod.
Terminals id digits atom oper 'module' 'where' 'import' skip lam
          '(' ')' '[' ']' '{' '}' '.' ',' ':' '=' '#' '|' '+' '-' '*' '/'
          arrow forall 'record' 'data' 'let' 'in' 'case' 'of'.

Erlang       code.

uncons({app,L,R}) -> [L|uncons(R)];
uncons(X) -> [X].
