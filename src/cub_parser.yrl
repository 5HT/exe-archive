mod -> 'module' id 'where' imp dec : {module,'$2','$3','$4','$5'}.
imp -> skip imp : '$2'.
imp -> '$empty' : [].
imp -> 'import' id imp : [{import,'$2'}|'$3'].
ide -> '$empty' : [].
ide -> ids : '$1'.
ids -> id ide : ['$1'|'$2'].
tele -> '$empty' : [].
tele -> cotele : '$1'.
cotele -> '(' exp ':' exp ')' tele : {tele,uncons('$2'),'$4','$6'}.
exp -> '(' exp ')' : '$2'.
exp -> id : '$1'.
exp -> exp '.1' : {fst,'$1'}.
exp -> exp '.2' : {snd,'$1'}.
exp -> lam cotele arrow exp : {lam,'$2','$4'}.
exp -> cotele arrow exp : {pi,'$1','$3'}.
exp -> exp arrow exp : {pi,'$1','$3'}.
exp -> cotele '*' exp : {sigma,'$1','$3'}.
exp -> app : '$1'.
app -> exp exp : {app,'$1','$2'}.
dec -> '$empty' : [].
dec -> def skip dec : ['$1'|'$3'].
dec -> def dec : ['$1'|'$2'].
def -> 'data' id tele '=' sum : {data,'$2','$3','$5'}.
def -> id tele ':' exp '=' exp : {def,'$1','$2','$4','$6'}.
sum -> '$empty' : [].
sum -> id tele : {ctor,'$1','$2'}.
sum -> id tele '|' sum : [{ctor,'$1','$2'}|'$4'].
Rootsymbol mod.
Left 20 app.
Right 10 exp.
Nonterminals mod imp tele exp app dec def ids ide sum prod cotele.
Terminals id digits atom oper 'module' 'where' 'import' skip lam
'(' ')' '[' ']' '{' '}' '.' ',' ':' '=' '#' '|' '+' '-' '*' '/'
arrow forall 'record' 'data' 'let' 'in' 'case' 'of' '.1' '.2' .
Erlang code.
uncons({app,L,R}) -> [L|uncons(R)];
uncons(X) -> [X].
