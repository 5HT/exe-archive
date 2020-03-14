% Copyright Groupoid Infinity, Inc.

mod -> 'module' id 'where' imp dec : {module,'$2','$3','$4','$5'}.
imp -> skip imp : '$2'.
imp -> '$empty' : [].
imp -> 'import' id imp : [{import,'$2'}|'$3'].
ids -> '$empty' : [].
ids -> id ids : ['$1'|'$2'].
tele -> '$empty' : [].
tele -> cotele : '$1'.
br -> ids arrow exp : {br,'$1','$3'}.
brs -> '$empty' : [].
brs -> cobrs : '$1'.
cobrs -> '|' br brs : ['$2'|'$3'].
cotele -> '(' exp ':' exp ')' tele : {tele,uncons('$2'),'$4','$6'}.
sys -> '[' sides ']' : {sys,'$'}.
sides -> '$empty' : [].
sides -> side : '$1'.
sides -> side ',' sides : ['$1'|'$3'].
side -> '(' id '=' dir ')' arrow exp : {side,'$2','$4','$7'}.
formula -> formula forall f1 : {join,'$1','$3'}.
f1 -> f1 meet f2 : {meet,'$1','$3'}.
f2 -> '-' f2 : {neg,'$2'}.
f2 -> id : '$1'.
f2 -> dir : {dir,'$1'}.
dir -> '0' : 0.
dir -> '1' : 1.
exp -> 'split' cobrs : {split,'$2'}.
exp -> '(' exp ')' : '$2'.
exp -> id : '$1'.
exp -> exp '@' formula : {papp,'$1','$3'}.
exp -> '<' ids '>' exp : {plam,'$1','$3'}.
exp -> exp '.1' : {fst,'$1'}.
exp -> exp '.2' : {snd,'$1'}.
exp -> lam cotele arrow exp : {lam,'$2','$4'}.
exp -> cotele arrow exp : {pi,'$1','$3'}.
exp -> exp arrow exp : {pi,'$1','$3'}.
exp -> cotele '*' exp : {sigma,'$1','$3'}.
exp -> 'comp' exp exp sys : {comp,'$2','$3','$4'}.
exp -> app : '$1'.
app -> exp exp : {app,'$1','$2'}.
dec -> '$empty' : [].
dec -> codec : '$1'.
codec -> def skip dec : ['$1'|'$3'].
codec -> def dec : ['$1'|'$2'].
def -> 'data' id tele '=' sum : {data,'$2','$3','$5'}.
def -> id tele ':' exp '=' exp : {def,'$1','$2','$4','$6'}.
def -> id tele ':' exp '=' exp 'where' def : {def,'$1','$2','$4','$6','$8'}.
sum -> '$empty' : [].
sum -> id tele : {ctor,'$1','$2'}.
sum -> id tele '|' sum : [{ctor,'$1','$2'}|'$4'].
Rootsymbol mod.
Right 100 exp.
Right 100 arrow.
Left 200 app.
Left 10 '|'.
Left 10 skip.
Left 10 'data'.
Left 10 id.
Left 10 def.
Nonterminals mod imp tele exp app dec def ids sum prod cotele br brs cobrs codec formula dir f1 f2 side sides sys papp plam.
Terminals id digits atom oper 'module' 'where' 'import' skip lam meet
'(' ')' '[' ']' '<' '>' '{' '}' '.' ',' ':' '=' '#' '|' '+' '-' '*' '/' '@' '0' '1'
arrow forall 'record' 'data' 'let' 'in' '.1' '.2' 'split' 'comp'.
Erlang code.
uncons({app,L,R}) -> [R|uncons(L)];
uncons(X) -> [X].
