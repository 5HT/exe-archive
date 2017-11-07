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
exp -> 'split' cobrs : {split,'$2'}.
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
dec -> codec : '$1'.
codec -> def skip dec : ['$1'|'$3'].
codec -> def dec : ['$1'|'$2'].
def -> 'data' id tele '=' sum : {data,'$2','$3','$5'}.
def -> id tele ':' exp '=' exp : {def,'$1','$2','$4','$6'}.
def -> id tele ':' exp '=' exp 'where' codec : {def,'$1','$2','$4','$6','$8'}.
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
Nonterminals mod imp tele exp app dec def ids sum prod cotele br brs cobrs codec.
Terminals id digits atom oper 'module' 'where' 'import' skip lam
'(' ')' '[' ']' '{' '}' '.' ',' ':' '=' '#' '|' '+' '-' '*' '/'
arrow forall 'record' 'data' 'let' 'in' '.1' '.2' 'split'.
Erlang code.
uncons({app,L,R}) -> [L|uncons(R)];
uncons(X) -> [X].
