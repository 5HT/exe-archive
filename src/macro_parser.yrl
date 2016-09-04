%
% Copyright Groupoid Infinity, Inc.
%

Nonterminals level level_comma_seq id path path_comma_seq
             pretype type type_par_seq preassign assign assign_par_seq
             id_ta id_ta_par_seq match match_seq match_comma_seq
             pattern pattern_comma_seq pattern_arg pattern_arg_seq
             clause clause_par_seq expr expr_ expr_comma_seq encoding .

Terminals    token_id token_digits token_atom token_quoted_literal
             '(' ')' '[' ']' '{' '}' '.' ',' ':' ':=' '#' '|' '+'
           % '-' '*' '/' % uncoment on usage
             token_arrow token_forall token_lambda 'define'
             'packed' 'record' 'new' 'data' 'default' 'let' 'in' 'case' 'of' .

Rootsymbol   assign .

% the actual syntax definition is divided into two parts: (1) concrete; (2) abstract

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PART 1
%
% The concrete syntax (defined by a grammar)
%
% note: all 'foobar_seq' non-terminals are mapped to erlang lists of 'foobar' nodes
%

% universe levels

level -> token_digits                        : mk_level_int('$1',('$1')) .
level -> token_id                            : mk_level_var('$1',('$1')) .
level -> '[' ']'                             : mk_level_seq('$1',[]) .
level -> '[' level_comma_seq ']'             : mk_level_seq('$1','$2') .
level -> '(' level '+' level ')'             : mk_level_sum('$1','$2','$4') .
level -> '(' level '|' level ')'             : mk_level_max('$1','$2','$4') .

level_comma_seq -> level                     : ['$1'] .
level_comma_seq -> level ',' level_comma_seq : ['$1'|'$3'] .

% identifiers with namespaces

id -> token_digits                       : mk_id_int('$1',('$1')) .
id -> token_id                           : mk_id_var('$1',('$1')) .
id -> token_atom                         : mk_id_etc('$1',('$1')) .

path -> '.' path                         : [mk_id_dot('$1')|'$2'] .
path -> id                               : ['$1'] .
path -> id '.' '{' level_comma_seq '}'   : ['$1'|'$4'] .
path -> id '.' path                      : ['$1'|'$3'] .

Right 10 id .
Right 20 '.' .
Right 40 '(' .
Left  30 ')' .
Right 50 '{' .
Left  60 '}' .
Right 70 '[' .
Left  80 ']' .
Right 90 ',' .
Right 100 ':=' .
Right 110 'record' .
Right 110 'define' .
Right 120 'new' .
Right 130 'case' .
Right 140 'data' .
Right 150 'let' .
Right 160 'packed' .
Right 170 '#' .
Right 180 token_arrow .
Right 190 token_forall .
Right 200 token_lambda .

path_comma_seq -> path                          : ['$1'] .
path_comma_seq -> path ',' path_comma_seq       : ['$1'|'$3'] .

%% fields of records, parameters of functions

pretype -> 'default' pretype                : mk_pretype_default('$1','$2') .
pretype -> path_comma_seq                   : mk_pretype_mk('$1','$1',[]) .
pretype -> path_comma_seq type_par_seq      : mk_pretype_mk('$1','$1','$2').

type -> pretype ':' expr                    : mk_type('$1','$1','$3').

type_par_seq -> '(' ')'                     : [] .
type_par_seq -> '(' type ')'                : ['$2'] .
type_par_seq -> '(' type ')' type_par_seq   : ['$2'|'$4'] .

preassign -> path_comma_seq                 : mk_preassign_match('$1','$1',[]) .
preassign -> path match_seq                 : mk_preassign_match('$1','$1','$2') .
preassign -> type                           : mk_preassign_type('$1','$1') .

assign -> preassign ':=' expr                   : mk_assign('$1','$1','$3') .
assign -> 'define' preassign ':=' expr          : mk_assign('$1','$2', mk_expr_define('$4','$4')) .
assign -> 'data'   preassign ':=' type_par_seq  : mk_assign('$1','$2', mk_expr_data('$4','$4')) .
assign -> 'record' preassign ':=' id_ta_par_seq : mk_assign('$1','$2', mk_expr_record('$4','$4')) .

assign_par_seq -> '(' ')'                       : [] .
assign_par_seq -> '(' assign ')'                : ['$2'] .
assign_par_seq -> '(' assign ')' assign_par_seq : ['$2'|'$4'] .

id_ta -> assign :   '$1' .
id_ta -> type   :   '$1' .

id_ta_par_seq -> '(' id_ta ')'                  : ['$2'] .
id_ta_par_seq -> '(' id_ta ')' id_ta_par_seq    : ['$2'|'$4'] .

% simple pattern matching (in function definition)

match -> path                             : mk_match_var('$1','$1') .
match -> '{'  '}'                         : mk_match_tuple('$1',[]).
match -> '{' match_comma_seq '}'          : mk_match_tuple('$1','$2').
match -> '['  ']'                         : mk_match_list('$1',[]).
match -> '[' match_comma_seq ']'          : mk_match_list('$1','$2').
match -> '[' match_comma_seq '|' path ']' : mk_match_listt('$1','$2','$4').

match_seq -> match                        : ['$1'] .
match_seq -> match match_seq              : ['$1'|'$2'] .

match_comma_seq ->   match                          : ['$1'] .
match_comma_seq ->   match ',' match_comma_seq      : ['$1'|'$3'] .

%% full pattern matching (in case statement)

pattern -> pattern_arg                                  : '$1' .
pattern -> path pattern_arg_seq                         : mk_pattern_constr('$1','$2') .

pattern_comma_seq -> pattern                            : ['$1'] .
pattern_comma_seq -> pattern ',' pattern_comma_seq      : ['$1'|'$3'] .

pattern_arg -> path                                     : mk_pattern_path('$1','$1') .
pattern_arg -> '(' pattern ')'                          : '$2' .
pattern_arg -> '{' '}'                                  : mk_pattern_tuple('$1',[]) .
pattern_arg -> '{' pattern_comma_seq '}'                : mk_pattern_tuple('$1','$2') .
pattern_arg -> '[' ']'                                  : mk_pattern_list('$1',[]) .
pattern_arg -> '[' pattern_comma_seq ']'                : mk_pattern_list('$1','$2') .
pattern_arg -> '[' pattern_comma_seq '|' path ']'       : mk_pattern_listt('$1','$2','$4') .

pattern_arg_seq -> pattern_arg                          : ['$1'].
pattern_arg_seq -> pattern_arg pattern_arg_seq          : ['$1'|'$2'].

clause -> pattern_comma_seq token_arrow expr            : mk_clause('$1','$1','$3') .

clause_par_seq -> '(' clause ')'                        : ['$2'] .
clause_par_seq -> '(' clause ')' clause_par_seq         : ['$2'|'$4'] .

expr ->     expr_                                       : ['$1'].
expr ->     expr_ expr                                  : ['$1'|'$2'] .

expr_ ->    'let' assign_par_seq 'in' expr              : mk_expr_let('$1','$2','$4') .
expr_ ->    token_forall id_ta_par_seq token_arrow expr : mk_expr_forall('$1','$2','$4') .
expr_ ->    token_lambda id_ta_par_seq token_arrow expr : mk_expr_lambda_type('$1','$2','$4') .
expr_ ->    token_lambda match_seq token_arrow expr     : mk_expr_lambda_match('$1','$2','$4') .
expr_ ->    path                                        : mk_expr_id('$1','$1') .
expr_ ->    token_id token_quoted_literal               : mk_expr_literal('$1','$1','$2') .
expr_ ->    expr token_arrow expr                       : mk_expr_arrow('$1','$1','$3') .
expr_ ->    '#' path                                    : mk_expr_external('$1','$2') .
expr_ ->    '(' expr ')'                                : '$2' .
expr_ ->    '(' ')'                                     : mk_expr_tuple('$1',[]).
expr_ ->    '{' '}'                                     : mk_expr_tuple('$1',[]) .
expr_ ->    '{' expr_comma_seq '}'                      : mk_expr_tuple('$1','$2') .
expr_ ->    '[' ']'                                     : mk_expr_list('$1',[]) .
expr_ ->    '[' expr_comma_seq ']'                      : mk_expr_list('$1','$2') .
expr_ ->    '[' expr_comma_seq '|' expr ']'             : mk_expr_listt('$1','$2','$3') .
expr_ ->    'record' '(' ')'                            : mk_expr_record('$1',[]) .
expr_ ->    'record' id_ta_par_seq                      : mk_expr_record('$1','$2') .
expr_ ->    'new' '(' ')'                               : mk_expr_new('$1',[]) .
expr_ ->    'new' id_ta_par_seq                         : mk_expr_new('$1','$2') .
expr_ ->    'data' type_par_seq                         : mk_expr_data('$1','$2') .
expr_ ->    'case' expr 'of' clause_par_seq             : mk_expr_case('$1','$2','$4') .
expr_ ->    'packed' encoding expr                      : mk_expr_packed('$1','$2','$3') .

expr_comma_seq ->     expr                          : ['$1'] .
expr_comma_seq ->     expr ',' expr_comma_seq       : ['$1'|'$3'] .

%

encoding -> '(' ')' : mk_encoding('$1') . %TODO

%TODO sugar/notation

%%%%%%%%%%%%%%%%%%%%%%%

Erlang       code.

get_line([])                 -> [];
get_line([T|_])              -> get_line(T);
get_line(T) when is_tuple(T) -> element(2, T).

get_data(T) when is_tuple(T) -> element(3, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PART 2
%
% The abstract syntax (defined by AST constructors)
%

%
% AST node format is {T,Args,Attrs}
% T - atom of type of AST-constructor
% Args - tuple of AST-constructor and args
% Attrs - proplist of extended attributes (currently just 'line')
%

exe_ast(T,L,Args) -> {T,get_line(L),Args}.

mk_level_int(L,X)           -> exe_ast(level, L, {int,get_data(X)}).
mk_level_var(L,X)           -> exe_ast(level, L, {var,get_data(X)}).
mk_level_seq(L,X)           -> exe_ast(level, L, {seq,X}).
mk_level_sum(L,X,Y)         -> exe_ast(level, L, {sum,X,Y}).
mk_level_max(L,X,Y)         -> exe_ast(level, L, {max,X,Y}).

mk_id_int(L,X)              -> exe_ast(id, L, {int,get_data(X)}).
mk_id_var(L,X)              -> exe_ast(id, L, {var,get_data(X)}).
mk_id_etc(L,X)              -> exe_ast(id, L, {etc,get_data(X)}).
mk_id_dot(L)                -> exe_ast(id, L, {dot}). % dummy id

mk_pretype_default(L,X)     -> exe_ast(pretype, L, {default,X}). % or add custom attribute?
mk_pretype_mk(L,X,Y)        -> exe_ast(pretype, L, {mk,X,Y}).

mk_type(L,X,Y)              -> exe_ast(type, L, {mk,X,Y}).

mk_preassign_type(L,X)      -> exe_ast(preassign, L, {mk_type,X}).
mk_preassign_match(L,X,Y)   -> exe_ast(preassign, L, {mk_match,X,Y}).

mk_assign(L,X,Y)            -> exe_ast(assign, L, {mk,X,Y}).

mk_match_var(L,X)           -> exe_ast(match, L, {mk_var,X}).
mk_match_tuple(L,X)         -> exe_ast(match, L, {mk_tuple,X}).
mk_match_list(L,X)          -> exe_ast(match, L, {mk_list,X}).
mk_match_listt(L,X,Y)       -> exe_ast(match, L, {mk_listt,X,Y}).

mk_pattern_constr(L,X)      -> exe_ast(pattern, L, {mk_constr,X}).
mk_pattern_path(L,X)        -> exe_ast(pattern, L, {mk_path,X}).
mk_pattern_tuple(L,X)       -> exe_ast(pattern, L, {mk_tuple,X}).
mk_pattern_list(L,X)        -> exe_ast(pattern, L, {mk_list,X}).
mk_pattern_listt(L,X,Y)     -> exe_ast(pattern, L, {mk_listt,X,Y}).

mk_clause(L,X,Y)            -> exe_ast(clause, L, {mk,X,Y}).

mk_expr_let(L,X,Y)          -> exe_ast(expr, L, {mk_let,X,Y}).
mk_expr_arrow(L,X,Y)        -> exe_ast(expr, L, {mk_arrow,X,Y}).
mk_expr_forall(L,X,Y)       -> exe_ast(expr, L, {mk_forall,X,Y}).
mk_expr_lambda_type(L,X,Y)  -> exe_ast(expr, L, {mk_lambda_type,X,Y}).
mk_expr_lambda_match(L,X,Y) -> exe_ast(expr, L, {mk_lambda_match,X,Y}).
mk_expr_define(L,X)         -> exe_ast(expr, L, {mk_define,X}).
mk_expr_record(L,X)         -> exe_ast(expr, L, {mk_record,X}).
mk_expr_new(L,X)            -> exe_ast(expr, L, {mk_new,X}).
mk_expr_data(L,X)           -> exe_ast(expr, L, {mk_data,X}).
mk_expr_case(L,X,Y)         -> exe_ast(expr, L, {mk_case,X,Y}).
mk_expr_packed(L,X,Y)       -> exe_ast(expr, L, {mk_packed,X,Y}).
mk_expr_id(L,X)             -> exe_ast(expr, L, {mk_id,X}).
mk_expr_external(L,X)       -> exe_ast(expr, L, {mk_external,X}).
mk_expr_literal(L,X,Y)      -> exe_ast(expr, L, {mk_literal,get_data(X),get_data(Y)}).
mk_expr_tuple(L,X)          -> exe_ast(expr, L, {mk_tuple,X}).
mk_expr_list(L,X)           -> exe_ast(expr, L, {mk_list,X}).
mk_expr_listt(L,X,Y)        -> exe_ast(expr, L, {mk_listt,X,Y}).

mk_encoding(L)              -> exe_ast(encoding, L, {mk}).
