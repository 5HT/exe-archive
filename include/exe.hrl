-define(VERSION,"689a80").

% Typed AST (can be typechecked with Erlang in compile time)

-record(int,{i}).
-record(var,{v}).
-record(sum,{x,y}).
-record(max,{x,y}).
-record(dot,{}).
-record(default,{}).
-record(encoding,{x}).
-record(match,{x}).
-record(pattern,{x}).
-record(pretype,{x}).
-record(type,{x}).
-record(preassign,{x}).
-record(assign,{x}).
-record(expr,{x}).

-record(mk,{x,y}).
-record(mk_type,{x}).
-record(mk_match,{x,y}).
-record(mk_tuple,{t}).
-record(mk_list,{l}).
-record(mk_constr,{l}).
-record(mk_path,{l}).
-record(mk_let,{x,y}).
-record(mk_arrow,{x,y}).
-record(mk_forall,{x,y}).
-record(mk_record,{x}).
-record(mk_data,{x}).
-record(mk_new,{x}).
-record(mk_case,{x,y}).
-record(mk_packed,{x,y}).
-record(mk_external,{x}).
-record(mk_id,{x}).
-record(mk_literal,{get,put}).
