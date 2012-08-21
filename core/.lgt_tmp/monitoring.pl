'.monitoring1._dcl'(before(_,_,_),p(p(p)),no,0).
'.monitoring1._dcl'(after(_,_,_),p(p(p)),no,0).
:-('.monitoring1._dcl'(A,B,C,D,monitoring1),'.monitoring1._dcl'(A,B,C,D)).
'.monitoring1._alias'(_,A,A).
:-(multifile(/('$lgt_current_protocol_',5))).
:-(dynamic(/('$lgt_current_protocol_',5))).
'$lgt_current_protocol_'(monitoring1,'.monitoring1.','.monitoring1._dcl','.monitoring1._alias',0).
:-(multifile(/('$lgt_loaded_file_',4))).
:-(dynamic(/('$lgt_loaded_file_',4))).
'$lgt_loaded_file_'('monitoring.lgt','/Users/pmoura/Documents/Logtalk/Logtalk 2.x/lgt3svn/core/',[],[]).
