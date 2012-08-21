'$expanding._dcl'(goal_expansion(_,_),p(p(p)),no,0).
'$expanding._dcl'(term_expansion(_,_),p(p(p)),no,0).
:-('$expanding._dcl'(A,B,C,D,expanding1),'$expanding._dcl'(A,B,C,D)).
'$expanding._alias'(_,A,A).
:-(multifile(/('$lgt_current_protocol_',5))).
:-(dynamic(/('$lgt_current_protocol_',5))).
'$lgt_current_protocol_'(expanding1,'$expanding.','$expanding._dcl','$expanding._alias',0).
:-(multifile(/('$lgt_loaded_file_',4))).
:-(dynamic(/('$lgt_loaded_file_',4))).
'$lgt_loaded_file_'('expanding.lgt','/Users/pmoura/Documents/Logtalk/Logtalk 2.x/lgt3svn/core/',[],[]).
