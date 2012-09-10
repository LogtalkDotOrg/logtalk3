%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% simple example of defining an operator local to a source file
	

:- op(200, xfx, edge).	% global operator, visible within all
						% entities defined in this source file

:- object(graph).

	:- public(path/3).
	:- public((edge)/2).

	path(Start, End, [Start, End]) :-
		::(Start edge End).
	path(Start, End, [Start| Path]) :-
		::(Start edge Node),
		path(Node, End, Path).

:- end_object.


:- object(graph1,
	extends(graph)).

	a edge b.
	a edge c.
	b edge d.
	c edge d.

:- end_object.


:- object(graph2,
	extends(graph)).

	v1 edge v2.
	v1 edge v3.
	v2 edge v4.
	v3 edge v4.

:- end_object.


:- op(0, xfx, edge).	% "undefine" the operator, effectively
						% making it local to this source file
