%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from the CLP(FD)
% documentation by Markus Triska


:- object(oneground).

	:- public(oneground/3).

	oneground(X, Y, Z) :-
		clpfd:make_propagator(oneground(X, Y, Z), Prop),
		clpfd:init_propagator(X, Prop),
		clpfd:init_propagator(Y, Prop),
		clpfd:trigger_once(Prop).

	:- multifile(clpfd:run_propagator/2).
	clpfd:run_propagator(oneground(X, Y, Z), MState) :-
		(	integer(X) -> clpfd:kill(MState), Z = 1
		;	integer(Y) -> clpfd:kill(MState), Z = 1
		;	true
		).

:- end_object.
