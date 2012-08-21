%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the examples
% found on the B-Prolog 7.1 documentation (August 2008)

:- object(puzzle).

	:- public(solve/1).

	solve(Vars) :-
		Vars=[S,E,N,D,M,O,R,Y],	% variable generation
		Vars in 0..9,
		alldifferent(Vars),		% constraint generation
		S #\= 0,
		M #\= 0,
			1000*S+100*E+10*N+D
		+	1000*M+100*O+10*R+E
		#= 10000*M+1000*O+100*N+10*E+Y,
		labeling(Vars).			% labeling

:- end_object.
