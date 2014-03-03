%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the CLP(FD) examples
% written by Markus Triska (August 2008)


:- object(puzzle).

	:- public(solve/1).

	:- use_module(clpfd, [
					op(450, xfx, ..), op(700, xfx, #=), op(700, xfx, #\=), op(700, xfx, ins),
					(#=)/2, (#\=)/2, all_different/1, (ins)/2, label/1]).

	solve([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
		Vars = [S,E,N,D,M,O,R,Y],
		Vars ins 0..9,
		all_different(Vars),
		          S*1000 + E*100 + N*10 + D +
		          M*1000 + O*100 + R*10 + E #=
		M*10000 + O*1000 + N*100 + E*10 + Y,
		M #\= 0, S #\= 0,
		label([M,O,N,E,Y]).

:- end_object.
