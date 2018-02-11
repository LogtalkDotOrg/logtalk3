%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the CLP(FD) examples
% written by Markus Triska (August 2008)


:- object(puzzle).

	:- public(solve/1).

	:- use_module(clpfd, [
		op(450, xfx, ..), op(700, xfx, #=), op(700, xfx, #\=), op(700, xfx, ins),
		(#=)/2, (#\=)/2, all_different/1, (ins)/2, label/1
	]).

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
