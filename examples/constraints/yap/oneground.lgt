%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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
