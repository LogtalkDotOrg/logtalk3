%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
