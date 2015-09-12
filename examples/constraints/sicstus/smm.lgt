%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
% distributed with SICStus Prolog 4.0.2 (November 2010)

/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : SEND+MORE=MONEY
 * Author	: Mats Carlsson
 */

%%   SEND
%%   MORE
%%  +____
%%  MONEY

:- object(smm).

	:- public([smm/2, smm_ix/2]).

	:- use_module(clpfd, [
		all_different/2, domain/3, labeling/2,
		(#=)/2, (#>)/2,
		op(700, xfx, #=), op(700, xfx, #>)
	]).

	smm(Lab, Consistency) :-
		L = [S,E,N,D,M,O,R,Y],
		domain(L, 0, 9),
		S #> 0,
		M #> 0,
		all_different(L, [consistency(Consistency)]),
						1000*S+100*E+10*N+D
			+		    1000*M+100*O+10*R+E
			#=  10000*M+1000*O+100*N+10*E+Y,
		labeling(Lab, L),
		writeq(L),
		nl.

	smm_ix(Lab, Consistency) :-
		L = [S,E,N,D,M,O,R,Y],
		domain(L, 0, 9),
		S #> 0,
		M #> 0,
		all_different(L, [consistency(Consistency)]),
		sendmory(S,E,N,D,M,O,R,Y),
		labeling(Lab, L),
		writeq(L),
		nl.

	sendmory(S,E,N,D,M,O,R,Y) +:
						1000*S+100*E+10*N+D
			+		    1000*M+100*O+10*R+E
			#=  10000*M+1000*O+100*N+10*E+Y.

:- end_object.
