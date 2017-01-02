%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(automaton).

	:- info([
		version is 0.2,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2012/08/17,
		comment is 'Coinduction omega-automaton example.'
	]).

	:- public(automaton/2).
	:- coinductive(automaton/2).

	automaton(State, [Input| Inputs]) :-
		trans(State, Input, NewState),
		automaton(NewState, Inputs).
%	automata(State, []) :-		% we drop the base case in order
%		final(State).			% to get an omega-automaton

	trans(s0, a, s1).
	trans(s1, b, s2).
	trans(s2, c, s3).
	trans(s2, e, s0).
	trans(s3, d, s0).

	final(s2).

:- end_object.
