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


:- object(nested).

	:- info([
		version is 0.3,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2012/09/11,
		comment is 'Nested automaton example.'
	]).

	:- public(state/2).
	:- coinductive(state/2).

	state(s0, [s0| T]) :- enter, state(s1, T).
	state(s0, [s0| T]) :- error, state(s3, T).
	state(s1, [s1| T]) :- work, state(s1, T).
	state(s1, [s1| T]) :- exit, state(s2, T).
	state(s2, [s2| T]) :- recur, state(s0, T).
	state(s3, [s3| T]) :- recur, state(s0, T).

	work.

	enter.	recur.
	exit.	error.

:- end_object.
