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


:- object(counter).

	:- info([
		version is 0.2,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2013/03/06,
		comment is 'Module four counter coinductive example.'
	]).

	:- public(verify/0).

	:- coinductive([
		sm1/2, s0/2, s1/2, s2/2, s3/2
	]).

	verify :-
		\+ (sm1(-1, X), lists::comember(sm1, X)).

	sm1(N, [sm1| T]) :- N1 is (N + 1) mod 4, s0(N1, T), N1 >= 0.

	s0(N, [s0| T]) :- N1 is (N + 1) mod 4, s1(N1, T), N1 >= 0.

	s1(N, [s1| T]) :- N1 is (N + 1) mod 4, s2(N1, T), N1 >= 0.

	s2(N, [s2| T]) :- N1 is (N + 1) mod 4, s3(N1, T), N1 >= 0.

	s3(N, [s3| T]) :- N1 is (N + 1) mod 4, s0(N1, T), N1 >= 0.

:- end_object.
