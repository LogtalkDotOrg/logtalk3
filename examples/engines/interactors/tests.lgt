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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/06/01,
		comment is 'Unit tests for the "interactors" example.'
	]).

	test(interactors_1) :-
		interactors::natural(N1),
		interactors::natural(N2),
		interactors::natural(N3),
		N1 == 1, N2 == 2, N3 == 3.

	test(interactors_2) :-
		interactors::prime(P1),
		interactors::prime(P2),
		interactors::prime(P3),
		P1 == 2, P2 == 3, P3 == 5.

	test(interactors_3) :-
		interactors::sums(S1),
		interactors::sums(S2),
		interactors::sums(S3),
		S1 == (0->2), S2 == (2->7), S3 == (7->9).

:- end_object.
