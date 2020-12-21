%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- set_prolog_flag(double_quotes, codes).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2020-12-21,
		comment is 'Unit tests for the "bench" example.'
	]).

	test(mu_01, true(Result == [[3,m,u,i,i,u],[3,m,u,i,i,i,i,i],[2,m,i,i,i,i,i,i,i,i],[2,m,i,i,i,i],[2,m,i,i],[a,m,i]])) :-
		mu::theorem([m,u,i,i,u], 5, Result).

	test(reducer_01, true(Result == 6)) :-
		reducer::try(fac(3), Result).

	test(reducer_02, true(Result == [1,2,3])) :-
		reducer::try(quick([3,1,2]), Result).

	test(serialise_01, true(Result == [2,3,6,4,1,9,2,8,1,5,1,4,7,4,1,5,1,8,2,9,1,4,6,3,2])) :-
		atom_codes('ABLE WAS I ERE I SAW ELBA', Codes),
		serialise::serialise(Codes, Result).

	test(tak_01, true(Result == 7)) :-
		tak::tak(18, 12, 6, Result).

:- end_object.
