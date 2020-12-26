%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2020-12-26,
		comment is 'Unit tests for the "diamonds" example.'
	]).

	test(d1_01, true(Object == b1)) :-
		d1::predicate_property(m, defined_in(Object)).

	test(d2_01, true(Object == d2)) :-
		d2::predicate_property(m, defined_in(Object)).

	test(d2_02, true(Object == c2)) :-
		d2::predicate_property(c2_m, defined_in(Object)).

	test(d3_01, true(Object == b3)) :-
		d3::predicate_property(b3_m, defined_in(Object)).

	test(d3_02, true(Object == c3)) :-
		d3::predicate_property(c3_m, defined_in(Object)).

	test(d3_03, true(Object == b3)) :-
		d3::predicate_property(m, defined_in(Object)).

:- end_object.
