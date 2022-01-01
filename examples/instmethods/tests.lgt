%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		version is 1:2:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2020-12-24,
		comment is 'Unit tests for the "instmethods" example.'
	]).

	cover(root).
	cover(instance1).
	cover(instance2).
	cover(instance3).

	test(instmethods_01, true(Object == root)) :-
		instance1::predicate_property(method, defined_in(Object)).

	test(instmethods_02, true(Object == instance2)) :-
		instance2::predicate_property(method, defined_in(Object)).

	test(instmethods_03, true(Object == instance3)) :-
		instance3::predicate_property(method, defined_in(Object)).

:- end_object.
