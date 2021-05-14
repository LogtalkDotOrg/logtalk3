%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2021-05-14,
		comment is 'Unit tests for the "complements/restrict" example.'
	]).

	cover(vault).
	cover(my_vault).
	cover(hacker).

	test(complements_restrict_1, true(Category-Object == hacker-my_vault)) :-
		complements_object(Category, Object).

	test(complements_restrict_2, true(Protocol == monitoring)) :-
		conforms_to_protocol(my_vault, Protocol).

	test(complements_restrict_3, true(Protocol-Scope == monitoring-(public))) :-
		conforms_to_protocol(my_vault, Protocol, Scope).

	test(complements_restrict_4, true) :-
		^^suppress_text_output,
		my_vault::open('!"#$%&/()=').

	test(complements_restrict_5, true) :-
		^^suppress_text_output,
		\+ my_vault::open('1234567890').

:- end_object.
