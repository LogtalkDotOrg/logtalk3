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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-08-18,
		comment is 'Unit tests for the phrase//1 built-in method.'
	]).

	test(phrase_1_01, true) :-
		phrase(list(atom), [a,b,c]).

	test(phrase_1_02, true(Sum == 6)) :-
		phrase(list(sum(Sum)), [1,2,3]).

	% test DCG

	list(_) --> [].
	list(NonTerminal) --> phrase(NonTerminal), list(NonTerminal).

	atom --> [Element], {atom(Element)}.

	sum(Sum) --> [Sum], eos.
	sum(_), [Sum1] --> [Sum0, Element], {Sum1 is Sum0 + Element}.

:- end_object.
