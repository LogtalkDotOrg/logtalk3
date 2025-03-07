%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		date is 2024-01-15,
		comment is 'Unit tests for the "lazy" example.'
	]).

	test(lazy_1) :-
		lazy::find_all(X, list::member(X,[1,2,3]), List),
		list::nth1(1, List, E1), ^^assertion(E1 == 1),
		list::nth1(2, List, E2), ^^assertion(E2 == 2),
		list::nth1(3, List, E3), ^^assertion(E3 == 3),
		\+ list::nth1(4, List, _).

	test(lazy_2) :-
		lazy::find_all(X, list::member(X,[1,2,3]), [Head| Tail]),
		^^assertion(Head == 1),
		^^assertion(var(Tail)).

:- end_object.
