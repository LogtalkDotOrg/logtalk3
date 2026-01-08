%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
		date is 2024-02-05,
		comment is 'Unit tests for the "threads/sorting" example.'
	]).

	:- uses(list, [
		msort/2
	]).

	cover(generator).
	cover(msort(_)).
	cover(qsort(_)).

	test(sorting_1, true(Sorted == Sorted0)) :-
		generator::list(20000, List),
		msort(1)::msort(List, Sorted),
		msort(List, Sorted0).

	test(sorting_2, true(Sorted == Sorted0)) :-
		generator::list(20000, List),
		msort(2)::msort(List, Sorted),
		msort(List, Sorted0).

	test(sorting_3, true(Sorted == Sorted0)) :-
		generator::list(20000, List),
		msort(4)::msort(List, Sorted),
		msort(List, Sorted0).

	test(sorting_4, true(Sorted == Sorted0)) :-
		generator::list(20000, List),
		qsort(1)::qsort(List, Sorted),
		msort(List, Sorted0).

	test(sorting_5, true(Sorted == Sorted0)) :-
		generator::list(20000, List),
		qsort(2)::qsort(List, Sorted),
		msort(List, Sorted0).

	test(sorting_6, true(Sorted == Sorted0)) :-
		generator::list(20000, List),
		qsort(4)::qsort(List, Sorted),
		msort(List, Sorted0).

:- end_object.
