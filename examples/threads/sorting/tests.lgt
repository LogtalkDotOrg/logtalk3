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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/08/13,
		comment is 'Unit tests for the "threads/sorting" example.'
	]).

	cover(generator).
	cover(msort(_)).
	cover(qsort(_)).

	test(sorting_1) :-
		generator::list(20000, List),
		msort(1)::msort(List, _Sorted).

	test(sorting_2) :-
		generator::list(20000, List),
		msort(2)::msort(List, _Sorted).

	test(sorting_3) :-
		generator::list(20000, List),
		msort(4)::msort(List, _Sorted).

	test(sorting_4) :-
		generator::list(20000, List),
		qsort(1)::qsort(List, _Sorted).

	test(sorting_5) :-
		generator::list(20000, List),
		qsort(2)::qsort(List, _Sorted).

	test(sorting_6) :-
		generator::list(20000, List),
		qsort(4)::qsort(List, _Sorted).

:- end_object.
