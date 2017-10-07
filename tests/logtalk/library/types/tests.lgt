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
		version is 0.1,
		author is 'Paulo Moura',
		date is 2017/10/07,
		comment is 'Unit tests for the type library objects.'
	]).

	test(list_sort_4_01) :-
		list::sort(0, @<, [3,2,1], Sorted),
		Sorted == [1,2,3].

	test(list_sort_4_02) :-
		list::sort(0, @>, [1,2,3], Sorted),
		Sorted == [3,2,1].

	test(list_sort_4_03) :-
		list::sort(0, @=<, [3,2,1,2,3], Sorted),
		Sorted == [1,2,2,3,3].

	test(list_sort_4_04) :-
		list::sort(0, @>=, [1,2,3,2,1], Sorted),
		Sorted == [3,2,2,1,1].

	test(list_sort_4_05) :-
		list::sort(1, @<, [3-a,2-b,1-c], Sorted),
		Sorted == [1-c,2-b,3-a].

	test(list_sort_4_06) :-
		list::sort(1, @>, [1-c,2-b,3-a], Sorted),
		Sorted == [3-a,2-b,1-c].

	test(list_sort_4_07) :-
		list::sort(1, @=<, [3-a,2-b,1-c,2-b,3-a], Sorted),
		Sorted == [1-c,2-b,2-b,3-a,3-a].

	test(list_sort_4_08) :-
		list::sort(1, @>=, [1-c,2-b,3-a,2-b,1-c], Sorted),
		Sorted == [3-a,2-b,2-b,1-c,1-c].

	test(list_sort_4_09) :-
		list::sort(2, @<, [3-a,2-b,1-c], Sorted),
		Sorted == [3-a,2-b,1-c].

	test(list_sort_4_10) :-
		list::sort(2, @>, [1-c,2-b,3-a], Sorted),
		Sorted == [1-c,2-b,3-a].

	test(list_sort_4_11) :-
		list::sort(2, @=<, [3-a,2-b,1-c,2-b,3-a], Sorted),
		Sorted == [3-a,3-a,2-b,2-b,1-c].

	test(list_sort_4_12) :-
		list::sort(2, @>=, [1-c,2-b,3-a,2-b,1-c], Sorted),
		Sorted == [1-c,1-c,2-b,2-b,3-a].

:- end_object.
