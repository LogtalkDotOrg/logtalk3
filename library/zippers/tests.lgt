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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2020-10-20,
		comment is 'Unit tests for the "zippers" library.'
	]).

	cover(zlist).

	test(zlist_zip_at_index_4_01) :-
		\+ zlist::zip_at_index(1, [], _, _).

	test(zlist_zip_at_index_4_02) :-
		\+ zlist::zip_at_index(3, [1,2], _, _).

	test(zlist_zip_at_index_4_03) :-
		zlist::zip_at_index(3, [1,2,3,4,5], _, Current),
		Current == 3.

	test(zlist_zip_2_01) :-
		\+ zlist::zip([], _).

	test(zlist_zip_2_02) :-
		zlist::zip([1,2,3,4,5], _).

	test(zlist_zip_3_01) :-
		\+ zlist::zip([], _, _).

	test(zlist_zip_3_02) :-
		zlist::zip([1,2,3,4,5], _, First),
		First == 1.

	test(zlist_unzip_2_02) :-
		zlist::zip([1,2,3,4,5], Zipper),
		zlist::unzip(Zipper, List),
		List == [1,2,3,4,5].

	test(zlist_current_2_01) :-
		zlist::zip([1,2,3,4,5], Zipper),
		zlist::current(Zipper, Current),
		Current == 1.

	test(zlist_next_2_01) :-
		zlist::zip([1], Zipper),
		\+ zlist::next(Zipper, _).

	test(zlist_next_2_02) :-
		zlist::zip([1,2,3,4,5], Zipper0),
		zlist::next(Zipper0, Zipper1),
		zlist::current(Zipper1, Current),
		Current == 2.

	test(zlist_next_3_01) :-
		zlist::zip([1], Zipper),
		\+ zlist::next(Zipper, _, _).

	test(zlist_next_3_02) :-
		zlist::zip([1,2,3,4,5], Zipper0),
		zlist::next(Zipper0, _, Current),
		Current == 2.

	test(zlist_previous_2_01) :-
		zlist::zip([1,2,3,4,5], Zipper),
		\+ zlist::previous(Zipper, _).

	test(zlist_previous_2_02) :-
		zlist::zip([1,2,3,4,5], Zipper0),
		zlist::next(Zipper0, Zipper1),
		zlist::previous(Zipper1, Zipper2),
		zlist::current(Zipper2, Current),
		Current == 1.

	test(zlist_previous_3_01) :-
		zlist::zip([1,2,3,4,5], Zipper),
		\+ zlist::previous(Zipper, _, _).

	test(zlist_previous_3_02) :-
		zlist::zip([1,2,3,4,5], Zipper0),
		zlist::next(Zipper0, Zipper1),
		zlist::previous(Zipper1, _, Current),
		Current == 1.

	test(zlist_rewind_2_01) :-
		zlist::zip([1,2,3,4,5], Zipper0),
		zlist::next(Zipper0, Zipper1),
		zlist::next(Zipper1, Zipper2),
		zlist::rewind(Zipper2, Zipper),
		Zipper0 == Zipper.

	test(zlist_rewind_3_01) :-
		zlist::zip([1,2,3,4,5], Zipper0),
		zlist::next(Zipper0, Zipper1),
		zlist::next(Zipper1, Zipper2),
		zlist::rewind(Zipper2, Zipper, First),
		Zipper0 == Zipper,
		First == 1.

	test(zlist_forward_2_01) :-
		zlist::zip([1,2,3,4,5], Zipper0),
		zlist::forward(Zipper0, Zipper),
		zlist::current(Zipper, Current),
		Current == 5.

	test(zlist_forward_3_01) :-
		zlist::zip([1,2,3,4,5], Zipper0),
		zlist::forward(Zipper0, Zipper, Last),
		zlist::zip_at_index(5, [1,2,3,4,5], Zipper1, Last1),
		Zipper == Zipper1,
		Last == 5,
		Last == Last1.

	test(zlist_apply_2_01) :-
		zlist::zip([1,2,3,4,5], Zipper),
		zlist::apply({M}/[N]>>(M is N + 1), Zipper),
		M == 2.

	test(zlist_insert_before_3_01) :-
		zlist::zip([1,2,3,4,5], Zipper0),
		zlist::insert_before(Zipper0, 0, Zipper1),
		zlist::previous(Zipper1, Zipper2),
		zlist::current(Zipper2, Current),
		Current == 0.

	test(zlist_insert_after_3_01) :-
		zlist::zip([1,3,5,7,9], Zipper0),
		zlist::insert_after(Zipper0, 2, Zipper1),
		zlist::next(Zipper1, Zipper2),
		zlist::current(Zipper2, Current),
		Current == 2.

	test(zlist_replace_3_01) :-
		zlist::zip([1,2,3,4,5], Zipper0),
		zlist::replace(Zipper0, 0, Zipper1),
		zlist::current(Zipper1, Current),
		Current == 0.

	test(zlist_delete_and_previous_2_01) :-
		zlist::zip_at_index(3, [1,2,3,4,5], Zipper0, _),
		zlist::delete_and_previous(Zipper0, Zipper1),
		zlist::current(Zipper1, Current),
		Current == 2.

	test(zlist_delete_and_next_2_01) :-
		zlist::zip_at_index(3, [1,2,3,4,5], Zipper0, _),
		zlist::delete_and_next(Zipper0, Zipper1),
		zlist::current(Zipper1, Current),
		Current == 4.

	test(zlist_delete_and_unzip_2_01) :-
		zlist::zip_at_index(3, [1,2,3,4,5], Zipper, _),
		zlist::delete_and_unzip(Zipper, List),
		List == [1,2,4,5].

	test(zlist_delete_all_before_2_01) :-
		zlist::zip_at_index(3, [1,2,3,4,5], Zipper0, _),
		zlist::delete_all_before(Zipper0, Zipper1),
		zlist::unzip(Zipper1, List),
		List == [3,4,5].

	test(zlist_delete_all_before_and_unzip_2_01) :-
		zlist::zip_at_index(3, [1,2,3,4,5], Zipper, _),
		zlist::delete_all_before_and_unzip(Zipper, List),
		List == [3,4,5].

	test(zlist_delete_all_after_2_01) :-
		zlist::zip_at_index(3, [1,2,3,4,5], Zipper0, _),
		zlist::delete_all_after(Zipper0, Zipper1),
		zlist::unzip(Zipper1, List),
		List == [1,2,3].

	test(zlist_delete_all_after_and_unzip_2_01) :-
		zlist::zip_at_index(3, [1,2,3,4,5], Zipper, _),
		zlist::delete_all_after_and_unzip(Zipper, List),
		List == [1,2,3].

:- end_object.
