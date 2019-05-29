%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		date is 2019/05/29,
		comment is 'Unit tests for the "meta" library.'
	]).

	cover(meta).

	% include/3 tests

	test(meta_include_3_01, deterministic(Included == [])) :-
		meta::include(integer, [], Included).

	test(meta_include_3_02, deterministic(Included == [3,2,1])) :-
		meta::include(integer, [a,3,b,2,1,c], Included).

	% exclude/3 tests

	test(meta_exclude_3_01, deterministic(Excluded == [])) :-
		meta::exclude(integer, [], Excluded).

	test(meta_exclude_3_02, deterministic(Excluded == [a,b,c])) :-
		meta::exclude(integer, [a,3,b,2,1,c], Excluded).

	% findall_member/4 tests

	test(meta_findall_member_4_01, deterministic(Result == [])) :-
		meta::findall_member(_, [], true, Result).

	test(meta_findall_member_4_02, deterministic(Result == [3,2,1])) :-
		meta::findall_member(Member, [a,3,b,2,1,c], integer(Member), Result).

	% findall_member/5 tests

	test(meta_findall_member_5_01, deterministic(Result == [0])) :-
		meta::findall_member(_, [], true, Result, [0]).

	test(meta_findall_member_5_02, deterministic(Result == [3,2,1,0])) :-
		meta::findall_member(Member, [a,3,b,2,1,c], integer(Member), Result, [0]).

	% partition/4 tests

	test(meta_partition_4_01, deterministic) :-
		meta::partition(integer, [], Included, Excluded),
		^^assertion(included, Included == []),
		^^assertion(excluded, Excluded == []).

	test(meta_partition_4_02, deterministic) :-
		meta::partition(integer, [a,3,b,2,1,c], Included, Excluded),
		^^assertion(included, Included == [3,2,1]),
		^^assertion(excluded, Excluded == [a,b,c]).

	% partition/6 tests

	test(meta_partition_6_01, deterministic) :-
		meta::partition(compare, [], 5, Less, Equal, Greater),
		^^assertion(less, Less == []),
		^^assertion(equal, Equal == []),
		^^assertion(greater, Greater == []).

	test(meta_partition_6_02, deterministic) :-
		meta::partition(compare, [7,3,4,2,1,5,6,5], 5, Less, Equal, Greater),
		^^assertion(less, Less == [3,4,2,1]),
		^^assertion(equal, Equal == [5,5]),
		^^assertion(greater, Greater == [7,6]).

	% fold_left/4 tests

	test(meta_fold_left_4_01, deterministic(Result == '_')) :-
		meta::fold_left(atom_concat, '_', [], Result).

	test(meta_fold_left_4_02, deterministic(Result == '_cab')) :-
		meta::fold_left(atom_concat, '_', [c,a,b], Result).

	% fold_left_1/3 tests

	test(meta_fold_left_1_3_01, fail) :-
		meta::fold_left_1(atom_concat, [], _).

	test(meta_fold_left_1_3_02, deterministic(Result == cab)) :-
		meta::fold_left_1(atom_concat, [c,a,b], Result).

	% scan_left/4 tests

	test(meta_scan_left_4_01, deterministic(Result == ['_'])) :-
		meta::scan_left(atom_concat, '_', [], Result).

	test(meta_scan_left_4_02, deterministic(Result == ['_','_c','_ca','_cab'])) :-
		meta::scan_left(atom_concat, '_', [c,a,b], Result).

	% scan_left_1/3 tests

	test(meta_scan_left_1_3_01, fail) :-
		meta::scan_left_1(atom_concat, [], _).

	test(meta_scan_left_1_3_02, deterministic(Result == [c,ca,cab])) :-
		meta::scan_left_1(atom_concat, [c,a,b], Result).

	% fold_right/4 tests

	test(meta_fold_right_4_01, deterministic(Result == '_')) :-
		meta::fold_right(atom_concat, '_', [], Result).

	test(meta_fold_right_4_02, deterministic(Result == 'cab_')) :-
		meta::fold_right(atom_concat, '_', [c,a,b], Result).

	% fold_right_1/3 tests

	test(meta_fold_right_1_3_01, fail) :-
		meta::fold_right_1(atom_concat, [], _).

	test(meta_fold_right_1_3_02, deterministic(Result == 'cab')) :-
		meta::fold_right_1(atom_concat, [c,a,b], Result).

	% scan_right/4 tests

	test(meta_scan_right_4_01, deterministic(Result == ['_'])) :-
		meta::scan_right(atom_concat, '_', [], Result).

	test(meta_scan_right_4_02, deterministic(Result == ['cab_','ab_','b_','_'])) :-
		meta::scan_right(atom_concat, '_', [c,a,b], Result).

	% scan_right_1/3 tests

	test(meta_scan_right_1_3_01, fail) :-
		meta::scan_right_1(atom_concat, [], _).

	test(meta_scan_right_1_3_02, deterministic(Result == ['cab','ab','b'])) :-
		meta::scan_right_1(atom_concat, [c,a,b], Result).

	% map/2 tests

	test(meta_map_2_01, true) :-
		meta::map(integer, []).

	test(meta_map_2_02, true) :-
		meta::map(integer, [3,1,2]).

	test(meta_map_2_03, fail) :-
		meta::map(integer, [3,a,2]).

	% map/3 tests

	test(meta_map_3_01, true(Codes == [])) :-
		meta::map(char_code, [], Codes).

	test(meta_map_3_02, true(Codes == [99,97,98])) :-
		meta::map(char_code, [c,a,b], Codes).

	test(meta_map_3_03, true(Chars == [c,a,b])) :-
		meta::map(char_code, Chars, [99,97,98]).

	% map/4 tests


	% map/5 tests


	% map/6 tests


	% map/7 tests


	% map/8 tests


	% map_reduce/5 tests

:- end_object.
