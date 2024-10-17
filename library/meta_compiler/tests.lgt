%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
		date is 2024-10-17,
		comment is 'Unit tests for the "meta_compiler" library.'
	]).

	cover(meta).

	% include/3 tests

	test(meta_compiler_compiler_include_3_01, deterministic(Included == [])) :-
		meta::include(integer, [], Included).

	test(meta_compiler_include_3_02, deterministic(Included == [3,2,1])) :-
		meta::include(integer, [a,3,b,2,1,c], Included).

	% exclude/3 tests

	test(meta_compiler_exclude_3_01, deterministic(Excluded == [])) :-
		meta::exclude(integer, [], Excluded).

	test(meta_compiler_exclude_3_02, deterministic(Excluded == [a,b,c])) :-
		meta::exclude(integer, [a,3,b,2,1,c], Excluded).

	% findall_member/4 tests

	test(meta_compiler_findall_member_4_01, deterministic(Result == [])) :-
		meta::findall_member(_, [], true, Result).

	test(meta_compiler_findall_member_4_02, deterministic(Result == [3,2,1])) :-
		meta::findall_member(Member, [a,3,b,2,1,c], integer(Member), Result).

	% findall_member/5 tests

	test(meta_compiler_findall_member_5_01, deterministic(Result == [0])) :-
		meta::findall_member(_, [], true, Result, [0]).

	test(meta_compiler_findall_member_5_02, deterministic(Result == [3,2,1,0])) :-
		meta::findall_member(Member, [a,3,b,2,1,c], integer(Member), Result, [0]).

	% partition/4 tests

	test(meta_compiler_partition_4_01, deterministic) :-
		meta::partition(integer, [], Included, Excluded),
		^^assertion(included, Included == []),
		^^assertion(excluded, Excluded == []).

	test(meta_compiler_partition_4_02, deterministic) :-
		meta::partition(integer, [a,3,b,2,1,c], Included, Excluded),
		^^assertion(included, Included == [3,2,1]),
		^^assertion(excluded, Excluded == [a,b,c]).

	% partition/6 tests

	test(meta_compiler_partition_6_01, deterministic) :-
		meta::partition(compare, [], 5, Less, Equal, Greater),
		^^assertion(less, Less == []),
		^^assertion(equal, Equal == []),
		^^assertion(greater, Greater == []).

	test(meta_compiler_partition_6_02, deterministic) :-
		meta::partition(compare, [7,3,4,2,1,5,6,5], 5, Less, Equal, Greater),
		^^assertion(less, Less == [3,4,2,1]),
		^^assertion(equal, Equal == [5,5]),
		^^assertion(greater, Greater == [7,6]).

	% fold_left/4 tests

	test(meta_compiler_fold_left_4_01, deterministic(Result == '_')) :-
		meta::fold_left(atom_concat, '_', [], Result).

	test(meta_compiler_fold_left_4_02, deterministic(Result == '_cab')) :-
		meta::fold_left(atom_concat, '_', [c,a,b], Result).

	% fold_left_1/3 tests

	test(meta_compiler_fold_left_1_3_01, fail) :-
		meta::fold_left_1(atom_concat, [], _).

	test(meta_compiler_fold_left_1_3_02, deterministic(Result == cab)) :-
		meta::fold_left_1(atom_concat, [c,a,b], Result).

	% scan_left/4 tests

	test(meta_compiler_scan_left_4_01, deterministic(Result == ['_'])) :-
		meta::scan_left(atom_concat, '_', [], Result).

	test(meta_compiler_scan_left_4_02, deterministic(Result == ['_','_c','_ca','_cab'])) :-
		meta::scan_left(atom_concat, '_', [c,a,b], Result).

	% scan_left_1/3 tests

	test(meta_compiler_scan_left_1_3_01, fail) :-
		meta::scan_left_1(atom_concat, [], _).

	test(meta_compiler_scan_left_1_3_02, deterministic(Result == [c,ca,cab])) :-
		meta::scan_left_1(atom_concat, [c,a,b], Result).

	% fold_right/4 tests

	test(meta_compiler_fold_right_4_01, deterministic(Result == '_')) :-
		meta::fold_right(atom_concat, '_', [], Result).

	test(meta_compiler_fold_right_4_02, deterministic(Result == 'cab_')) :-
		meta::fold_right(atom_concat, '_', [c,a,b], Result).

	% fold_right_1/3 tests

	test(meta_compiler_fold_right_1_3_01, fail) :-
		meta::fold_right_1(atom_concat, [], _).

	test(meta_compiler_fold_right_1_3_02, deterministic(Result == 'cab')) :-
		meta::fold_right_1(atom_concat, [c,a,b], Result).

	% scan_right/4 tests

	test(meta_compiler_scan_right_4_01, deterministic(Result == ['_'])) :-
		meta::scan_right(atom_concat, '_', [], Result).

	test(meta_compiler_scan_right_4_02, deterministic(Result == ['cab_','ab_','b_','_'])) :-
		meta::scan_right(atom_concat, '_', [c,a,b], Result).

	% scan_right_1/3 tests

	test(meta_compiler_scan_right_1_3_01, fail) :-
		meta::scan_right_1(atom_concat, [], _).

	test(meta_compiler_scan_right_1_3_02, deterministic(Result == ['cab','ab','b'])) :-
		meta::scan_right_1(atom_concat, [c,a,b], Result).

	% map/2 tests

	test(meta_compiler_map_2_01, true) :-
		meta::map(integer, [3,1,2]).

	test(meta_compiler_map_2_02, fail) :-
		meta::map(integer, [3,a,2]).

	% map/3 tests

	test(meta_compiler_map_3_01, true(Codes == [99,97,98])) :-
		meta::map(char_code, [c,a,b], Codes).

	test(meta_compiler_map_3_02, true(Chars == [c,a,b])) :-
		meta::map(char_code, Chars, [99,97,98]).

	% map/4 tests

	test(meta_compiler_map_4_01, true(Atoms == [a,ab,abc])) :-
		meta::map(sub_atom(abcde,_), [1,2,3], [_,_,_], Atoms).

	% map/5 tests

	test(meta_compiler_map_5_01, true(Atoms == [a,bc,cde])) :-
		meta::map(sub_atom(abcde), [0,1,2], [1,2,3], [_,_,_], Atoms).

	% map/6 tests

	test(meta_compiler_map_6_01, true(Atoms == [a,ef,lmn])) :-
		meta::map(sub_atom, [abc,defghi,jklmnopqr], [0,1,2], [1,2,3], [_,_,_], Atoms).

	% map/7 tests

	test(meta_compiler_map_7_01, true(Sums == [20,25])) :-
		meta::map(sum, [0,1], [2,3], [4,5], [6,7], [8,9], Sums).

	% map/8 tests

	test(meta_compiler_map_8_01, true(Sums == [30,36])) :-
		meta::map(sum, [0,1], [2,3], [4,5], [6,7], [8,9], [10,11], Sums).

	% map_reduce/5 tests

	test(meta_compiler_map_reduce_5_01, true(Result == cab)) :-
		meta::map_reduce([Code,Char]>>char_code(Char,Code), atom_concat, '', [99,97,98], Result).

	% test for meta-calls that cannot be optimized due to unbound arguments at compile time

	test(meta_compiler_skipped_01, true(type::valid(list(atom),Files))) :-
		% make Object only bound at runtime
		object(Object),
		meta::map(Object::loaded_file, Files).

	% auxiliary predicates

	sum(A1, A2, A3, A4, A5, Sum) :-
		Sum is A1 + A2 + A3 + A4 + A5.

	sum(A1, A2, A3, A4, A5, A6, Sum) :-
		Sum is A1 + A2 + A3 + A4 + A5 + A6.

	object(logtalk).

:- end_object.
