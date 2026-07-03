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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'Tests for the "json_patch" library.'
	]).

	:- uses(json_patch, [
		apply/3
	]).

	cover(json_patch).

	test(apply_empty_patch, deterministic(NewJSON == {foo-bar})) :-
		apply([], {foo-bar}, NewJSON).

	test(apply_add_object_member, deterministic(NewJSON == {foo-bar, baz-qux})) :-
		Patch = [{op-add, path-'/baz', value-qux}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_add_array_insert, deterministic(NewJSON == {items-[a, b, c]})) :-
		Patch = [{op-add, path-'/items/1', value-b}],
		apply(Patch, {items-[a, c]}, NewJSON).

	test(apply_add_array_append, deterministic(NewJSON == {items-[a, b]})) :-
		Patch = [{op-add, path-'/items/-', value-b}],
		apply(Patch, {items-[a]}, NewJSON).

	test(apply_add_root_replaces_document, deterministic(NewJSON == {bar-baz})) :-
		Patch = [{op-add, path-'', value-{bar-baz}}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_replace_member, deterministic(NewJSON == {foo-baz})) :-
		Patch = [{op-replace, path-'/foo', value-baz}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_replace_root, deterministic(NewJSON == [1, 2])) :-
		Patch = [{op-replace, path-'', value-[1, 2]}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_remove_member, deterministic(NewJSON == {})) :-
		Patch = [{op-remove, path-'/foo'}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_remove_root_fails, fail) :-
		Patch = [{op-remove, path-''}],
		apply(Patch, {foo-bar}, _).

	test(apply_copy_member, deterministic(NewJSON == {foo-bar, baz-bar})) :-
		Patch = [{op-copy, from-'/foo', path-'/baz'}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_copy_to_root, deterministic(NewJSON == bar)) :-
		Patch = [{op-copy, from-'/foo', path-''}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_copy_codes_and_chars_pointers, deterministic(NewJSON == {foo-bar, baz-bar})) :-
		Patch = [{op-copy, from-codes([0'/, 0'f, 0'o, 0'o]), path-chars(['/', b, a, z])}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_move_member, deterministic(NewJSON == {baz-bar})) :-
		Patch = [{op-move, from-'/foo', path-'/baz'}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_move_array_element, deterministic(NewJSON == {foo-[all, cows, eat, grass]})) :-
		Patch = [{op-move, from-'/foo/1', path-'/foo/3'}],
		apply(Patch, {foo-[all, grass, cows, eat]}, NewJSON).

	test(apply_move_into_child_fails, fail) :-
		Patch = [{op-move, from-'/0', path-'/0/0'}],
		apply(Patch, [[1], []], _).

	test(apply_move_to_root, deterministic(NewJSON == bar)) :-
		Patch = [{op-move, from-'/foo', path-''}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_test_member, deterministic(NewJSON == {flag- @true})) :-
		Patch = [{op-test, path-'/flag', value- @true}],
		apply(Patch, {flag- @true}, NewJSON).

	test(apply_test_root, deterministic(NewJSON == {foo-bar})) :-
		Patch = [{op-test, path-'', value-{foo-bar}}],
		apply(Patch, {foo-bar}, NewJSON).

	test(apply_test_semantic_object_equality, deterministic(NewJSON == {foo-json([bar=1])})) :-
		Patch = [{op-test, path-'/foo', value-{bar-1}}],
		apply(Patch, {foo-json([bar=1])}, NewJSON).

	test(apply_list_object_member, deterministic(NewJSON == json([foo=1, bar=2]))) :-
		Patch = [{op-add, path-'/bar', value-2}],
		apply(Patch, json([foo=1]), NewJSON).

	test(apply_list_object_colon_member, deterministic(NewJSON == json([':'(foo, 1), ':'(bar, 2)]))) :-
		Patch = [json([':'(op, add), ':'(path, '/bar'), ':'(value, 2)])],
		apply(Patch, json([':'(foo, 1)]), NewJSON).

	test(apply_preserves_chars_key_representation, deterministic(NewJSON == json([chars([f, o, o])=1, chars([b, a, r])=2]))) :-
		Patch = [{op-add, path-'/bar', value-2}],
		apply(Patch, json([chars([f, o, o])=1]), NewJSON).

	test(apply_error_invalid_patch_document, error(type_error(list, foo))) :-
		apply(foo, {foo-bar}, _).

	test(apply_error_malformed_operation, error(domain_error(json_patch_operation, {path-'/foo'}))) :-
		apply([{path-'/foo'}], {}, _).

	test(apply_missing_target_fails, fail) :-
		Patch = [{op-remove, path-'/missing'}],
		apply(Patch, {foo-bar}, _).

	test(apply_failed_test_fails, fail) :-
		Patch = [{op-test, path-'/foo', value-baz}],
		apply(Patch, {foo-bar}, _).

:- end_object.
