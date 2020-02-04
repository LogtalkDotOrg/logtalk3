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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-12-25,
		comment is 'Unit tests for the built-in blackboard predicates provided by some backend Prolog compilers.'
	]).

	test(bb_put_2_01, true) :-
		bb_put(a, 1).

	test(bb_put_2_02, true) :-
		bb_put(1, one).

	test(bb_put_2_03, true) :-
		key(atom, Key),
		bb_put(Key, 2).

	test(bb_put_2_04, true) :-
		key(integer, Key),
		bb_put(Key, two).

	test(bb_get_2_01, true(Value == 26)) :-
		bb_put(z, 26),
		bb_get(z, Value).

	test(bb_get_2_02, true(Value == 42)) :-
		key1(Key),
		bb_put(Key, 42),
		bb_get(Key, Value).

	test(bb_update_3_01, true(Old == 99)) :-
		bb_put(w1, 99),
		bb_update(w1, Old, 66).

	test(bb_update_3_02, true(New == 66)) :-
		bb_put(w2, 99),
		bb_update(w2, _, 66),
		bb_get(w2, New).

	test(bb_update_3_03, true(Old == 33)) :-
		key2(Key),
		bb_put(Key, 33),
		bb_update(Key, Old, 44).

	test(bb_update_3_04, true(New == 44)) :-
		key3(Key),
		bb_put(Key, 33),
		bb_update(Key, _, 44),
		bb_get(Key, New).

	test(bb_delete_2_01, true(Value == foo)) :-
		bb_put(dd1, foo),
		bb_delete(dd1, Value).

	test(bb_delete_2_02, true(Value == bar)) :-
		key4(Key),
		bb_put(Key, bar),
		bb_delete(Key, Value).

	cleanup :-
		bb_delete(a, _),
		bb_delete(b, _),
		bb_delete(1, _),
		bb_delete(2, _),
		bb_delete(da, _),
		bb_delete(zz, _),
		bb_delete(yy, _).

	% auxiliary predicates

	key1(da).

	key2(zz).

	key3(yy).

	key4(dd2).

	key(atom, b).
	key(integer, 2).

:- end_object.
