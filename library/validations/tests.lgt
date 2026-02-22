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
		date is 2026-02-22,
		comment is 'Unit tests for the "validations" library.'
	]).

	cover(validation).
	cover(validation(_)).
	cover(validated).

	% "validation" type support

	test(validation_type_1_01, true) :-
		type::type(validation).

	test(validated_type_1_01, true) :-
		type::type(validated(_,_)).

	test(validated_arbitrary_1_01, true) :-
		type::arbitrary(validated(_,_)).

	% "validated" helper object tests

	test(validated_valids_2_01, true(Values == [])) :-
		validated::valids([], Values).

	test(validated_valids_2_02, true(Values == [1,2])) :-
		validation::of_valid(1, V1),
		validation::of_invalid(e1, V2),
		validation::of_valid(2, V3),
		validated::valids([V1, V2, V3], Values).

	test(validated_invalids_2_01, true(Errors == [])) :-
		validated::invalids([], Errors).

	test(validated_invalids_2_02, true(Errors == [e1,e2,e3])) :-
		validation::of_valid(1, V1),
		validation::of_invalids([e1,e2], V2),
		validation::of_invalid(e3, V3),
		validated::invalids([V1, V2, V3], Errors).

	test(validated_partition_3_01, true(Values-Errors == []-[])) :-
		validated::partition([], Values, Errors).

	test(validated_partition_3_02, true(Values-Errors == [1,2]-[e1,e2,e3])) :-
		validation::of_valid(1, V1),
		validation::of_invalids([e1,e2], V2),
		validation::of_valid(2, V3),
		validation::of_invalid(e3, V4),
		validated::partition([V1, V2, V3, V4], Values, Errors).

	test(validated_sequence_2_01, true(Validation == valid([1,2]))) :-
		validation::of_valid(1, V1),
		validation::of_valid(2, V2),
		validated::sequence([V1, V2], Validation).

	test(validated_traverse_3_01, true(Validation == invalid([not_integer(a),not_integer(b)]))) :-
		validated::traverse({validation}/[Term,V]>>(
			(	integer(Term) ->
				validation::of_valid(Term, V)
			;	validation::of_invalid(not_integer(Term), V)
			)
		), [1,a,2,b], Validation).

	test(validated_map_3_01, true(ValuesErrors == []-[])) :-
		validated::map({validation}/[Term,V]>>(validation::of_valid(Term, V)), [], ValuesErrors).

	test(validated_map_3_02, true(ValuesErrors == [1,2]-[not_integer(a),not_integer(b)])) :-
		validated::map({validation}/[Term,V]>>(
			(	integer(Term) ->
				validation::of_valid(Term, V)
			;	validation::of_invalid(not_integer(Term), V)
			)
		), [1,a,2,b], ValuesErrors).

	test(validated_map_4_01, true(Values-Errors == []-[])) :-
		validated::map({validation}/[Term,V]>>(validation::of_valid(Term, V)), [], Values, Errors).

	test(validated_map_4_02, true(Values-Errors == [1,2]-[not_integer(a),not_integer(b)])) :-
		validated::map({validation}/[Term,V]>>(
			(	integer(Term) ->
				validation::of_valid(Term, V)
			;	validation::of_invalid(not_integer(Term), V)
			)
		), [1,a,2,b], Values, Errors).

	% "validation" constructor tests

	test(validation_of_valid_2_01, true(Value == 1)) :-
		validation::of_valid(1, V),
		validation(V)::valid(Value).

	test(validation_of_invalid_2_01, true(Errors == [e])) :-
		validation::of_invalid(e, V),
		validation(V)::invalid(Errors).

	test(validation_of_invalids_2_01, true(Errors == [e1,e2])) :-
		validation::of_invalids([e1,e2], V),
		validation(V)::invalid(Errors).

	% from_goal/4 tests

	test(validation_from_goal_4_01, true(Validation == valid(1))) :-
		validation::from_goal(true, 1, e, Validation).

	test(validation_from_goal_4_02, true(Validation == invalid([e]))) :-
		validation::from_goal(fail, _, e, Validation).

	test(validation_from_goal_4_03, true(Validation == invalid([e]))) :-
		validation::from_goal(throw(err), _, e, Validation).

	% from_goal/3 tests

	test(validation_from_goal_3_01, true(Validation == valid(1))) :-
		validation::from_goal(true, 1, Validation).

	test(validation_from_goal_3_02, true(Validation == invalid([fail]))) :-
		validation::from_goal(fail, _, Validation).

	test(validation_from_goal_3_03, true(Validation == invalid([err]))) :-
		validation::from_goal(throw(err), _, Validation).

	% from_goal/2 tests

	test(validation_from_goal_2_01, true(Validation == valid(42))) :-
		validation::from_goal([X]>>(X = 42), Validation).

	test(validation_from_goal_2_02, true(Validation == invalid([fail]))) :-
		validation::from_goal([_]>>fail, Validation).

	test(validation_from_goal_2_03, true(Validation == invalid([err]))) :-
		validation::from_goal([_]>>throw(err), Validation).

	% from_generator/4 tests

	test(validation_from_generator_4_01, true) :-
		findall(V, validation::from_generator(list::member(X, [1,2]), X, e, V), [V1,V2,V3]),
		validation(V1)::is_valid,
		validation(V2)::is_valid,
		validation(V3)::is_invalid.

	test(validation_from_generator_4_02, true(r(Value1,Value2,Errors) == r(1,2,[e]))) :-
		findall(V, validation::from_generator(list::member(X, [1,2]), X, e, V), [V1,V2,V3]),
		validation(V1)::valid(Value1),
		validation(V2)::valid(Value2),
		validation(V3)::invalid(Errors).

	test(validation_from_generator_4_03, true(L == [invalid([e])])) :-
		findall(V, validation::from_generator(fail, _, e, V), L).

	% from_generator/3 tests

	test(validation_from_generator_3_01, true) :-
		findall(V, validation::from_generator(list::member(X, [1,2]), X, V), [V1,V2,V3]),
		validation(V1)::is_valid,
		validation(V2)::is_valid,
		validation(V3)::is_invalid.

	test(validation_from_generator_3_02, true(r(Value1,Value2,Errors) == r(1,2,[fail]))) :-
		findall(V, validation::from_generator(list::member(X, [1,2]), X, V), [V1,V2,V3]),
		validation(V1)::valid(Value1),
		validation(V2)::valid(Value2),
		validation(V3)::invalid(Errors).

	test(validation_from_generator_3_03, true(L == [invalid([fail])])) :-
		findall(V, validation::from_generator(fail, _, V), L).

	% from_generator/2 tests

	test(validation_from_generator_2_01, true) :-
		findall(V, validation::from_generator([X]>>(list::member(X, [1,2])), V), [V1,V2,V3]),
		validation(V1)::is_valid,
		validation(V2)::is_valid,
		validation(V3)::is_invalid.

	test(validation_from_generator_2_02, true(r(Value1,Value2,Errors) == r(1,2,[fail]))) :-
		findall(V, validation::from_generator([X]>>(list::member(X, [1,2])), V), [V1,V2,V3]),
		validation(V1)::valid(Value1),
		validation(V2)::valid(Value2),
		validation(V3)::invalid(Errors).

	test(validation_from_generator_2_03, true(L == [invalid([fail])])) :-
		findall(V, validation::from_generator([_]>>fail, V), L).

	% from_optional/3 tests

	test(validation_from_optional_3_01, true(Validation == valid(42))) :-
		validation::from_optional(optional(42), e, Validation).

	test(validation_from_optional_3_02, true(Validation == invalid([e]))) :-
		validation::from_optional(empty, e, Validation).

	% from_expected/2 tests

	test(validation_from_expected_2_01, true(Validation == valid(42))) :-
		validation::from_expected(expected(42), Validation).

	test(validation_from_expected_2_02, true(Validation == invalid([e]))) :-
		validation::from_expected(unexpected(e), Validation).

	% "validation(_)" predicate tests - is_valid/0 and is_invalid/0

	test(validation_is_valid_1_01, true) :-
		validation::of_valid(1, V),
		validation(V)::is_valid.

	test(validation_is_valid_1_02, false) :-
		validation::of_invalid(e, V),
		validation(V)::is_valid.

	test(validation_is_invalid_1_01, true) :-
		validation::of_invalid(e, V),
		validation(V)::is_invalid.

	test(validation_is_invalid_1_02, false) :-
		validation::of_valid(1, V),
		validation(V)::is_invalid.

	% if_valid/1 and if_invalid/1

	test(validation_if_valid_1_01, true(X == 2)) :-
		validation::of_valid(1, V),
		validation(V)::if_valid({X}/[Value]>>(X is Value + 1)).

	test(validation_if_valid_1_02, true) :-
		validation::of_invalid(e, V),
		validation(V)::if_valid([_]>>fail).

	test(validation_if_invalid_1_01, true(X == [e])) :-
		validation::of_invalid(e, V),
		validation(V)::if_invalid({X}/[Errors]>>(X = Errors)).

	test(validation_if_invalid_1_02, true) :-
		validation::of_valid(1, V),
		validation(V)::if_invalid([_]>>fail).

	% if_valid_or_else/2

	test(validation_if_valid_or_else_2_01, true(X == value)) :-
		validation::of_valid(1, V),
		validation(V)::if_valid_or_else({X}/[_]>>(X = value), {X}/[_]>>(X = errors)).

	test(validation_if_valid_or_else_2_02, true(X == errors)) :-
		validation::of_invalid(e, V),
		validation(V)::if_valid_or_else({X}/[_]>>(X = value), {X}/[_]>>(X = errors)).

	% valid/1 and invalid/1

	test(validation_valid_1_01, true(Value == 1)) :-
		validation::of_valid(1, V),
		validation(V)::valid(Value).

	test(validation_valid_1_02, error(existence_error(valid_value,_))) :-
		validation::of_invalid(e, V),
		validation(V)::valid(_).

	test(validation_invalid_1_01, true(Errors == [e])) :-
		validation::of_invalid(e, V),
		validation(V)::invalid(Errors).

	test(validation_invalid_1_02, error(existence_error(validation_errors,_))) :-
		validation::of_valid(1, V),
		validation(V)::invalid(_).

	% filter/3

	test(validation_filter_3_01, true(NewV == valid(2))) :-
		validation::of_valid(2, V),
		validation(V)::filter([X]>>(X > 1), too_small, NewV).

	test(validation_filter_3_02, true(NewV == invalid([too_small]))) :-
		validation::of_valid(0, V),
		validation(V)::filter([X]>>(X > 1), too_small, NewV).

	test(validation_filter_3_03, true(NewV == invalid([e]))) :-
		validation::of_invalid(e, V),
		validation(V)::filter([_]>>true, too_small, NewV).

	% map/2

	test(validation_map_2_01, true(NewV == valid(2))) :-
		validation::of_valid(1, V),
		validation(V)::map(integer::succ, NewV).

	test(validation_map_2_02, true(NewV == invalid([e]))) :-
		validation::of_invalid(e, V),
		validation(V)::map(integer::succ, NewV).

	% flat_map/2

	test(validation_flat_map_2_01, true(NewV == valid(2))) :-
		validation::of_valid(1, V),
		validation(V)::flat_map(flat_map_closure, NewV).

	test(validation_flat_map_2_02, true(NewV == invalid([e]))) :-
		validation::of_invalid(e, V),
		validation(V)::flat_map(flat_map_closure, NewV).

	% map_or_else/3

	test(validation_map_or_else_3_01, true(Value == 2)) :-
		validation::of_valid(1, V),
		validation(V)::map_or_else(integer::succ, 0, Value).

	test(validation_map_or_else_3_02, true(Value == 0)) :-
		validation::of_invalid(e, V),
		validation(V)::map_or_else(integer::succ, 0, Value).

	% map_catching/2

	test(validation_map_catching_2_01, true(NewV == valid(2))) :-
		validation::of_valid(1, V),
		validation(V)::map_catching(integer::succ, NewV).

	test(validation_map_catching_2_02, true(NewV == invalid([my_error]))) :-
		validation::of_valid(1, V),
		validation(V)::map_catching([_,_]>>throw(my_error), NewV).

	test(validation_map_catching_2_03, true(NewV == invalid([fail]))) :-
		validation::of_valid(1, V),
		validation(V)::map_catching([_,_]>>fail, NewV).

	test(validation_map_catching_2_04, true(NewV == invalid([e]))) :-
		validation::of_invalid(e, V),
		validation(V)::map_catching(integer::succ, NewV).

	% map_invalid/2

	test(validation_map_invalid_2_01, true(NewV == invalid([e1_mapped,e2_mapped]))) :-
		validation::of_invalids([e1,e2], V),
		validation(V)::map_invalid(meta::map([E,N]>>atom_concat(E,'_mapped',N)), NewV).

	test(validation_map_invalid_2_02, true(NewV == valid(1))) :-
		validation::of_valid(1, V),
		validation(V)::map_invalid([_,_]>>fail, NewV).

	% map_both/3

	test(validation_map_both_3_01, true(NewV == valid(2))) :-
		validation::of_valid(1, V),
		validation(V)::map_both(integer::succ, [_,_]>>fail, NewV).

	test(validation_map_both_3_02, true(NewV == invalid([wrapped(e1),wrapped(e2)]))) :-
		validation::of_invalids([e1,e2], V),
		validation(V)::map_both([_,_]>>fail, map_wrap_errors, NewV).

	test(validation_map_both_3_03, true(NewV == valid(1))) :-
		validation::of_valid(1, V),
		validation(V)::map_both([_,_]>>fail, [_,_]>>fail, NewV).

	test(validation_map_both_3_04, true(NewV == invalid([e]))) :-
		validation::of_invalid(e, V),
		validation(V)::map_both([_,_]>>fail, [_,_]>>fail, NewV).

	% swap/1

	test(validation_swap_1_01, true(NewV == invalid([1]))) :-
		validation::of_valid(1, V),
		validation(V)::swap(NewV).

	test(validation_swap_1_02, true(NewV == valid([e1,e2]))) :-
		validation::of_invalids([e1,e2], V),
		validation(V)::swap(NewV).

	% or/2

	test(validation_or_2_01, true(NewV == valid(1))) :-
		validation::of_valid(1, V),
		validation(V)::or(NewV, [_]>>fail).

	test(validation_or_2_02, true(NewV == valid(2))) :-
		validation::of_invalid(e, V),
		validation(V)::or(NewV, [N]>>(N = valid(2))).

	test(validation_or_2_03, false) :-
		validation::of_invalid(e, V),
		validation(V)::or(_, [_]>>fail).

	% or_else/2

	test(validation_or_else_2_01, true(Value == 1)) :-
		validation::of_valid(1, V),
		validation(V)::or_else(Value, 0).

	test(validation_or_else_2_02, true(Value == 0)) :-
		validation::of_invalid(e, V),
		validation(V)::or_else(Value, 0).

	% or_else_get/2

	test(validation_or_else_get_2_01, true(Value == 1)) :-
		validation::of_valid(1, V),
		validation(V)::or_else_get(Value, [_]>>fail).

	test(validation_or_else_get_2_02, true(Value == 42)) :-
		validation::of_invalid(e, V),
		validation(V)::or_else_get(Value, [X]>>(X = 42)).

	test(validation_or_else_get_2_03, error(existence_error(valid_value,_))) :-
		validation::of_invalid(e, V),
		validation(V)::or_else_get(_, [_]>>fail).

	% or_else_call/2

	test(validation_or_else_call_2_01, true(Value == 1)) :-
		validation::of_valid(1, V),
		validation(V)::or_else_call(Value, fail).

	test(validation_or_else_call_2_02, true(X == called)) :-
		validation::of_invalid(e, V),
		validation(V)::or_else_call(_, X = called).

	% or_else_fail/1

	test(validation_or_else_fail_1_01, true(Value == 1)) :-
		validation::of_valid(1, V),
		validation(V)::or_else_fail(Value).

	test(validation_or_else_fail_1_02, false) :-
		validation::of_invalid(e, V),
		validation(V)::or_else_fail(_).

	% or_else_throw/1

	test(validation_or_else_throw_1_01, true(Value == 1)) :-
		validation::of_valid(1, V),
		validation(V)::or_else_throw(Value).

	test(validation_or_else_throw_1_02, ball([e])) :-
		validation::of_invalid(e, V),
		validation(V)::or_else_throw(_).

	% or_else_throw/2

	test(validation_or_else_throw_2_01, true(Value == 1)) :-
		validation::of_valid(1, V),
		validation(V)::or_else_throw(Value, my_error).

	test(validation_or_else_throw_2_02, ball(my_error)) :-
		validation::of_invalid(e, V),
		validation(V)::or_else_throw(_, my_error).

	% zip/3

	test(validation_zip_3_01, true(NewV == valid(3))) :-
		validation::of_valid(1, V1),
		validation::of_valid(2, V2),
		validation(V1)::zip([X,Y,Z]>>(Z is X + Y), V2, NewV).

	test(validation_zip_3_02, true(NewV == invalid([e1,e2]))) :-
		validation::of_invalid(e1, V1),
		validation::of_invalids([e2], V2),
		validation(V1)::zip([_,_,_]>>true, V2, NewV).

	test(validation_zip_3_03, true(NewV == invalid([e]))) :-
		validation::of_invalid(e, V1),
		validation::of_valid(2, V2),
		validation(V1)::zip([_,_,_]>>true, V2, NewV).

	test(validation_zip_3_04, true(NewV == invalid([e]))) :-
		validation::of_valid(1, V1),
		validation::of_invalid(e, V2),
		validation(V1)::zip([_,_,_]>>true, V2, NewV).

	% flatten/1

	test(validation_flatten_1_01, true(NewV == valid(1))) :-
		validation::of_valid(valid(1), V),
		validation(V)::flatten(NewV).

	test(validation_flatten_1_02, true(NewV == invalid([e]))) :-
		validation::of_valid(invalid([e]), V),
		validation(V)::flatten(NewV).

	test(validation_flatten_1_03, true(NewV == valid(not_a_validation))) :-
		validation::of_valid(not_a_validation, V),
		validation(V)::flatten(NewV).

	test(validation_flatten_1_04, true(NewV == invalid([e]))) :-
		validation::of_invalid(e, V),
		validation(V)::flatten(NewV).

	% to_optional/1

	test(validation_to_optional_1_01, true(Optional == optional(1))) :-
		validation::of_valid(1, V),
		validation(V)::to_optional(Optional).

	test(validation_to_optional_1_02, true(Optional == empty)) :-
		validation::of_invalid(e, V),
		validation(V)::to_optional(Optional).

	% to_expected/1

	test(validation_to_expected_1_01, true(Expected == expected(1))) :-
		validation::of_valid(1, V),
		validation(V)::to_expected(Expected).

	test(validation_to_expected_1_02, true(Expected == unexpected([e]))) :-
		validation::of_invalid(e, V),
		validation(V)::to_expected(Expected).

	% sequence/2 and traverse/3

	test(validation_sequence_2_01, true(Validation == valid([]))) :-
		validated::sequence([], Validation).

	test(validation_sequence_2_02, true(Validation == valid([1,2]))) :-
		validation::of_valid(1, V1),
		validation::of_valid(2, V2),
		validated::sequence([V1, V2], Validation).

	test(validation_sequence_2_03, true(Validation == invalid([e1,e2,e3]))) :-
		validation::of_valid(1, V1),
		validation::of_invalids([e1,e2], V2),
		validation::of_valid(2, V3),
		validation::of_invalid(e3, V4),
		validated::sequence([V1, V2, V3, V4], Validation).

	test(validation_traverse_3_01, true(Validation == valid([1,2]))) :-
		validated::traverse(traverse_closure, [1,2], Validation).

	test(validation_traverse_3_02, true(Validation == invalid([not_integer(a),not_integer(b)]))) :-
		validated::traverse(traverse_closure, [1,a,2,b], Validation).

	% "validation" type checking

	test(validation_type_checking_01, true) :-
		type::check(validation, valid(1)).

	test(validation_type_checking_02, true) :-
		type::check(validation, invalid([e])).

	test(validation_type_checking_03, ball(instantiation_error)) :-
		type::check(validation, _).

	test(validation_type_checking_04, ball(type_error(validation,12345))) :-
		type::check(validation, 12345).

	% "validated" parameterized type checking

	test(validated_type_checking_01, true) :-
		type::check(validated(integer, atom), valid(1)).

	test(validated_type_checking_02, true) :-
		type::check(validated(integer, atom), invalid([a,b])).

	test(validated_type_checking_03, ball(type_error(integer,a))) :-
		type::check(validated(integer, atom), valid(a)).

	test(validated_type_checking_04, ball(type_error(atom,1))) :-
		type::check(validated(integer, atom), invalid([a,1])).

	% "validated" QuickCheck arbitrary support

	quick_check(
		validated_arbitrary_01,
		type::arbitrary({validated(integer, atom)}, -validated(integer, atom))
	).

	% "validated" shrinker tests

	test(validated_shrinker_1_01, true) :-
		type::shrinker(validated(integer, atom)).

	test(validated_shrink_3_01, all(type::check(validated(integer, atom), Small))) :-
		type::shrink(validated(integer, atom), valid(42), Small).

	test(validated_shrink_3_02, all(type::check(validated(integer, atom), Small))) :-
		type::shrink(validated(integer, atom), invalid([abc]), Small).

	test(validated_shrink_3_03, false) :-
		type::shrink(validated(integer, atom), valid(0), _).

	test(validated_shrink_3_04, false) :-
		type::shrink(validated(integer, atom), invalid(['']), _).

	% "validated" edge case tests

	test(validated_edge_case_2_01, true) :-
		findall(T, type::edge_case(validated(integer, atom), T), Cases),
		Cases \== [].

	% auxiliary predicates

	flat_map_closure(Value, NewValidation) :-
		NewValue is Value + 1,
		validation::of_valid(NewValue, NewValidation).

	traverse_closure(Term, Validation) :-
		(	integer(Term) ->
			validation::of_valid(Term, Validation)
		;	validation::of_invalid(not_integer(Term), Validation)
		).

	map_wrap_errors([], []).
	map_wrap_errors([Error| Errors], [wrapped(Error)| Wrapped]) :-
		map_wrap_errors(Errors, Wrapped).

:- end_object.
