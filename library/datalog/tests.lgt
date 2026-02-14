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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-02-13,
		comment is 'Unit tests for the datalog library.'
	]).

	cover(datalog).

	setup :-
		clear.

	cleanup :-
		clear.

	:- uses(datalog, [
		clear/0, load_program/1, rules/1, remove_rule/1, begin/0, commit/0, rollback/0, materialize/0,
		predicate_stratum/3, strata/1, query/1, query/2, update/3, explain/2, facts/1
	]).

	program([
		rule(path_base, path(X, Y), [edge(X, Y)]),
		rule(path_rec, path(X, Z), [edge(X, Y), path(Y, Z)]),
		fact(edge(a, b)),
		fact(edge(b, c)),
		fact(edge(c, d))
	]).

	test(datalog_materialize_transitive_closure_01, true(Goal == path(a, d))) :-
		program(Program),
		load_program(Program),
		query(path(a, d), Goal).

	test(datalog_incremental_insert_01, true(delta(Added, Removed) == delta([edge(c,d),path(a,d),path(b,d),path(c,d)], []))) :-
		load_program([
			rule(path_base, path(X, Y), [edge(X, Y)]),
			rule(path_rec, path(X, Z), [edge(X, Y), path(Y, Z)]),
			fact(edge(a, b)),
			fact(edge(b, c))
		]),
		update([edge(c, d)], [], delta(Added, Removed)).

	test(datalog_incremental_delete_01, true(Removed == [edge(b,c),path(a,c),path(a,d),path(b,c),path(b,d)])) :-
		program(Program),
		load_program(Program),
		update([], [edge(b, c)], delta(_Added, Removed)).

	test(datalog_explain_01, true(Proof == proof(rule(path_rec), [path(b,c),edge(a,b)]))) :-
		load_program([
			rule(path_base, path(X, Y), [edge(X, Y)]),
			rule(path_rec, path(X, Z), [edge(X, Y), path(Y, Z)]),
			fact(edge(a, b)),
			fact(edge(b, c))
		]),
		explain(path(a, c), Proof).

	test(datalog_query_01, true(Facts == [edge(a,b),edge(b,c),edge(c,d),path(a,b),path(a,c),path(a,d),path(b,c),path(b,d),path(c,d)])) :-
		program(Program),
		load_program(Program),
		facts(Facts),
		query(path(_, _)).

	test(datalog_stratified_negation_materialize_01, true(Facts == [active(a),blocked(b),person(a),person(b)])) :-
		load_program([
			rule(active_rule, active(X), [person(X), neg(blocked(X))]),
			fact(person(a)),
			fact(person(b)),
			fact(blocked(b))
		]),
		facts(Facts).

	test(datalog_stratified_negation_update_01, true(delta(Added, Removed) == delta([blocked(a)], [active(a)]))) :-
		load_program([
			rule(active_rule, active(X), [person(X), neg(blocked(X))]),
			fact(person(a)),
			fact(person(b)),
			fact(blocked(b))
		]),
		update([blocked(a)], [], delta(Added, Removed)).

	test(datalog_unsafe_negation_rule_01, error(domain_error(safe_stratified_rule, rule(unsafe, unsafe(X), [neg(flag(X))])))) :-
		load_program([
			rule(unsafe, unsafe(X), [neg(flag(X))])
		]).

	test(datalog_unstratified_program_01, error(permission_error(modify, program, unstratified_negation_cycle))) :-
		load_program([
			rule(p_rule, p, [neg(q)]),
			rule(q_rule, q, [neg(p)])
		]).

	test(datalog_remove_rule_01, true(\+ query(path(a, c)))) :-
		load_program([
			rule(path_base, path(X, Y), [edge(X, Y)]),
			rule(path_rec, path(X, Z), [edge(X, Y), path(Y, Z)]),
			fact(edge(a, b)),
			fact(edge(b, c))
		]),
		remove_rule(path_rec),
		materialize.

	test(datalog_transaction_rollback_01, true((query(path(a, c)), \+ query(path(a, d))))) :-
		load_program([
			rule(path_base, path(X, Y), [edge(X, Y)]),
			rule(path_rec, path(X, Z), [edge(X, Y), path(Y, Z)]),
			fact(edge(a, b)),
			fact(edge(b, c))
		]),
		begin,
		update([edge(c, d)], [], _),
		rollback.

	test(datalog_transaction_commit_01, true(query(path(a, d)))) :-
		load_program([
			rule(path_base, path(X, Y), [edge(X, Y)]),
			rule(path_rec, path(X, Z), [edge(X, Y), path(Y, Z)]),
			fact(edge(a, b)),
			fact(edge(b, c))
		]),
		begin,
		update([edge(c, d)], [], _),
		commit.

	test(datalog_transaction_nested_begin_01, error(permission_error(create, transaction, already_active))) :-
		load_program([
			rule(path_base, path(X, Y), [edge(X, Y)]),
			fact(edge(a, b))
		]),
		begin,
		begin.

	test(datalog_transaction_rollback_without_begin_01, error(existence_error(transaction, active))) :-
		clear,
		rollback.

	test(datalog_predicate_stratum_01, true(Stratum == 1)) :-
		load_program([
			rule(active_rule, active(X), [person(X), neg(blocked(X))]),
			fact(person(a)),
			fact(blocked(b))
		]),
		predicate_stratum(active, 1, Stratum).

	test(datalog_strata_01, true(Strata == [stratum(0, [predicate(blocked,1),predicate(person,1)]),stratum(1, [predicate(active,1)])])) :-
		load_program([
			rule(active_rule, active(X), [person(X), neg(blocked(X))]),
			fact(person(a)),
			fact(blocked(b))
		]),
		strata(Strata).

	test(datalog_aggregate_count_01, true(Facts == [node(a),node(b),node(c),degree(a,2),degree(b,1),degree(c,0),edge(a,b),edge(a,c),edge(b,c)])) :-
		load_program([
			rule(degree_rule, degree(X, N), [node(X), agg(count, Y, [edge(X, Y)], N)]),
			fact(node(a)),
			fact(node(b)),
			fact(node(c)),
			fact(edge(a, b)),
			fact(edge(a, c)),
			fact(edge(b, c))
		]),
		facts(Facts).

	test(datalog_aggregate_update_01, true(delta(Added, Removed) == delta([degree(b,2),edge(b,d)], [degree(b,1)]))) :-
		load_program([
			rule(degree_rule, degree(X, N), [node(X), agg(count, Y, [edge(X, Y)], N)]),
			fact(node(a)),
			fact(node(b)),
			fact(node(c)),
			fact(edge(a, b)),
			fact(edge(a, c)),
			fact(edge(b, c))
		]),
		update([edge(b, d)], [], delta(Added, Removed)).

	test(datalog_recursive_aggregate_cycle_01, error(permission_error(modify, program, unstratified_negation_cycle))) :-
		load_program([
			rule(rec_agg, p(X, N), [agg(count, Y, [p(X, Y)], N)])
		]).

	test(datalog_aggregate_sum_min_max_01, true(Facts == [node(a),node(b),max_score(a,20),max_score(b,9),min_score(a,10),min_score(b,7),score(a,10),score(a,15),score(a,20),score(b,7),score(b,9),sum_score(a,45),sum_score(b,16)])) :-
		load_program([
			rule(sum_rule, sum_score(X, S), [node(X), agg(sum, Y, [score(X, Y)], S)]),
			rule(min_rule, min_score(X, M), [node(X), agg(min, Y, [score(X, Y)], M)]),
			rule(max_rule, max_score(X, M), [node(X), agg(max, Y, [score(X, Y)], M)]),
			fact(node(a)),
			fact(node(b)),
			fact(score(a, 10)),
			fact(score(a, 20)),
			fact(score(a, 15)),
			fact(score(b, 7)),
			fact(score(b, 9))
		]),
		facts(Facts).

	test(datalog_aggregate_sum_update_01, true(delta(Added, Removed) == delta([max_score(a,25),score(a,25),sum_score(a,70)], [max_score(a,20),sum_score(a,45)]))) :-
		load_program([
			rule(sum_rule, sum_score(X, S), [node(X), agg(sum, Y, [score(X, Y)], S)]),
			rule(max_rule, max_score(X, M), [node(X), agg(max, Y, [score(X, Y)], M)]),
			fact(node(a)),
			fact(score(a, 10)),
			fact(score(a, 20)),
			fact(score(a, 15))
		]),
		update([score(a, 25)], [], delta(Added, Removed)).

	test(datalog_rule_body_normalization_01, variant(Body, [flag,person(X),edge(X,Y),agg(count,Y,[edge(X,Y)],N),neg(blocked(X))])) :-
		load_program([
			rule(mix_rule, target(X), [neg(blocked(X)), agg(count, Y, [edge(X, Y)], N), edge(X, Y), person(X), flag])
		]),
		rules([rule(mix_rule, target(_), Body)]).

:- end_object.
