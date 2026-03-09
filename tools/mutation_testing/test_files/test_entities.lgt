%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% most mutations are expected to trigger linter warnings
:- set_logtalk_flag(linter, off).


:- object(mt_sample).

	:- public(target/2).
	target(X, Y) :-
		(   X > 0 ->
			Y is X + 1
		;   Y is X - 1
		).

	:- public(check/1).
	check(X) :-
		target(X, Y),
		Y =\= 0.

	:- public(term_target/2).
	term_target(X, Y) :-
		X @> Y.

	:- public(term_check/0).
	term_check :-
		term_target(b, a).

	:- private(helper/1).
	helper(done).

	:- public(always_true/0).
	always_true :-
		true.

	:- public(always_false/0).
	always_false :-
		fail.

	:- public(multi_clause/1).
	multi_clause(1).
	multi_clause(2).

	:- public(multi_clause_check/0).
	multi_clause_check :-
		multi_clause(2).

	:- public(neg_multi/1).
	neg_multi(1) :-
		true.
	neg_multi(2) :-
		true.

	:- public(neg_multi_check/0).
	neg_multi_check :-
		neg_multi(2).

	:- public(disj_cmp/1).
	disj_cmp(X) :-
		X > 0 ;
		X < 0.

	:- public(disj_cmp_check/0).
	disj_cmp_check :-
		disj_cmp(1).

	:- public(arithmetic_value/1).
	arithmetic_value(Value) :-
		Value is 4 + 2 - 1.

	:- public(truth_choice/0).
	truth_choice :-
		true ;
		fail.

	:- public(pair_match/2).
	pair_match(1, 2).

	:- public(pair_match_check/0).
	pair_match_check :-
		pair_match(1, 2).

	:- public(ordered_choice/1).
	ordered_choice(first).
	ordered_choice(second).

	:- public(ordered_choice_check/1).
	ordered_choice_check(Value) :-
		once(ordered_choice(Value)).

	:- public(head_bound_atom_integer/2).
	head_bound_atom_integer(alpha, 1).

	:- public(head_bound_atom_integer_check/0).
	head_bound_atom_integer_check :-
		head_bound_atom_integer(alpha, 1).

:- end_object.


:- object(mt_other_sample).

	:- public(check/1).
	check(X) :-
		X > 0.

:- end_object.


:- object(mt_dcg_sample).

	:- public(dcg_multi//0).
	dcg_multi --> [a].
	dcg_multi --> [b].
	dcg_multi --> [c].
	dcg_multi --> [d].

	:- public(dcg_multi_check/1).
	dcg_multi_check(Token) :-
		phrase(dcg_multi, [Token]).

	:- public(dcg_guard//0).
	dcg_guard --> [N], {N > 0}.

	:- public(dcg_guard_check/0).
	dcg_guard_check :-
		phrase(dcg_guard, [1]).

	:- public(dcg_arithmetic//1).
	dcg_arithmetic(Value) --> [x], {Value is 1 + 1}.

	:- public(dcg_arithmetic_check/1).
	dcg_arithmetic_check(Value) :-
		phrase(dcg_arithmetic(Value), [x]).

	:- public(dcg_truth//0).
	dcg_truth --> [x], {true}.

	:- public(dcg_truth_check/0).
	dcg_truth_check :-
		phrase(dcg_truth, [x]).

	:- public(dcg_pair//2).
	dcg_pair(First, Second) --> [First, Second].

	:- public(dcg_pair_check/0).
	dcg_pair_check :-
		phrase(dcg_pair(a, b), [a, b]).

	:- public(dcg_ordered_choice//1).
	dcg_ordered_choice(first) --> [].
	dcg_ordered_choice(second) --> [].

	:- public(dcg_ordered_choice_check/1).
	dcg_ordered_choice_check(Value) :-
		once(phrase(dcg_ordered_choice(Value), [])).

	:- public(dcg_head_bound//1).
	dcg_head_bound(alpha) --> [a].

	:- public(dcg_head_bound_check/0).
	dcg_head_bound_check :-
		phrase(dcg_head_bound(alpha), [a]).

:- end_object.


:- object(mt_mutation_state).

	:- public(set/1).
	:- dynamic(current/1).

	set(Value) :-
		retractall(current(_)),
		assertz(current(Value)).

	:- public(get/1).
	get(Value) :-
		(   current(Value) ->
			true
		;   Value = original
		).

:- end_object.


:- object(mt_code_coverage).

	:- public(p/2).

	p(1, X) :- X > 1.
	p(2, X) :- X > 2.
	p(3, X) :- X > 3.

:- end_object.


:- object(mt_fail_insertion).

	:- public(a/0).
	a :- b, c, d.

	b.

	c.

	d.

:- end_object.


:- object(mt_clauses_reordering).

	:- public(p/1).
	p(1).
	p(2).
	p(3).

:- end_object.


:- object(mt_body_goal_negation).

	:- public(a/0).
	a :- b, c.

	b.

	c.

:- end_object.
