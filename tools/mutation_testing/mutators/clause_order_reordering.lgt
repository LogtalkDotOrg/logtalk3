%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(clause_order_reordering(_Entity_, _Predicate_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-06,
		comment is 'Hook object implementing the ``clause_order_reordering`` mutator by reordering the clauses of a non-discontiguous predicate or non-terminal definition.',
		parameters is [
			'Entity' - 'Identifier of the entity being mutated.',
			'Predicate' - 'Predicate or non-terminal indicator selecting clauses to mutate.',
			'Occurrence' - '1-based clause index selecting which clause is swapped with its successor (last swaps with first).',
			'PrintMutation' - 'Boolean flag to print the original and mutated term plus source location.'
		]
	]).

	:- private(clauses_/1).
	:- dynamic(clauses_/1).
	:- private(seen_/1).
	:- dynamic(seen_/1).

	:- uses(list, [
		append/3, length/2, reverse/2
	]).

	term_expansion((Head :- Body), []) :-
		^^target_predicate((Head :- Body), _Entity_, _Predicate_),
		collect_clause((Head :- Body)),
		!.
	term_expansion((Head --> Body), []) :-
		^^target_predicate((Head --> Body), _Entity_, _Predicate_),
		collect_clause((Head --> Body)),
		!.
	term_expansion(Head, []) :-
		^^target_predicate(Head, _Entity_, _Predicate_),
		nonvar(Head),
		Head \= (:- _),
		Head \= (_ :- _),
		collect_clause(Head),
		!.
	term_expansion(end_of_file, Expanded) :-
		logtalk_load_context(entity_identifier, _Entity_),
		flush_collected_clauses(Clauses),
		select_reordered_clauses(Clauses, [end_of_file], Expanded),
		!.
	term_expansion(Term, Expanded) :-
		logtalk_load_context(entity_identifier, _Entity_),
		flush_collected_clauses(Clauses),
		(   Clauses == [] ->
			Expanded = Term
		;   select_reordered_clauses(Clauses, [Term], Expanded)
		).

	mutation(clauses(Clauses), Reordered) :-
		length(Clauses, Total),
		Total > 1,
		clause_index(1, Total, ClauseIndex),
		swap_clause_with_successor(Clauses, Total, ClauseIndex, Reordered).

	select_reordered_clauses(Clauses, Tail, Expanded) :-
		reset_seen,
		(   mutation(clauses(Clauses), Reordered),
			next_occurrence(Occurrence),
			Occurrence =:= _Occurrence_,
			^^print_mutation(_PrintMutation_, Clauses, Reordered),
			append(Reordered, Tail, Expanded)
		;   append(Clauses, Tail, Expanded)
		).

	reset :-
		retractall(clauses_(_)),
		reset_seen.

	reset_seen :-
		retractall(seen_(_)),
		assertz(seen_(0)).

	next_occurrence(Occurrence) :-
		(   retract(seen_(Previous)) ->
			true
		;   Previous = 0
		),
		Occurrence is Previous + 1,
		assertz(seen_(Occurrence)).

	collect_clause(Clause) :-
		(   retract(clauses_(Clauses0)) ->
			true
		;   Clauses0 = []
		),
		assertz(clauses_([Clause| Clauses0])).

	flush_collected_clauses(Clauses) :-
		(   retract(clauses_(ReversedClauses)) ->
			reverse(ReversedClauses, Clauses)
		;   Clauses = []
		).

	clause_index(Current, Total, Current) :-
		Current =< Total.
	clause_index(Current, Total, Index) :-
		Current < Total,
		Next is Current + 1,
		clause_index(Next, Total, Index).

	swap_clause_with_successor(Clauses, Total, ClauseIndex, Reordered) :-
		(   ClauseIndex < Total ->
			swap_with_next(Clauses, ClauseIndex, Reordered)
		;   rotate_last_to_front(Clauses, Reordered)
		).

	swap_with_next([First, Second| Rest], 1, [Second, First| Rest]) :-
		same_clause_predicate(First, Second),
		!.
	swap_with_next([Clause| Clauses], ClauseIndex, [Clause| ReorderedClauses]) :-
		ClauseIndex > 1,
		NextClauseIndex is ClauseIndex - 1,
		swap_with_next(Clauses, NextClauseIndex, ReorderedClauses).

	rotate_last_to_front(Clauses, Reordered) :-
		reverse(Clauses, [Last| ReversedClauses]),
		collect_trailing_same_predicate(ReversedClauses, Last, ReversedGroupTail, ReversedPrefix),
		reverse(ReversedGroupTail, GroupTail),
		GroupTail \= [],
		reverse(ReversedPrefix, Prefix),
		append(Prefix, [Last| GroupTail], Reordered).

	collect_trailing_same_predicate([], _Reference, [], []).
	collect_trailing_same_predicate([Clause| Clauses], Reference, [Clause| GroupTail], Prefix) :-
		same_clause_predicate(Clause, Reference),
		!,
		collect_trailing_same_predicate(Clauses, Reference, GroupTail, Prefix).
	collect_trailing_same_predicate(Prefix, _Reference, [], Prefix).

	same_clause_predicate(FirstClause, SecondClause) :-
		clause_head(FirstClause, FirstHead),
		clause_head(SecondClause, SecondHead),
		functor(FirstHead, Functor, Arity),
		functor(SecondHead, Functor, Arity).

	clause_head((Head :- _), Head) :-
		!.
	clause_head((Head --> _), Head) :-
		!.
	clause_head(Head, Head).

:- end_object.
