%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(mutator_common,
	implements(mutator_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-07,
		comment is 'Mutator common predicate utilities.'
	]).

	:- protected(print_mutation/3).
	:- mode(print_mutation(+boolean, @callable, @callable), one).
	:- info(print_mutation/3, [
		comment is 'Prints a term and its mutation when ``Flag`` is true. Succeeds otherwise',
		argnames is ['Flag', 'Original', 'Mutation']
	]).

	print_mutation(true, Original, Mutation) :-
		self(Self),
		functor(Self, Mutator, _),
		logtalk_load_context(variable_names(Original), Variables),
		logtalk_load_context(file, File),
		logtalk_load_context(term_position, StartLine-EndLine),
		logtalk::print_message(information, mutation_testing, mutated_term(Mutator, Original, Mutation, Variables, File, StartLine-EndLine)).
	print_mutation(false, _Original, _Mutation).

	:- protected(target_predicate/3).
	:- mode(target_predicate(@callable, @entity_identifier, @predicate_indicator), one).
	:- info(target_predicate/3, [
		comment is 'True iff ``Term`` is a candidate for mutation.',
		argnames is ['Term', 'Entity', 'Predicate']
	]).

	:- protected(target_predicate_clause_index/4).
	:- mode(target_predicate_clause_index(@callable, @entity_identifier, @predicate_indicator, -integer), zero_or_one).
	:- info(target_predicate_clause_index/4, [
		comment is 'True iff ``Term`` is a candidate for mutation while also returning its current 1-based contiguous clause index for the matching predicate or non-terminal.',
		argnames is ['Term', 'Entity', 'Predicate', 'ClauseIndex']
	]).

	:- private(current_predicate_clause_index_/2).
	:- dynamic(current_predicate_clause_index_/2).

	:- private(update_target_predicate_clause_index_/2).
	:- mode(update_target_predicate_clause_index_(@predicate_indicator, -integer), one).

	target_predicate((Head :- _Body), Entity, Predicate) :-
		!,
		target_predicate(Head, Entity, Predicate).
	target_predicate((Head --> _Body), Entity, Predicate) :-
		!,
		target_predicate(Head, Entity, Predicate).
	target_predicate(Head, Entity, Predicate) :-
		logtalk_load_context(entity_identifier, Entity),
		nonvar(Head),
		Head \= (:- _),
		once((
			Predicate = Name/Arity
		;   Predicate = Name//Arity
		)),
		functor(Head, Name, Arity).

	target_predicate_clause_index(Term, Entity, Predicate, ClauseIndex) :-
		target_predicate(Term, Entity, Predicate),
		update_target_predicate_clause_index_(Predicate, ClauseIndex).

	update_target_predicate_clause_index_(Predicate, ClauseIndex) :-
		(   ::retract(current_predicate_clause_index_(Predicate, Previous)) ->
			ClauseIndex is Previous + 1
		;   ::retractall(current_predicate_clause_index_(_, _)),
			ClauseIndex = 1
		),
		::assertz(current_predicate_clause_index_(Predicate, ClauseIndex)).

	reset :-
		::retractall(current_predicate_clause_index_(_, _)).

	% by default, mutators do not map mutation occurrences to clause numbers
	coverage_clause_mutator :-
		fail.

:- end_category.
