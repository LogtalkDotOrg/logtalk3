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
		date is 2026-03-06,
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

	% by default, reset/0 does nothing
	reset.

:- end_category.
