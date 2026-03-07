%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(head_arguments_mutation(_Entity_, _Predicate_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-06,
		comment is 'Hook object implementing the ``head_arguments_mutation`` mutator by mutating one compile-time bound predicate/non-terminal head argument using the ``type::mutation/3`` predicate.',
		parameters is [
			'Entity' - 'Identifier of the entity being mutated.',
			'Predicate' - 'Predicate or non-terminal indicator selecting clauses to mutate.',
			'Occurrence' - '1-based mutation occurrence index selecting one compile-time bound head argument to mutate.',
			'PrintMutation' - 'Boolean flag to print the original and mutated term plus source location.'
		]
	]).

	:- private(seen_/1).
	:- dynamic(seen_/1).

	term_expansion(Term, Mutation) :-
		^^target_predicate(Term, _Entity_, _Predicate_),
		mutation(Term, Mutation),
		next_occurrence(Occurrence),
		Occurrence =:= _Occurrence_,
		^^print_mutation(_PrintMutation_, Term, Mutation).

	mutation((Head :- Body), (MutatedHead :- Body)) :-
		!,
		mutate_head_arguments(Head, MutatedHead).
	mutation((Head --> Body), (MutatedHead --> Body)) :-
		!,
		mutate_head_arguments(Head, MutatedHead).
	mutation(Head, MutatedHead) :-
		mutate_head_arguments(Head, MutatedHead).

	reset :-
		retractall(seen_(_)),
		assertz(seen_(0)).

	next_occurrence(Occurrence) :-
		(   retract(seen_(Previous)) ->
			true
		;   Previous = 0
		),
		Occurrence is Previous + 1,
		assertz(seen_(Occurrence)).

	mutate_head_arguments(Head, MutatedHead) :-
		Head =.. [Functor| Arguments],
		mutate_head_arguments_list(Arguments, MutatedArguments),
		MutatedHead =.. [Functor| MutatedArguments].

	mutate_head_arguments_list([Argument| Arguments], [MutatedArgument| Arguments]) :-
		mutable_head_argument(Argument, Type),
		type::mutation(Type, Argument, MutatedArgument).
	mutate_head_arguments_list([Argument| Arguments], [Argument| MutatedArguments]) :-
		mutate_head_arguments_list(Arguments, MutatedArguments).

	mutable_head_argument(Argument, atom) :-
		atom(Argument),
		!.
	mutable_head_argument(Argument, integer) :-
		integer(Argument),
		!.
	mutable_head_argument(Argument, float) :-
		float(Argument),
		!.
	mutable_head_argument(Argument, list) :-
		nonvar(Argument),
		(	Argument = []
		;	Argument = [_| _]
		),
		!.
	mutable_head_argument(Argument, compound) :-
		compound(Argument).

:- end_object.
