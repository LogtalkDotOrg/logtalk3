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


:- object(asp_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-11,
		comment is 'Whole-document Clingo and Zetesis JSON output decoding.'
	]).

	:- public(decode/8).
	:- mode(decode(+atom, +list(code), +term, +compound, +list(code), +list(compound), -compound, +list(compound)), one_or_error).
	:- info(decode/8, [
		comment is 'Decodes CLI JSON and process metadata into a collected ``asp_result(Status, Models, Properties)`` term.',
		argnames is [
			'Solver', 'Codes', 'RawStatus', 'NormalizedStatus', 'Diagnostics', 'SourceMap', 'Result', 'Options'
		],
		exceptions is [
			'``Solver`` is a variable' - instantiation_error,
			'``Solver`` is neither a variable nor an atom' - type_error(atom, 'Solver'),
			'``Solver`` is an atom but not a valid ASP solver' - domain_error(asp_solver, 'Solver'),
			'``Codes`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Codes`` is neither a partial list nor a list of character codes' - type_error(codes, 'Codes'),
			'``Codes`` does not contain parsable JSON' - domain_error(json, codes('Codes')),
			'A JSON field has an invalid type or value' - domain_error(asp_output, 'Field'-'Value'),
			'A JSON field required by the selected solver output schema is missing' - domain_error(asp_output, 'Field'-missing),
			'The JSON simultaneously reports a time limit and another interruption' - domain_error(asp_output, interruption_flags),
			'A shown value is not a supported symbol' - domain_error(asp_symbol, 'Codes'),
			'A numeric symbol cannot be represented as an integer' - representation_error(asp_integer),
			'``RawStatus`` is not a ground term' - instantiation_error,
			'``NormalizedStatus``` is not a ground term' - instantiation_error,
			'``NormalizedStatus`` is fatal or inconsistent with the decoded solver result' - domain_error(asp_process_status, 'NormalizedStatus'),
			'``Diagnostics`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Diagnostics`` is neither a partial list nor a list of character codes' - type_error(codes, 'Diagnostics'),
			'``SourceMap`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``SourceMap`` is neither a variable nor a list' - type_error(list, 'SourceMap'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- uses(list, [
		append/3, length/2, member/2, nth1/3, reverse/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	decode(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Result, Options) :-
		context(Context),
		check(ground, Solver-Options, Context),
		check(list(compound), Options, Context),
		decode_solver(Solver, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Result, Options).

	decode_solver(clingo, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Result, _) :-
		!,
		decode_clingo(Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Result).
	decode_solver(zetesis, Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Result, Options) :-
		!,
		decode_zetesis(Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Result, Options).
	decode_solver(Solver, _, _, _, _, _, _, _) :-
		domain_error(asp_solver, Solver).

	decode_clingo(Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Result) :-
		context(Context),
		check(ground, Codes-RawStatus-NormalizedStatus-Diagnostics-SourceMap, Context),
		check(codes, Codes, Context),
		check(codes, Diagnostics, Context),
		check(list, SourceMap, Context),
		json(list, dash, atom)::parse(codes(Codes), JSON),
		object_field(root, JSON),
		(	JSON = json(Pairs),
			member('Result'-Label, Pairs) ->
			(	result_status(Label, Status) ->
				true
			;	domain_error(asp_output, 'Result'-Label)
			)
		;	domain_error(asp_output, 'Result'-missing)
		),
		field('Call', JSON, [], Calls),
		list_field('Call', Calls),
		call_models(Calls, Models, []),
		field('Models', JSON, json([]), Summary),
		object_field('Models', Summary),
		field('More', Summary, unknown, More),
		(	completeness(More, Complete) ->
			true
		;	domain_error(asp_output, 'More'-More)
		),
		field('Optimum', Summary, none, Optimum),
		(	optimality(Optimum, Optimality) ->
			true
		;	domain_error(asp_output, 'Optimum'-Optimum)
		),
		field('Number', Summary, unknown, Count),
		(	Count == unknown ->
			true
		;	nonnegative_integer('Number', Count)
		),
		field('Costs', Summary, none, Costs),
		cost_field(Costs),
		interrupt_flag('TIME LIMIT', JSON, TimeLimit),
		interrupt_flag('INTERRUPTED', JSON, Interrupted),
		termination(TimeLimit, Interrupted, Termination),
		validate_status(NormalizedStatus, Status, Complete, Termination),
		(	consistent_result(Label, Status, Models, Complete, Optimality) ->
			true
		;	domain_error(asp_output, 'Result'-Label)
		),
		field('Solver', JSON, unknown, Solver),
		(	atom(Solver) ->
			true
		;	domain_error(asp_output, 'Solver'-Solver)
		),
		field('Time', JSON, none, Time),
		field('Stats', JSON, none, Statistics),
		Result = asp_result(Status, Models, [
			solver(clingo), solver_version(Solver), raw_status(RawStatus), process_status(NormalizedStatus),
			termination(Termination), enumeration_complete(Complete), optimality(Optimality),
			model_count(Count), costs(Costs), time(Time), statistics(Statistics),
			diagnostics(Diagnostics), source_map(SourceMap), raw_json(JSON)
		]).

	decode_zetesis(Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Result, Options) :-
		context(Context),
		check(ground, Codes-RawStatus-NormalizedStatus-Diagnostics-SourceMap, Context),
		check(codes, Codes, Context),
		check(codes, Diagnostics, Context),
		check(list, SourceMap, Context),
		json(list, dash, atom)::parse(codes(Codes), JSON),
		required_field(schema, JSON, Schema),
		required_field(format, JSON, Format),
		(	Schema == 3,
			Format == zetesis ->
			true
		;	domain_error(asp_output, schema-Schema)
		),
		required_field(models, JSON, Records),
		list_field(models, Records),
		zetesis_models(Records, 1, [], Models, ModelPriorities),
		required_field(outcome, JSON, Outcome),
		required_field(status, Outcome, Label),
		required_field(completion, Outcome, Completion),
		required_field(coverage, Outcome, Coverage),
		required_field(published_models, Outcome, Count),
		required_field(verified_models, Outcome, Verified),
		required_field(interruption, Outcome, Interruption),
		required_field(publication_stop, Outcome, PublicationStop),
		required_field(error, Outcome, Error),
		(	Error == '@'(null) ->
			true
		;	domain_error(asp_output, error-Error)
		),
		required_field(optimization, Outcome, Optimization),
		nonnegative_integer(published_models, Count),
		nonnegative_integer(verified_models, Verified),
		length(Models, ActualCount),
		(	ActualCount == Count ->
			true
		;	domain_error(asp_output, published_models-Count)
		),
		(	Verified >= Count ->
			true
		;	domain_error(asp_output, verified_models-Verified)
		),
		zetesis_outcome(Label, Completion, Coverage, Interruption, PublicationStop, Error, NormalizedStatus),
		zetesis_status(Label, Verified, Status),
		(	Label == satisfiable, Verified == 0 ->
			domain_error(asp_output, verified_models-Verified)
		;	true
		),
		(	Status == unsatisfiable, Verified \== 0 ->
			domain_error(asp_output, verified_models-Verified)
		;	true
		),
		zetesis_optimization(Optimization, Optimality, Costs, Priorities, Ties),
		(	Optimality == proven, (Completion \== exhausted; Status \== satisfiable) ->
			domain_error(asp_output, optimization-Optimization)
		;	true
		),
		(	integer(Ties), Ties < Count ->
			domain_error(asp_output, tied_models-Ties)
		;	true
		),
		zetesis_model_costs(Models, ModelPriorities, Costs, Priorities),
		zetesis_completeness(Completion, PublicationStop, Options, Ties, Count, Complete),
		zetesis_termination(Interruption, PublicationStop, Termination),
		(	Completion == exhausted ->
			SearchComplete = true
		;	SearchComplete = false
		),
		(	PublicationStop == '@'(null) ->
			PublicationComplete = true
		;	PublicationComplete = false
		),
		field(statistics, JSON, '@'(null), RawStatistics),
		(	RawStatistics == '@'(null) ->
			Statistics = none
		;	object_field(statistics, RawStatistics),
			Statistics = RawStatistics
		),
		Result = asp_result(Status, Models, [
			solver(zetesis), solver_version(unknown), raw_status(RawStatus), process_status(NormalizedStatus),
			termination(Termination), enumeration_complete(Complete), optimality(Optimality),
			model_count(Count), verified_model_count(Verified), optimal_tie_count(Ties),
			costs(Costs), cost_priorities(Priorities), time(none), statistics(Statistics),
			search_complete(SearchComplete), publication_complete(PublicationComplete),
			completion(Completion), coverage(Coverage), interruption(Interruption), publication_stop(PublicationStop),
			diagnostics(Diagnostics), source_map(SourceMap), raw_json(JSON)
		]).

	required_field(Key, Object, Value) :-
		object_field(Key, Object),
		Object = json(Pairs),
		(	member(Key-Value, Pairs) ->
			true
		;	domain_error(asp_output, Key-missing)
		).

	zetesis_models([], _, _, [], []).
	zetesis_models([Record| Records], Number, Table, [model(Symbols, Costs, Values)| Models], [Priorities| ModelPriorities]) :-
		required_field(number, Record, ActualNumber),
		(	ActualNumber == Number ->
			true
		;	domain_error(asp_output, number-ActualNumber)
		),
		required_field(model, Record, Model),
		required_field(atoms, Model, Atoms),
		list_field(atoms, Atoms),
		zetesis_atoms(Atoms, NewAtoms),
		(	NewAtoms == [] ->
			NextTable = Table
		;	append(Table, NewAtoms, NextTable)
		),
		required_field(full_model, Model, FullIndices),
		zetesis_references(FullIndices, NextTable, FullAtoms),
		required_field(shown, Model, Shown),
		required_field(atom_indices, Shown, Indices),
		zetesis_references(Indices, NextTable, ShownAtoms),
		(	forall(member(Atom, ShownAtoms), member(Atom, FullAtoms)) ->
			true
		;	domain_error(asp_output, atom_indices-Indices)
		),
		required_field(terms, Shown, Tokens),
		list_field(terms, Tokens),
		zetesis_terms(Tokens, ShownTerms),
		append(ShownAtoms, ShownTerms, Symbols),
		zetesis_spellings(Symbols, Values),
		required_field(costs, Model, RawCosts),
		zetesis_costs(RawCosts, Costs, Priorities),
		NextNumber is Number + 1,
		zetesis_models(Records, NextNumber, NextTable, Models, ModelPriorities).

	zetesis_atoms([], []).
	zetesis_atoms([Atom| Atoms], [function(Name, Arguments, Sign)| Symbols]) :-
		required_field(predicate, Atom, Name),
		required_field(sign, Atom, Sign),
		required_field(arguments, Atom, Tokens),
		list_field(arguments, Tokens),
		zetesis_terms(Tokens, Arguments),
		asp_program::symbol_codes(function(Name, Arguments, Sign), _),
		zetesis_atoms(Atoms, Symbols).

	zetesis_references(Indices, Table, Symbols) :-
		list_field(atom_indices, Indices),
		zetesis_indices(Indices, Table, Symbols).

	zetesis_indices([], _, []).
	zetesis_indices([Index| Indices], Table, [Symbol| Symbols]) :-
		(	integer(Index),
			Index >= 0,
			Position is Index + 1,
			nth1(Position, Table, Symbol) ->
			true
		;	domain_error(asp_output, atom_index-Index)
		),
		zetesis_indices(Indices, Table, Symbols).

	zetesis_terms([], []).
	zetesis_terms([Tokens| TokenLists], [Term| Terms]) :-
		(	valid(list, Tokens),
			phrase(zetesis_term(Term), Tokens) ->
			asp_program::symbol_codes(Term, _)
		;	domain_error(asp_output, term-Tokens)
		),
		zetesis_terms(TokenLists, Terms).

	zetesis_term(Term) -->
		[Token],
		{ required_field(kind, Token, Kind) },
		zetesis_token(Kind, Token, Term).

	zetesis_token(number, Token, number(Integer)) -->
		!,
		{	required_field(value, Token, Integer),
			integer(Integer)
		}.
	zetesis_token(string, Token, string(Codes)) -->
		!,
		{	required_field(value, Token, Atom),
			atom(Atom),
			atom_codes(Atom, Codes)
		}.
	zetesis_token(symbol, Token, function(Name, [], positive)) -->
		!,
		{ required_field(value, Token, Name) }.
	zetesis_token(infimum, _, infimum) -->
		!.
	zetesis_token(supremum, _, supremum) -->
		!.
	zetesis_token(function, Token, function(Name, Arguments, Sign)) -->
		!,
		{	required_field(name, Token, Name),
			required_field(sign, Token, Sign),
			required_field(arity, Token, Arity),
			nonnegative_integer(arity, Arity)
		},
		zetesis_arguments(Arity, Arguments).
	zetesis_token(tuple, Token, tuple(Terms)) -->
		{	required_field(arity, Token, Arity),
			nonnegative_integer(arity, Arity)
		},
		zetesis_arguments(Arity, Terms).

	zetesis_arguments(0, []) -->
		!.
	zetesis_arguments(Count, [Term| Terms]) -->
		zetesis_term(Term),
		{ Next is Count - 1 },
		zetesis_arguments(Next, Terms).

	zetesis_spellings([], []).
	zetesis_spellings([Symbol| Symbols], [Atom| Atoms]) :-
		asp_program::symbol_codes(Symbol, Codes),
		atom_codes(Atom, Codes),
		zetesis_spellings(Symbols, Atoms).

	zetesis_costs('@'(null), none, []) :-
		!.
	zetesis_costs(Records, Costs, Priorities) :-
		list_field(costs, Records),
		zetesis_cost_pairs(Records, Pairs),
		keysort(Pairs, Sorted),
		reverse(Sorted, Descending),
		zetesis_cost_values(Descending, Costs, Priorities).

	zetesis_cost_pairs([], []).
	zetesis_cost_pairs([Record| Records], [Priority-Value| Pairs]) :-
		required_field(priority, Record, Priority),
		required_field(value, Record, Value),
		(	integer(Priority),
			integer(Value) ->
			true
		;	domain_error(asp_output, costs-Record)
		),
		zetesis_cost_pairs(Records, Pairs),
		(	member(Priority-_, Pairs) ->
			domain_error(asp_output, priority-Priority)
		;	true
		).

	zetesis_cost_values([], [], []).
	zetesis_cost_values([Priority-Value| Pairs], [Value| Values], [Priority| Priorities]) :-
		zetesis_cost_values(Pairs, Values, Priorities).

	zetesis_optimization('@'(null), not_applicable, none, [], none) :-
		!.
	zetesis_optimization(Optimization, Optimality, Costs, Priorities, Ties) :-
		required_field(optimal, Optimization, Optimal),
		( Optimal == '@'(true) -> Optimality = proven
		; ( Optimal == '@'(false) -> Optimality = unproven ; domain_error(asp_output, optimal-Optimal) )
		),
		required_field(costs, Optimization, Records),
		zetesis_costs(Records, Costs, Priorities),
		( Costs == none -> domain_error(asp_output, costs-Records) ; true ),
		required_field(tied_models, Optimization, RawTies),
		( RawTies == '@'(null) ->
			Ties = unknown
		; nonnegative_integer(tied_models, RawTies),
			Ties = RawTies
		).

	zetesis_model_costs([], [], _, _).
	zetesis_model_costs([model(_, Actual, _)| Models], [ActualPriorities| Rest], Costs, Priorities) :-
		(	Actual == Costs,
			ActualPriorities == Priorities ->
			true
		;	domain_error(asp_output, costs-Actual)
		),
		zetesis_model_costs(Models, Rest, Costs, Priorities).

	zetesis_outcome(Label, Completion, Coverage, Interruption, PublicationStop, Error, Status) :-
		( Error == '@'(null) -> true ; domain_error(asp_output, error-Error) ),
		( Completion == exhausted -> ExpectedCoverage = exhausted
		; ( (Completion == requested_models; Completion == interrupted) -> ExpectedCoverage = partial ; domain_error(asp_output, completion-Completion) )
		),
		( Coverage == ExpectedCoverage -> true ; domain_error(asp_output, coverage-Coverage) ),
		zetesis_stop_field(interruption, Interruption),
		zetesis_stop_field(publication_stop, PublicationStop),
		( Status == exit(0),
			PublicationStop == '@'(null),
			Interruption == '@'(null),
			Completion \== interrupted,
			once((Label == satisfiable; Label == unsatisfiable)) ->
			true
		; ( Status == exit(3),
				Label == incomplete,
				(Completion == interrupted; PublicationStop \== '@'(null)) ->
			true
		  ; domain_error(asp_process_status, Status)
		  )
		),
		( Label == unsatisfiable, Completion \== exhausted -> domain_error(asp_output, status-Label) ; true ).

	zetesis_stop_field(_, '@'(null)) :-
		!.
	zetesis_stop_field(Key, Stop) :-
		required_field(code, Stop, Code),
		(	atom(Code) ->
			true
		;	domain_error(asp_output, Key-Stop)
		).

	zetesis_status(unsatisfiable, _, unsatisfiable) :-
		!.
	zetesis_status(_, Verified, Status) :-
		(	Verified > 0 ->
			Status = satisfiable
		;	Status = unknown
		).

	zetesis_completeness(Completion, Stop, Options, Ties, Count, Complete) :-
		( Completion \== exhausted -> Complete = false
		; ( Stop \== '@'(null) -> Complete = false
		  ; ( Ties == none -> Complete = true
		    ; ( Ties == unknown -> Complete = unknown
		      ; ( member(models(Limit), Options), Limit \== 0, Count < Ties -> Complete = false
		        ; ( Count == Ties -> Complete = true ; Complete = false )
		        )
		      )
		    )
		  )
		).

	zetesis_termination(Interruption, Stop, Termination) :-
		( Interruption \== '@'(null),
			required_field(code, Interruption, Code),
			Code == deadline ->
			Termination = time_limit
		; ( Stop \== '@'(null),
				required_field(code, Stop, StopCode),
				StopCode == deadline ->
				Termination = time_limit
		  ; ( Interruption == '@'(null),
					Stop == '@'(null) ->
					Termination = completed
				; Termination = interrupted
				)
		  )
		).

	field(Key, json(Pairs), Default, Value) :-
		(	member(Key-Found, Pairs) ->
			Value = Found
		;	Value = Default
		).

	object_field(Key, Object) :-
		(	Object = json(Pairs),
			valid(list(pair(atom, term)), Pairs) ->
			true
		;	domain_error(asp_output, Key-Object)
		).

	list_field(Key, List) :-
		(	valid(list, List) ->
			true
		;	domain_error(asp_output, Key-List)
		).

	nonnegative_integer(Key, Integer) :-
		(	integer(Integer),
			Integer >= 0 ->
			true
		;	domain_error(asp_output, Key-Integer)
		).

	call_models([], Models, Models).
	call_models([Call| Calls], Models, Tail) :-
		object_field('Call', Call),
		field('Witnesses', Call, [], Witnesses),
		list_field('Witnesses', Witnesses),
		witness_models(Witnesses, Models, Next),
		call_models(Calls, Next, Tail).

	witness_models([], Models, Models).
	witness_models([Witness| Witnesses], Models, Tail) :-
		object_field('Witness', Witness),
		(	Witness = json(Pairs),
			member('Value'-Values, Pairs) ->
			(	valid(list(atom), Values) ->
				true
			;	domain_error(asp_output, 'Value'-Values)
			),
			parse_values(Values, Symbols),
			field('Costs', Witness, none, Costs),
			cost_field(Costs),
			Models = [model(Symbols, Costs, Values)| Next]
		;	Models = Next
		),
		witness_models(Witnesses, Next, Tail).

	parse_values([], []).
	parse_values([Value| Values], [Symbol| Symbols]) :-
		atom_codes(Value, Codes),
		asp_symbols::parse(Codes, Symbol),
		parse_values(Values, Symbols).

	cost_field(Costs) :-
		(	Costs == none ->
			true
		;	valid(list(integer), Costs) ->
			true
		;	domain_error(asp_output, 'Costs'-Costs)
		).

	interrupt_flag(Key, JSON, Flag) :-
		field(Key, JSON, 0, Value),
		(	Value == 0 ->
			Flag = false
		;	Value == 1 ->
			Flag = true
		;	domain_error(asp_output, Key-Value)
		).

	termination(true, false, time_limit) :-
		!.
	termination(false, true, interrupted) :-
		!.
	termination(false, false, completed) :-
		!.
	termination(true, true, _) :-
		domain_error(asp_output, interruption_flags).

	result_status('SATISFIABLE', satisfiable).
	result_status('OPTIMUM FOUND', satisfiable).
	result_status('UNSATISFIABLE', unsatisfiable).
	result_status('UNKNOWN', unknown).

	completeness(no, true).
	completeness(yes, false).
	completeness(unknown, unknown).

	optimality(yes, proven).
	optimality(unknown, unproven).
	optimality(no, unproven).
	optimality(none, not_applicable).

	consistent_result(Label, Status, Models, Complete, Optimality) :-
		(	Status == satisfiable ->
			true
		;	Models == []
		),
		(	Label == 'OPTIMUM FOUND' ->
			Optimality == proven,
			Complete == true
		;	Optimality \== proven
		),
		(	Status == unsatisfiable ->
			Complete \== false
		;	true
		).

	validate_status(NormalizedStatus, Status, Complete, Termination) :-
		(	status_agrees(NormalizedStatus, Status, Complete, Termination) ->
			true
		;	domain_error(asp_process_status, NormalizedStatus)
		).

	status_agrees(exit(Code), Status, Complete, Termination) :-
		integer(Code),
		Interrupt is Code mod 2,
		Base is Code - Interrupt,
		(	Termination == completed ->
			Interrupt =:= 0
		;	Interrupt =:= 1
		),
		(	Status == satisfiable ->
			(	Base =:= 10 ->
				true
			;	Base =:= 30
			)
		;	(	Status == unsatisfiable ->
				Base =:= 20
			;	Base =:= 0
			)
		),
		(	Base >= 20 ->
			Complete \== false
		;	Complete \== true
		).
	status_agrees(unknown(_), _, _, _).
	status_agrees(killed(_), _, _, Termination) :-
		Termination \== completed.

:- end_object.
