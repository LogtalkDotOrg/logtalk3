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


:- object(datalog,
	implements(datalog_protocol)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-02-13,
		comment is 'Portable Datalog engine with stratified negation and incremental updates.'
	]).

	:- private(rule_/3).
	:- dynamic(rule_/3).
	:- mode(rule_(?nonvar, ?callable, ?list), zero_or_more).
	:- info(rule_/3, [
		comment is 'Table of loaded rules represented as rule id, head, and body literals.',
		argnames is ['Id', 'Head', 'Body']
	]).

	:- private(edb_fact_/1).
	:- dynamic(edb_fact_/1).
	:- mode(edb_fact_(?callable), zero_or_more).
	:- info(edb_fact_/1, [
		comment is 'Table of extensional database (EDB) facts.',
		argnames is ['Fact']
	]).

	:- private(idb_fact_/1).
	:- dynamic(idb_fact_/1).
	:- mode(idb_fact_(?callable), zero_or_more).
	:- info(idb_fact_/1, [
		comment is 'Table of intensional database (IDB) currently derived facts.',
		argnames is ['Fact']
	]).

	:- private(support_count_/2).
	:- dynamic(support_count_/2).
	:- mode(support_count_(?callable, ?integer), zero_or_more).
	:- info(support_count_/2, [
		comment is 'Table of derivation support counts for currently derived facts.',
		argnames is ['Fact', 'Count']
	]).

	:- private(support_edge_/3).
	:- dynamic(support_edge_/3).
	:- mode(support_edge_(?callable, ?nonvar, ?list), zero_or_more).
	:- info(support_edge_/3, [
		comment is 'Table of concrete derivation edges as fact, rule id, and supporting literals.',
		argnames is ['Fact', 'RuleId', 'Supports']
	]).

	:- private(predicate_stratum_/3).
	:- dynamic(predicate_stratum_/3).
	:- mode(predicate_stratum_(?atom, ?integer, ?integer), zero_or_more).
	:- info(predicate_stratum_/3, [
		comment is 'Table of computed predicate strata represented as name, arity, and stratum number.',
		argnames is ['Name', 'Arity', 'Stratum']
	]).

	:- private(snapshot_/6).
	:- dynamic(snapshot_/6).
	:- mode(snapshot_(?list, ?list, ?list, ?list, ?list, ?list), zero_or_one).
	:- info(snapshot_/6, [
		comment is 'Transaction snapshot of rules, EDB facts, IDB facts, support counts, support edges, and predicate strata.',
		argnames is ['Rules', 'EdbFacts', 'IdbFacts', 'SupportCounts', 'SupportEdges', 'PredicateStrata']
	]).

	:- private(restore_snapshot/6).
	:- mode(restore_snapshot(+list, +list, +list, +list, +list, +list), one).
	:- info(restore_snapshot/6, [
		comment is 'Restores a saved transaction snapshot into the current engine state.',
		argnames is ['Rules', 'EdbFacts', 'IdbFacts', 'SupportCounts', 'SupportEdges', 'PredicateStrata']
	]).

	:- private(restore_edb_facts/1).
	:- mode(restore_edb_facts(+list(callable)), one).
	:- info(restore_edb_facts/1, [
		comment is 'Restores EDB facts from a saved list.',
		argnames is ['Facts']
	]).

	:- private(restore_idb_facts/1).
	:- mode(restore_idb_facts(+list(callable)), one).
	:- info(restore_idb_facts/1, [
		comment is 'Restores IDB facts from a saved list.',
		argnames is ['Facts']
	]).

	:- private(restore_support_counts/1).
	:- mode(restore_support_counts(+list), one).
	:- info(restore_support_counts/1, [
		comment is 'Restores support counts from a saved list.',
		argnames is ['Supports']
	]).

	:- private(restore_support_edges/1).
	:- mode(restore_support_edges(+list), one).
	:- info(restore_support_edges/1, [
		comment is 'Restores support edges from a saved list.',
		argnames is ['Edges']
	]).

	:- private(restore_predicate_strata/1).
	:- mode(restore_predicate_strata(+list), one).
	:- info(restore_predicate_strata/1, [
		comment is 'Restores predicate strata from a saved list.',
		argnames is ['Strata']
	]).

	:- private(strata_from_numbers/2).
	:- mode(strata_from_numbers(+list(integer), -list), one).
	:- info(strata_from_numbers/2, [
		comment is 'Builds grouped strata terms from a sorted list of stratum numbers.',
		argnames is ['StratumNumbers', 'Strata']
	]).

	:- private(predicates_in_stratum/2).
	:- mode(predicates_in_stratum(+integer, -list), one).
	:- info(predicates_in_stratum/2, [
		comment is 'Returns predicates in a given stratum as sorted ``predicate(Name, Arity)`` terms.',
		argnames is ['Stratum', 'Predicates']
	]).

	:- private(has_aggregate_rules/0).
	:- mode(has_aggregate_rules, zero_or_one).
	:- info(has_aggregate_rules/0, [
		comment is 'True when at least one loaded rule body contains an aggregate literal.'
	]).

	:- private(aggregate_body_predicate/2).
	:- mode(aggregate_body_predicate(+list, -compound), zero_or_more).
	:- info(aggregate_body_predicate/2, [
		comment is 'Enumerates predicate indicators referenced in aggregate goals from a body literal list.',
		argnames is ['Body', 'Predicate']
	]).

	:- private(derive_aggregate_literal/2).
	:- mode(derive_aggregate_literal(+compound, -nonvar), zero_or_one).
	:- info(derive_aggregate_literal/2, [
		comment is 'Evaluates an aggregate literal and returns a normalized support term.',
		argnames is ['AggregateLiteral', 'Support']
	]).

	:- private(derive_aggregate_goals/1).
	:- mode(derive_aggregate_goals(+list(callable)), zero_or_one).
	:- info(derive_aggregate_goals/1, [
		comment is 'Succeeds when all aggregate goals are true for the current bindings.',
		argnames is ['Goals']
	]).

	:- private(optimize_rule_body/2).
	:- mode(optimize_rule_body(+list, -list), one).
	:- info(optimize_rule_body/2, [
		comment is 'Normalizes a rule body by placing positive ground literals first, then positive non-ground literals, then aggregates, and finally negative literals.',
		argnames is ['Body', 'OptimizedBody']
	]).

	:- private(partition_body_literals/5).
	:- mode(partition_body_literals(+list, -list, -list, -list, -list), one).
	:- info(partition_body_literals/5, [
		comment is 'Partitions body literals into positive-ground, positive-non-ground, aggregate, and negative lists preserving relative order.',
		argnames is ['Body', 'PositiveGround', 'PositiveNonGround', 'Aggregates', 'Negatives']
	]).

	:- private(literal_bucket/2).
	:- mode(literal_bucket(+nonvar, -atom), one).
	:- info(literal_bucket/2, [
		comment is 'Classifies a body literal into one of the normalization buckets.',
		argnames is ['Literal', 'Bucket']
	]).

	:- uses(list, [
		append/3, memberchk/2, subtract/3, length/2, reverse/2
	]).

	:- uses(numberlist, [
		max/2, min/2, sum/2
	]).

	:- uses(type, [
		check/3
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			clear/0,
			load_program/1,
			add_rule/3,
			remove_rule/1,
			begin/0,
			commit/0,
			rollback/0,
			assert_fact/1,
			retract_fact/1,
			materialize/0,
			update/3
		]).
	:- endif.

	clear :-
		retractall(rule_(_, _, _)),
		retractall(edb_fact_(_)),
		retractall(idb_fact_(_)),
		retractall(support_count_(_, _)),
		retractall(support_edge_(_, _, _)),
		retractall(predicate_stratum_(_, _, _)),
		retractall(snapshot_(_, _, _, _, _, _)).

	load_program(Program) :-
		context(Context),
		clear,
		check(list, Program, Context),
		load_program_items(Program),
		materialize.

	load_program_items([]).
	load_program_items([Item| Items]) :-
		load_program_item(Item),
		load_program_items(Items).

	load_program_item(rule(Id, Head, Body)) :-
		add_rule(Id, Head, Body).
	load_program_item(fact(Fact)) :-
		assert_fact(Fact).
	load_program_item(Item) :-
		domain_error(datalog_program_item, Item).

	add_rule(Id, Head, Body) :-
		check_rule(Id, Head, Body),
		optimize_rule_body(Body, OptimizedBody),
		findall(rule(Id, OldHead, OldBody), rule_(Id, OldHead, OldBody), OldRules),
		retractall(rule_(Id, _, _)),
		assertz(rule_(Id, Head, OptimizedBody)),
		(	catch(ensure_stratified_program, _, fail) ->
			true
		;	retractall(rule_(Id, _, _)),
			restore_rules(OldRules),
			ensure_stratified_program,
			permission_error(modify, program, unstratified_negation_cycle)
		).

	remove_rule(Id) :-
		nonvar(Id),
		retractall(rule_(Id, _, _)),
		ensure_stratified_program,
		!.
	remove_rule(_Id) :-
		instantiation_error.

	begin :-
		(	snapshot_(_, _, _, _, _, _) ->
			permission_error(create, transaction, already_active)
		;	findall(rule(Id, Head, Body), rule_(Id, Head, Body), Rules),
			findall(Fact, edb_fact_(Fact), EdbFacts),
			findall(Fact, idb_fact_(Fact), IdbFacts),
			findall(support(Fact, Count), support_count_(Fact, Count), SupportCounts),
			findall(edge(Fact, RuleId, Supports), support_edge_(Fact, RuleId, Supports), SupportEdges),
			findall(stratum(Name, Arity, Stratum), predicate_stratum_(Name, Arity, Stratum), PredicateStrata),
			assertz(snapshot_(Rules, EdbFacts, IdbFacts, SupportCounts, SupportEdges, PredicateStrata))
		).

	commit :-
		(	retract(snapshot_(_, _, _, _, _, _)) ->
			true
		;	existence_error(transaction, active)
		).

	rollback :-
		(	retract(snapshot_(Rules, EdbFacts, IdbFacts, SupportCounts, SupportEdges, PredicateStrata)) ->
			restore_snapshot(Rules, EdbFacts, IdbFacts, SupportCounts, SupportEdges, PredicateStrata)
		;	existence_error(transaction, active)
		).

	restore_snapshot(Rules, EdbFacts, IdbFacts, SupportCounts, SupportEdges, PredicateStrata) :-
		retractall(rule_(_, _, _)),
		retractall(edb_fact_(_)),
		retractall(idb_fact_(_)),
		retractall(support_count_(_, _)),
		retractall(support_edge_(_, _, _)),
		retractall(predicate_stratum_(_, _, _)),
		restore_rules(Rules),
		restore_edb_facts(EdbFacts),
		restore_idb_facts(IdbFacts),
		restore_support_counts(SupportCounts),
		restore_support_edges(SupportEdges),
		restore_predicate_strata(PredicateStrata).

	restore_edb_facts([]).
	restore_edb_facts([Fact| Facts]) :-
		assertz(edb_fact_(Fact)),
		restore_edb_facts(Facts).

	restore_idb_facts([]).
	restore_idb_facts([Fact| Facts]) :-
		assertz(idb_fact_(Fact)),
		restore_idb_facts(Facts).

	restore_support_counts([]).
	restore_support_counts([support(Fact, Count)| Supports]) :-
		assertz(support_count_(Fact, Count)),
		restore_support_counts(Supports).

	restore_support_edges([]).
	restore_support_edges([edge(Fact, RuleId, Supports)| Edges]) :-
		assertz(support_edge_(Fact, RuleId, Supports)),
		restore_support_edges(Edges).

	restore_predicate_strata([]).
	restore_predicate_strata([stratum(Name, Arity, Stratum)| Strata]) :-
		assertz(predicate_stratum_(Name, Arity, Stratum)),
		restore_predicate_strata(Strata).

	restore_rules([]).
	restore_rules([rule(Id, Head, Body)| Rules]) :-
		assertz(rule_(Id, Head, Body)),
		restore_rules(Rules).

	check_rule(Id, Head, Body) :-
		context(Context),
		nonvar(Id),
		callable(Head),
		check(list, Body, Context),
		check_body_literals(Body),
		safe_rule(Head, Body),
		!.
	check_rule(Id, Head, Body) :-
		domain_error(safe_stratified_rule, rule(Id, Head, Body)).

	optimize_rule_body(Body, OptimizedBody) :-
		partition_body_literals(Body, PositiveGroundReversed, PositiveNonGroundReversed, AggregateReversed, NegativeReversed),
		reverse(PositiveGroundReversed, PositiveGround),
		reverse(PositiveNonGroundReversed, PositiveNonGround),
		reverse(AggregateReversed, Aggregates),
		reverse(NegativeReversed, Negatives),
		append(PositiveGround, PositiveNonGround, PositiveLiterals),
		append(PositiveLiterals, Aggregates, PositiveAndAggregateLiterals),
		append(PositiveAndAggregateLiterals, Negatives, OptimizedBody).

	partition_body_literals([], [], [], [], []).
	partition_body_literals([Literal| Literals], PositiveGround0, PositiveNonGround0, Aggregate0, Negative0) :-
		literal_bucket(Literal, Bucket),
		partition_body_literals(Literals, PositiveGround1, PositiveNonGround1, Aggregate1, Negative1),
		(	Bucket == positive_ground ->
			PositiveGround0 = [Literal| PositiveGround1],
			PositiveNonGround0 = PositiveNonGround1,
			Aggregate0 = Aggregate1,
			Negative0 = Negative1
		;	Bucket == positive_non_ground ->
			PositiveGround0 = PositiveGround1,
			PositiveNonGround0 = [Literal| PositiveNonGround1],
			Aggregate0 = Aggregate1,
			Negative0 = Negative1
		;	Bucket == aggregate ->
			PositiveGround0 = PositiveGround1,
			PositiveNonGround0 = PositiveNonGround1,
			Aggregate0 = [Literal| Aggregate1],
			Negative0 = Negative1
		; 	PositiveGround0 = PositiveGround1,
			PositiveNonGround0 = PositiveNonGround1,
			Aggregate0 = Aggregate1,
			Negative0 = [Literal| Negative1]
		).

	literal_bucket(Literal, positive_ground) :-
		body_literal_term(Literal, Term, positive),
		ground(Term),
		!.
	literal_bucket(Literal, positive_non_ground) :-
		body_literal_term(Literal, _, positive),
		!.
	literal_bucket(Literal, aggregate) :-
		body_literal_term(Literal, _, aggregate),
		!.
	literal_bucket(_Literal, negative).

	check_body_literals([]).
	check_body_literals([Literal| Literals]) :-
		body_literal_term(Literal, _, _),
		check_body_literals(Literals).

	body_literal_term(neg(Term), Term, negative) :-
		callable(Term),
		!.
	body_literal_term(agg(Operation, Template, Goals, Result), agg(Operation, Template, Goals, Result), aggregate) :-
		context(Context),
		aggregate_operation(Operation),
		check(list, Goals, Context),
		check_aggregate_goals(Goals),
		( var(Result) ; number(Result) ),
		!.
	body_literal_term(Term, Term, positive) :-
		callable(Term),
		Term \= neg(_),
		Term \= agg(_, _, _, _).

	check_aggregate_goals([]).
	check_aggregate_goals([Goal| Goals]) :-
		callable(Goal),
		Goal \= neg(_),
		Goal \= agg(_, _, _, _),
		check_aggregate_goals(Goals).

	aggregate_operation(count).
	aggregate_operation(sum).
	aggregate_operation(min).
	aggregate_operation(max).

	safe_rule(Head, Body) :-
		term_variables(Head, HeadVariables),
		positive_body_variables(Body, PositiveVariables),
		aggregate_body_variables(Body, AggregateVariables),
		negative_body_variables(Body, NegativeVariables),
		append(PositiveVariables, AggregateVariables, BoundVariables),
		subset_variables(HeadVariables, BoundVariables),
		subset_variables(NegativeVariables, PositiveVariables).

	aggregate_body_variables([], []).
	aggregate_body_variables([Literal| Literals], Variables) :-
		body_literal_term(Literal, Term, Sign),
		(	Sign == aggregate ->
			Term = agg(_, Template, Goals, Result),
			term_variables(Template-Goals-Result, TermVariables),
			aggregate_body_variables(Literals, VariablesTail),
			append(TermVariables, VariablesTail, Variables)
		;	aggregate_body_variables(Literals, Variables)
		).

	positive_body_variables([], []).
	positive_body_variables([Literal| Literals], Variables) :-
		body_literal_term(Literal, Term, Sign),
		(	Sign == positive ->
			term_variables(Term, TermVariables),
			positive_body_variables(Literals, VariablesTail),
			append(TermVariables, VariablesTail, Variables)
		;	positive_body_variables(Literals, Variables)
		).

	negative_body_variables([], []).
	negative_body_variables([Literal| Literals], Variables) :-
		body_literal_term(Literal, Term, Sign),
		(	Sign == negative ->
			term_variables(Term, TermVariables),
			negative_body_variables(Literals, VariablesTail),
			append(TermVariables, VariablesTail, Variables)
		;	negative_body_variables(Literals, Variables)
		).

	subset_variables([], _).
	subset_variables([Variable| Variables], Superset) :-
		varlist::memberchk(Variable, Superset),
		subset_variables(Variables, Superset).

	assert_fact(Fact) :-
		callable(Fact),
		ground(Fact),
		(	\+ edb_fact_(Fact) ->
			assertz(edb_fact_(Fact))
		;	true
		).

	retract_fact(Fact) :-
		retractall(edb_fact_(Fact)).

	materialize :-
		retractall(idb_fact_(_)),
		retractall(support_count_(_, _)),
		retractall(support_edge_(_, _, _)),
		ensure_stratified_program,
		maximum_stratum(MaximumStratum),
		materialize_strata(0, MaximumStratum).

	ensure_stratified_program :-
		compute_predicate_strata(_).

	compute_predicate_strata(MaximumStratum) :-
		retractall(predicate_stratum_(_, _, _)),
		collect_predicates(Predicates),
		initialize_predicate_strata(Predicates),
		length(Predicates, Count),
		relax_strata_iterations(Count),
		(	relax_strata_once(changed) ->
			permission_error(load, program, unstratified_negation_cycle)
		;	true
		),
		maximum_stratum(MaximumStratum).

	collect_predicates(Predicates) :-
		findall(
			Predicate,
			(
				rule_(_, Head, Body),
				(
					term_predicate(Head, Predicate)
				;	body_predicate(Body, Predicate)
				;	aggregate_body_predicate(Body, Predicate)
				)
			),
			Predicates0
		),
		sort(Predicates0, Predicates).

	aggregate_body_predicate([Literal| _], Predicate) :-
		body_literal_term(Literal, Term, aggregate),
		Term = agg(_, _, Goals, _),
		aggregate_goals_predicate(Goals, Predicate).
	aggregate_body_predicate([_| Literals], Predicate) :-
		aggregate_body_predicate(Literals, Predicate).

	aggregate_goals_predicate([Goal| _], Predicate) :-
		term_predicate(Goal, Predicate).
	aggregate_goals_predicate([_| Goals], Predicate) :-
		aggregate_goals_predicate(Goals, Predicate).

	body_predicate([Literal| _], Predicate) :-
		body_literal_term(Literal, Term, _),
		term_predicate(Term, Predicate).
	body_predicate([_| Literals], Predicate) :-
		body_predicate(Literals, Predicate).

	term_predicate(Term, predicate(Name, Arity)) :-
		functor(Term, Name, Arity).

	initialize_predicate_strata([]).
	initialize_predicate_strata([predicate(Name, Arity)| Predicates]) :-
		assertz(predicate_stratum_(Name, Arity, 0)),
		initialize_predicate_strata(Predicates).

	relax_strata_iterations(0) :-
		!.
	relax_strata_iterations(Count) :-
		relax_strata_once(_),
		NextCount is Count - 1,
		relax_strata_iterations(NextCount).

	relax_strata_once(Change) :-
		findall(rule(Head, Body), rule_(_, Head, Body), Rules),
		relax_rules(Rules, not_changed, Change).

	relax_rules([], Change, Change).
	relax_rules([rule(Head, Body)| Rules], Change0, Change) :-
		term_predicate(Head, predicate(HeadName, HeadArity)),
		predicate_stratum_(HeadName, HeadArity, HeadStratum),
		relax_body_literals(Body, HeadName, HeadArity, HeadStratum, Change0, Change1),
		relax_rules(Rules, Change1, Change).

	relax_body_literals([], _, _, _, Change, Change).
	relax_body_literals([Literal| Literals], HeadName, HeadArity, HeadStratum, Change0, Change) :-
		body_literal_term(Literal, Term, Sign),
		(	Sign == aggregate ->
			Term = agg(_, _, Goals, _),
			relax_aggregate_goals(Goals, HeadName, HeadArity, HeadStratum, Change0, Change1, NewHeadStratum)
		; 	term_predicate(Term, predicate(BodyName, BodyArity)),
			( predicate_stratum_(BodyName, BodyArity, BodyStratum) ->
				true
			; 	BodyStratum = 0
			),
			( Sign == negative ->
				MinimumHeadStratum is BodyStratum + 1
			; 	MinimumHeadStratum is BodyStratum
			),
			( HeadStratum < MinimumHeadStratum ->
				update_predicate_stratum(HeadName, HeadArity, MinimumHeadStratum),
				NewHeadStratum = MinimumHeadStratum,
				Change1 = changed
			; 	NewHeadStratum = HeadStratum,
				Change1 = Change0
			)
		),
		relax_body_literals(Literals, HeadName, HeadArity, NewHeadStratum, Change1, Change).

	relax_aggregate_goals([], _, _, HeadStratum, Change, Change, HeadStratum).
	relax_aggregate_goals([Goal| Goals], HeadName, HeadArity, HeadStratum, Change0, Change, FinalHeadStratum) :-
		term_predicate(Goal, predicate(BodyName, BodyArity)),
		( predicate_stratum_(BodyName, BodyArity, BodyStratum) ->
			true
		; 	BodyStratum = 0
		),
		MinimumHeadStratum is BodyStratum + 1,
		( HeadStratum < MinimumHeadStratum ->
			update_predicate_stratum(HeadName, HeadArity, MinimumHeadStratum),
			NextHeadStratum = MinimumHeadStratum,
			Change1 = changed
		; 	NextHeadStratum = HeadStratum,
			Change1 = Change0
		),
		relax_aggregate_goals(Goals, HeadName, HeadArity, NextHeadStratum, Change1, Change, FinalHeadStratum).

	update_predicate_stratum(Name, Arity, Stratum) :-
		retractall(predicate_stratum_(Name, Arity, _)),
		assertz(predicate_stratum_(Name, Arity, Stratum)).

	maximum_stratum(Maximum) :-
		findall(Stratum, predicate_stratum_(_, _, Stratum), Strata),
		(	Strata == [] ->
			Maximum is -1
		; 	max(Strata, Maximum)
		).

	materialize_strata(Stratum, MaximumStratum) :-
		Stratum =< MaximumStratum,
		!,
		materialize_fixpoint(Stratum),
		NextStratum is Stratum + 1,
		materialize_strata(NextStratum, MaximumStratum).
	materialize_strata(_, _).

	materialize_fixpoint(Stratum) :-
		derive_all(Stratum, NewEdges),
		( NewEdges =:= 0 ->
			true
		;	materialize_fixpoint(Stratum)
		).

	derive_all(Stratum, NewEdges) :-
		findall(
			derivation(FactCopy, Id, SupportsCopy),
			(
				rule_(Id, Head, Body),
				rule_stratum(Head, Stratum),
				derive_supports(Body, Supports),
				copy_term(Head-Supports, FactCopy-SupportsCopy),
				ground(FactCopy)
			),
			Derivations
		),
		add_derivations(Derivations, 0, NewEdges).

	rule_stratum(Head, Stratum) :-
		functor(Head, Name, Arity),
		predicate_stratum_(Name, Arity, Stratum).

	derive_supports([], []).
	derive_supports([Literal| Literals], [SupportCopy| Supports]) :-
		body_literal_term(Literal, Term, Sign),
		(	Sign == positive,
			fact_true_(Term),
			copy_term(Term, SupportCopy)
		;	Sign == negative,
			\+ fact_true_(Term),
			copy_term(neg(Term), SupportCopy)
		;	Sign == aggregate,
			derive_aggregate_literal(Term, SupportCopy)
		),
		derive_supports(Literals, Supports).

	derive_aggregate_literal(agg(count, Template, Goals, Count), agg(count, TemplateCopy, GoalsCopy, Count)) :-
		findall(Template, derive_aggregate_goals(Goals), Values),
		length(Values, CountValue),
		Count = CountValue,
		copy_term(Template-Goals, TemplateCopy-GoalsCopy).
	derive_aggregate_literal(agg(sum, Template, Goals, Sum), agg(sum, TemplateCopy, GoalsCopy, Sum)) :-
		findall(Template, derive_aggregate_goals(Goals), Values),
		sum(Values, SumValue),
		Sum = SumValue,
		copy_term(Template-Goals, TemplateCopy-GoalsCopy).
	derive_aggregate_literal(agg(min, Template, Goals, Min), agg(min, TemplateCopy, GoalsCopy, Min)) :-
		findall(Template, derive_aggregate_goals(Goals), Values),
		Values \= [],
		min(Values, MinValue),
		Min = MinValue,
		copy_term(Template-Goals, TemplateCopy-GoalsCopy).
	derive_aggregate_literal(agg(max, Template, Goals, Max), agg(max, TemplateCopy, GoalsCopy, Max)) :-
		findall(Template, derive_aggregate_goals(Goals), Values),
		Values \= [],
		max(Values, MaxValue),
		Max = MaxValue,
		copy_term(Template-Goals, TemplateCopy-GoalsCopy).

	derive_aggregate_goals([]).
	derive_aggregate_goals([Goal| Goals]) :-
		fact_true_(Goal),
		derive_aggregate_goals(Goals).

	fact_true_(Fact) :-
		edb_fact_(Fact).
	fact_true_(Fact) :-
		idb_fact_(Fact).

	add_derivations([], Added, Added).
	add_derivations([derivation(Fact, Id, Supports)| Derivations], Added0, Added) :-
		add_derivation(Fact, Id, Supports, Increment),
		Added1 is Added0 + Increment,
		add_derivations(Derivations, Added1, Added).

	add_derivation(Fact, Id, Supports, 0) :-
		support_edge_(Fact, Id, Supports),
		!.
	add_derivation(Fact, Id, Supports, 1) :-
		assertz(support_edge_(Fact, Id, Supports)),
		(	retract(support_count_(Fact, Count0)) ->
			Count is Count0 + 1
		;	Count is 1
		),
		assertz(support_count_(Fact, Count)),
		(	idb_fact_(Fact) ->
			true
		;	assertz(idb_fact_(Fact))
		).

	update(Inserts, Deletes, delta(Added, Removed)) :-
		context(Context),
		check(list, Inserts, Context),
		check(list, Deletes, Context),
		facts(OldFacts),
		(	(has_negation_rules ; has_aggregate_rules) ->
			apply_deletes_plain(Deletes),
			apply_inserts_plain(Inserts),
			materialize
		;	apply_deletes(Deletes, DeleteSeeds),
			propagate_deletes(DeleteSeeds),
			apply_inserts(Inserts, OldFacts, InsertSeeds),
			propagate_inserts(InsertSeeds),
			repair_closure
		),
		facts(NewFacts),
		subtract(NewFacts, OldFacts, Added),
		subtract(OldFacts, NewFacts, Removed).

	has_negation_rules :-
		rule_(_, _, Body),
		contains_negation_literal(Body),
		!.

	has_aggregate_rules :-
		rule_(_, _, Body),
		contains_aggregate_literal(Body),
		!.

	contains_negation_literal([Literal| _]) :-
		body_literal_term(Literal, _, negative),
		!.
	contains_negation_literal([_| Literals]) :-
		contains_negation_literal(Literals).

	contains_aggregate_literal([Literal| _]) :-
		body_literal_term(Literal, _, aggregate),
		!.
	contains_aggregate_literal([_| Literals]) :-
		contains_aggregate_literal(Literals).

	apply_deletes_plain([]).
	apply_deletes_plain([Fact| Facts]) :-
		retract_fact(Fact),
		apply_deletes_plain(Facts).

	apply_inserts_plain([]).
	apply_inserts_plain([Fact| Facts]) :-
		assert_fact(Fact),
		apply_inserts_plain(Facts).

	apply_deletes(Deletes, DeleteSeeds) :-
		apply_deletes(Deletes, [], DeleteSeeds).

	apply_deletes([], Seeds, Seeds).
	apply_deletes([Fact| Facts], Seeds0, Seeds) :-
		retract_fact(Fact),
		(	\+ fact_true_(Fact), \+ memberchk(Fact, Seeds0) ->
			Seeds1 = [Fact| Seeds0]
		;	Seeds1 = Seeds0
		),
		apply_deletes(Facts, Seeds1, Seeds).

	apply_inserts(Inserts, OldFacts, InsertSeeds) :-
		apply_inserts(Inserts, OldFacts, [], InsertSeeds).

	apply_inserts([], _, Seeds, Seeds).
	apply_inserts([Fact| Facts], OldFacts, Seeds0, Seeds) :-
		assert_fact(Fact),
		(	\+ memberchk(Fact, OldFacts), \+ memberchk(Fact, Seeds0) ->
			Seeds1 = [Fact| Seeds0]
		;	Seeds1 = Seeds0
		),
		apply_inserts(Facts, OldFacts, Seeds1, Seeds).

	propagate_deletes(Seeds) :-
		propagate_deletes_queue(Seeds).

	propagate_deletes_queue([]).
	propagate_deletes_queue([LostFact| Queue]) :-
		remove_fact_supports(LostFact, NewlyLostFacts),
		append_unique(Queue, NewlyLostFacts, Queue2),
		propagate_deletes_queue(Queue2).

	remove_fact_supports(LostFact, NewlyLostFacts) :-
		findall(
			edge(DerivedFact, Id, Supports),
			( support_edge_(DerivedFact, Id, Supports), memberchk(LostFact, Supports) ),
			Edges
		),
		remove_edges(Edges, [], NewlyLostFacts).

	remove_edges([], LostFacts, LostFacts).
	remove_edges([edge(DerivedFact, Id, Supports)| Edges], LostFacts0, LostFacts) :-
		(	retract(support_edge_(DerivedFact, Id, Supports)) ->
			decrement_support_count(DerivedFact, DerivedLost),
			( DerivedLost == true, \+ memberchk(DerivedFact, LostFacts0) ->
				LostFacts1 = [DerivedFact| LostFacts0]
			;	LostFacts1 = LostFacts0
			)
		;	LostFacts1 = LostFacts0
		),
		remove_edges(Edges, LostFacts1, LostFacts).

	decrement_support_count(Fact, Lost) :-
		retract(support_count_(Fact, Count0)),
		Count is Count0 - 1,
		(	Count > 0 ->
			assertz(support_count_(Fact, Count)),
			Lost = false
		;	retractall(idb_fact_(Fact)),
			( edb_fact_(Fact) ->
				Lost = false
			;	Lost = true
			)
		),
		!.
	decrement_support_count(_, false).

	propagate_inserts([]).
	propagate_inserts(DeltaFacts) :-
		derive_from_delta(DeltaFacts, NewFacts),
		(	NewFacts == [] ->
			true
		;	propagate_inserts(NewFacts)
		).

	derive_from_delta(DeltaFacts, NewFacts) :-
		findall(
			derivation(FactCopy, Id, SupportsCopy),
			(	rule_(Id, Head, Body),
				derive_supports(Body, Supports),
				supports_intersect(Supports, DeltaFacts),
				copy_term(Head-Supports, FactCopy-SupportsCopy),
				ground(FactCopy)
			),
			Derivations
		),
		add_derivations_collect_new_facts(Derivations, [], NewFacts0),
		sort(NewFacts0, NewFacts).

	supports_intersect([Support| _], DeltaFacts) :-
		memberchk(Support, DeltaFacts),
		!.
	supports_intersect([_| Supports], DeltaFacts) :-
		supports_intersect(Supports, DeltaFacts).

	add_derivations_collect_new_facts([], NewFacts, NewFacts).
	add_derivations_collect_new_facts([derivation(Fact, Id, Supports)| Derivations], NewFacts0, NewFacts) :-
		add_derivation_status(Fact, Id, Supports, Status),
		( Status == new_fact ->
			NewFacts1 = [Fact| NewFacts0]
		;	NewFacts1 = NewFacts0
		),
		add_derivations_collect_new_facts(Derivations, NewFacts1, NewFacts).

	add_derivation_status(Fact, Id, Supports, none) :-
		support_edge_(Fact, Id, Supports),
		!.
	add_derivation_status(Fact, Id, Supports, Status) :-
		assertz(support_edge_(Fact, Id, Supports)),
		(	retract(support_count_(Fact, Count0)) ->
			Count is Count0 + 1,
			Status = support_only
		;	Count is 1,
			Status = new_fact
		),
		assertz(support_count_(Fact, Count)),
		(	idb_fact_(Fact) ->
			true
		;	assertz(idb_fact_(Fact))
		).

	repair_closure :-
		materialize_delta_fixpoint.

	materialize_delta_fixpoint :-
		maximum_stratum(MaximumStratum),
		derive_all_strata(0, MaximumStratum, NewEdges),
		(	NewEdges =:= 0 ->
			true
		;	materialize_delta_fixpoint
		).

	derive_all_strata(Stratum, MaximumStratum, 0) :-
		Stratum > MaximumStratum,
		!.
	derive_all_strata(Stratum, MaximumStratum, NewEdges) :-
		derive_all(Stratum, CurrentEdges),
		NextStratum is Stratum + 1,
		derive_all_strata(NextStratum, MaximumStratum, RemainingEdges),
		NewEdges is CurrentEdges + RemainingEdges.

	query(Goal) :-
		fact_true_(Goal).

	query(Goal, Bindings) :-
		fact_true_(Goal),
		Bindings = Goal.

	explain(Fact, edb) :-
		edb_fact_(Fact).
	explain(Fact, proof(rule(Id), Supports)) :-
		support_edge_(Fact, Id, Supports).

	rules(Rules) :-
		findall(rule(Id, Head, Body), rule_(Id, Head, Body), Rules).

	facts(Facts) :-
		findall(Fact, fact_true_(Fact), Facts0),
		sort(Facts0, Facts).

	predicate_stratum(Name, Arity, Stratum) :-
		ensure_stratified_program,
		predicate_stratum_(Name, Arity, Stratum).

	strata(Strata) :-
		ensure_stratified_program,
		findall(Stratum, predicate_stratum_(_, _, Stratum), Strata0),
		sort(Strata0, StratumNumbers),
		strata_from_numbers(StratumNumbers, Strata).

	strata_from_numbers([], []).
	strata_from_numbers([Stratum| StratumNumbers], [stratum(Stratum, Predicates)| Strata]) :-
		predicates_in_stratum(Stratum, Predicates),
		strata_from_numbers(StratumNumbers, Strata).

	predicates_in_stratum(Stratum, Predicates) :-
		findall(predicate(Name, Arity), predicate_stratum_(Name, Arity, Stratum), Predicates0),
		sort(Predicates0, Predicates).

	append_unique(List, [], List).
	append_unique(List, [Head| Tail], Result) :-
		(	memberchk(Head, List) ->
			append_unique(List, Tail, Result)
		;	append_unique([Head| List], Tail, Result)
		).

:- end_object.
