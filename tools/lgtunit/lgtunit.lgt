%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(lgtunit,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- set_logtalk_flag(debug, off).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/10/23,
		comment is 'A simple unit test framework featuring predicate clause coverage.'
	]).

	:- public(cover/1).
	:- mode(cover(?entity_identifier), zero_or_more).
	:- info(cover/1, [
		comment is 'Declares entities being tested for which code coverage information should be collected.'
	]).

	:- public(run/2).
	:- mode(run(+atom, +atom), one).
	:- info(run/2, [
		comment is 'Runs the unit tests, writing the results to the specified file. Mode can be either "write" (to create a new file) or "append" (to add results to an existing file).',
		argnames is ['File', 'Mode']
	]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs the unit tests, writing the results to the current output stream.'
	]).

	:- public(deterministic/1).
	:- meta_predicate(deterministic(0)).
	:- mode(deterministic(+callable), zero_or_one).
	:- info(deterministic/1, [
		comment is 'True if a goal succeeds once without leaving choice-points.',
		argnames is ['Goal']
	]).

	:- public(op(700, xfx, ('=~='))).
	:- public(('=~=')/2).
	:- mode('=~='(+float, +float), zero_or_one).
	:- info(('=~=')/2, [
		comment is 'Compares two floats for approximate equality using 100*epsilon for the absolute error and, if that fails, 99.999% accuracy for the relative error. Handy when writing certain unit tests but the default precision values may not be adequate for all cases.',
		argnames is ['Float1', 'Float2']
	]).

	:- protected(run_tests/0).
	:- mode(run_tests, one).
	:- info(run_tests/0, [
		comment is 'Runs all defined unit tests.'
	]).

	:- protected(run_tests/2).
	:- mode(run_tests(+list(callable), +atom), one).
	:- info(run_tests/2, [
		comment is 'Runs a list of defined tests.',
		argnames is ['Tests', 'File']
	]).

	:- protected(setup/0).
	:- mode(setup, zero_or_one).
	:- info(setup/0, [
		comment is 'Setup environment before running the test set. Defaults to the goal true.'
	]).

	:- protected(cleanup/0).
	:- mode(cleanup, zero_or_one).
	:- info(cleanup/0, [
		comment is 'Cleanup environment after running the test set. Defaults to the goal true.'
	]).

	:- private(test/2).
	:- mode(test(?atom, ?nonvar), zero_or_more).
	:- info(test/2, [
		comment is 'Specifies a unit test.',
		argnames is ['Identifier', 'Outcome']
	]).

	:- private(test_/1).
	:- dynamic(test_/1).
	:- mode(test_(?compound), zero_or_more).
	:- info(test_/1, [
		comment is 'Table of defined tests.',
		argnames is ['Test']
	]).

	:- private(skipped_/1).
	:- dynamic(skipped_/1).
	:- mode(skipped_(?integer), zero_or_one).
	:- info(skipped_/1, [
		comment is 'Counter for skipped tests.',
		argnames is ['Counter']
	]).

	:- private(passed_/1).
	:- dynamic(passed_/1).
	:- mode(passed_(?integer), zero_or_one).
	:- info(passed_/1, [
		comment is 'Counter for passed tests.',
		argnames is ['Counter']
	]).

	:- private(failed_/1).
	:- dynamic(failed_/1).
	:- mode(failed_(?callable), zero_or_one).
	:- info(failed_/1, [
		comment is 'Counter for failed tests.',
		argnames is ['Counter']
	]).

	:- private(fired_/3).
	:- dynamic(fired_/3).
	:- mode(fired_(?entity_identifier, ?predicate_indicator, ?integer), zero_or_more).
	:- info(fired_/3, [
		comment is 'Fired clauses when running the unit tests.',
		argnames is ['Entity', 'Predicate', 'Clause']
	]).

	:- private(covered_/2).
	:- dynamic(covered_/2).
	:- mode(covered_(?integer, ?integer), zero_or_more).
	:- info(covered_/2, [
		comment is 'Auxiliary predicate for collecting statistics on clause coverage.',
		argnames is ['Covered', 'Total']
	]).

	% we use the structured printing mechanism in order to allow unit tests
	% results to be intercepted for alternative reporting by e.g. GUI IDEs
	:- uses(logtalk, [
		print_message/3
	]).

	run(File, Mode) :-
		open(File, Mode, Stream, [alias(lgtunit_redirected_output)]),
		logtalk::assertz((
			message_hook(_, Kind, lgtunit, Tokens) :-
				message_prefix_stream(Kind, lgtunit, Prefix, _),
				print_message_tokens(lgtunit_redirected_output, Prefix, Tokens)
		)),
		run,
		logtalk::retractall(message_hook(_, _, lgtunit, _)),
		close(Stream).

	run :-
		current_output(Output),
		reset_test_counters,
		reset_coverage_results,
		write_tests_header,
		(	catch(::setup, Error, (broken_step(setup, Error), fail)) ->
			::run_tests,
			(	catch(::cleanup, Error, (broken_step(cleanup, Error), fail)) ->
				true
			;	failed_step(cleanup)
			),
			write_tests_results,
			write_coverage_results
		;	failed_step(setup)
		),
		write_tests_footer,
		set_output(Output).

	% by default, no test setup is needed:
	setup.

	% by default, no test cleanup is needed:
	cleanup.

	run_tests([], _).
	run_tests([Test| Tests], File) :-
		run_test(Test, File),
		run_tests(Tests, File).

	% by default, no tests are defined:
	run_tests :-
		run_tests([], _).

	run_test(succeeds(Test, Position), File) :-
		(	catch(::test(Test, true), Error, failed_test(Test, File, Position, error_instead_of_success(Error))) ->
			(	var(Error) ->
				passed_test(Test, File, Position)
			;	true
			)
		;	failed_test(Test, File, Position, failure_instead_of_success)
		).
	run_test(deterministic(Test, Position), File) :-
		(	catch(::test(Test, deterministic), Error, failed_test(Test, File, Position, error_instead_of_success(Error))) ->
			(	var(Error) ->
				passed_test(Test, File, Position)
			;	true
			)
		;	failed_test(Test, File, Position, failure_instead_of_success)
		).
	run_test(fails(Test, Position), File) :-
		(	catch(::test(Test, fail), Error, failed_test(Test, File, Position, error_instead_of_failure(Error))) ->
			(	var(Error) ->
				failed_test(Test, File, Position, success_instead_of_failure)
			;	true
			)
		;	passed_test(Test, File, Position)
		).
	run_test(throws(Test, PossibleErrors, Position), File) :-
		(	catch(::test(Test, PossibleErrors), Error, check_error(Test, PossibleErrors, Error, File, Position)) ->
			(	var(Error) ->
				failed_test(Test, File, Position, success_instead_of_error)
			;	true
			)
		;	failed_test(Test, File, Position, failure_instead_of_error)
		).

	check_error(Test, [PossibleError| PossibleErrors], Error, File, Position) :-
		(	member(ExpectedError, [PossibleError| PossibleErrors]),
			subsumes_term(ExpectedError, Error) ->
			passed_test(Test, File, Position)
		;	failed_test(Test, File, Position, wrong_error(PossibleError, Error))
		).

	write_tests_header :-
		self(Self),
		date::today(Year, Month, Day),
		time::now(Hours, Minutes, Seconds),
		print_message(information, lgtunit, tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		(	object_property(Self, file(File, Directory)) ->
			atom_concat(Directory, File, Path),
			print_message(information, lgtunit, running_tests_from_object_file(Self, Path))
		;	print_message(information, lgtunit, running_tests_from_object(Self))
		).

	write_tests_results :-
		self(Self),
		::skipped_(Skipped),
		::passed_(Passed),
		::failed_(Failed),
		Total is Skipped + Passed + Failed,
		print_message(information, lgtunit, tests_results_summary(Total, Skipped, Passed, Failed)),
		print_message(information, lgtunit, completed_tests_from_object(Self)).

	write_tests_footer :-
		date::today(Year, Month, Day),
		time::now(Hours, Minutes, Seconds),
		print_message(information, lgtunit, tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)).

	passed_test(Test, File, Position) :-
		increment_passed_tests_counter,
		print_message(information, lgtunit, passed_test(Test, File, Position)).

	failed_test(Test, File, Position, Reason) :-
		increment_failed_tests_counter,
		print_message(error, lgtunit, failed_test(Test, File, Position, Reason)).

	broken_step(Step, Error) :-
		self(Self),
		print_message(error, lgtunit, broken_step(Step, Self, Error)).

	failed_step(Step) :-
		self(Self),
		print_message(error, lgtunit, failed_step(Step, Self)).

	reset_compilation_counters :-
		retractall(test_(_)),
		retractall(skipped_(_)),
		asserta(skipped_(0)).

	increment_skipped_tests_counter :-
		retract(skipped_(Old)) ->
		New is Old + 1,
		asserta(skipped_(New)).

	reset_test_counters :-
		::retractall(passed_(_)),
		::asserta(passed_(0)),
		::retractall(failed_(_)),
		::asserta(failed_(0)).

	increment_passed_tests_counter :-
		::retract(passed_(Old)),
		New is Old + 1,
		::asserta(passed_(New)).

	increment_failed_tests_counter :-
		::retract(failed_(Old)),
		New is Old + 1,
		::asserta(failed_(New)).

	% different unit test idioms are supported using the term-expansion mechanism;
	% the unit test objects must be loaded using this object as an hook object
	%
	% we make sure that context-switching calls are enabled to help the user in
	% debugging failed unit tests
	term_expansion(
			(:- object(Test, Relation)),
			[(:- object(Test, Relation)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		reset_compilation_counters.
	term_expansion(
			(:- object(Test, Relation1, Relation2)),
			[(:- object(Test, Relation1, Relation2)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		reset_compilation_counters.
	term_expansion(
			(:- object(Test, Relation1, Relation2, Relation3)),
			[(:- object(Test, Relation1, Relation2, Relation3)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		reset_compilation_counters.
	term_expansion(
			(:- object(Test, Relation1, Relation2, Relation3, Relation4)),
			[(:- object(Test, Relation1, Relation2, Relation3, Relation4)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		reset_compilation_counters.
	term_expansion(
			(:- object(Test, Relation1, Relation2, Relation3, Relation4, Relation5)),
			[(:- object(Test, Relation1, Relation2, Relation3, Relation4, Relation5)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		reset_compilation_counters.

	% the discontiguous/1 directives usually required when using some of the
	% unit tests idioms are no longer necessary after term-expanding them 
	term_expansion((:- discontiguous(PI)), Expansion) :-
		ground(PI),
		filter_discontiguous_directive(PI, Filtered),
		(	Filtered == [] ->
			Expansion = []
		;	Expansion = (:- discontiguous(Filtered))
		).

	% skipped tests
	term_expansion((- Head :- _), []) :-
		test_idiom_head(Head, Test),
		check_for_valid_test_identifier(Test),
		increment_skipped_tests_counter.

	% unit test idiom test/2
	term_expansion((test(Test, Outcome0) :- Goal0), [(test(Test, Outcome) :- Goal)]) :-
		callable(Outcome0),
		convert_test_outcome(Outcome0, Goal0, Outcome, Goal),
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		(	Outcome == true ->
			assertz(test_(succeeds(Test, Position)))
		;	Outcome == deterministic ->
			assertz(test_(deterministic(Test, Position)))
		;	Outcome == fail ->
			assertz(test_(fails(Test, Position)))
		;	% errors
			assertz(test_(throws(Test, Outcome, Position)))
		).

	% unit test idiom test/1
	term_expansion((test(Test) :- Goal), [(test(Test, true) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(succeeds(Test, Position))).

	% unit test idiom succeeds/1 + deterministic/1 + fails/1 + throws/2
	term_expansion((succeeds(Test) :- Goal), [(test(Test, true) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(succeeds(Test, Position))).
	term_expansion((deterministic(Test) :- Goal), [(test(Test, deterministic) :- lgtunit::deterministic(Goal))]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(deterministic(Test, Position))).
	term_expansion((fails(Test) :- Goal), [(test(Test, fail) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(fails(Test, Position))).
	term_expansion((throws(Test, Balls) :- Goal), [(test(Test, Errors) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		(	Balls = [_| _] ->
			Errors = Balls
		;	Errors = [Balls]
		),
		logtalk_load_context(term_position, Position),
		assertz(test_(throws(Test, Errors, Position))).

	% collect all unit test identifiers when reching the end_object/0 directive 
	term_expansion((:- end_object), [skipped_(N), (run_tests :- ::run_tests(Tests, File)), (:- end_object)]) :-
		retract(skipped_(N)),
		findall(Test, retract(test_(Test)), Tests),
		logtalk_load_context(source, File).

	% support the deprecated unit/1 predicate which may still be in use in old code
	term_expansion(unit(Entity), [cover(Entity)]).

	filter_discontiguous_directive((PI,PIs), Filtered) :-
		filter_discontiguous_directive(PI, FilteredHead),
		filter_discontiguous_directive(PIs, FilteredTail),
		append(FilteredHead, FilteredTail, Filtered).
	filter_discontiguous_directive([], []).
	filter_discontiguous_directive([PI| PIs], Filtered) :-
		filter_discontiguous_directive(PI, FilteredHead),
		filter_discontiguous_directive(PIs, FilteredTail),
		append(FilteredHead, FilteredTail, Filtered).
	filter_discontiguous_directive(Functor/Arity, Filtered) :-
		(	ignorable_discontiguous_predicate(Functor/Arity) ->
			Filtered = []
		;	Filtered = Functor/Arity
		).

	ignorable_discontiguous_predicate((-)/1).
	ignorable_discontiguous_predicate(test/2).
	ignorable_discontiguous_predicate(test/1).
	ignorable_discontiguous_predicate(succeeds/1).
	ignorable_discontiguous_predicate(deterministic/1).
	ignorable_discontiguous_predicate(fails/1).
	ignorable_discontiguous_predicate(throws/2).

	convert_test_outcome(true, Goal, true, Goal).
	convert_test_outcome(true(Test), Goal, true, (Goal, Test)).
	convert_test_outcome(deterministic, Goal, deterministic, lgtunit::deterministic(Goal)).
	convert_test_outcome(deterministic(Test), Goal, deterministic, (lgtunit::deterministic(Goal), Test)).
	convert_test_outcome(fail, Goal, fail, Goal).
	convert_test_outcome(error(Ball), Goal, [error(Ball,_)], Goal).
	convert_test_outcome(errors(Balls), Goal, Errors, Goal) :-
		map_errors(Balls, Errors).
	convert_test_outcome(ball(Ball), Goal, [Ball], Goal).
	convert_test_outcome(balls(Balls), Goal, Balls, Goal).

	map_errors([], []).
	map_errors([Ball| Balls], [error(Ball,_)| Errors]) :-
		map_errors(Balls, Errors).

	test_idiom_head(test(Test, _), Test).
	test_idiom_head(test(Test), Test).
	test_idiom_head(succeeds(Test), Test).
	test_idiom_head(deterministic(Test), Test).
	test_idiom_head(fails(Test), Test).
	test_idiom_head(throws(Test, _), Test).

	check_for_valid_test_identifier(Test) :-
		(	var(Test) ->
			print_message(error, lgtunit, non_instantiated_test_identifier)
		;	(	test_(succeeds(Test, _))
			;	test_(deterministic(Test, _))
			;	test_(fails(Test, _))
			;	test_(throws(Test, _, _))
			) ->
			print_message(error, lgtunit, repeated_test_identifier(Test))
		;	true
		).

	:- if((	current_logtalk_flag(prolog_dialect, Dialect),
			(Dialect == b; Dialect == qp; Dialect == swi; Dialect == yap)
	)).
		deterministic(Goal) :-
			setup_call_cleanup(true, Goal, Deterministic = true),
			(	nonvar(Deterministic) ->
				!
			;	!,
				fail
			).
	:- elif((	current_logtalk_flag(prolog_dialect, Dialect),
				(Dialect == cx; Dialect == sicstus; Dialect == xsb)
	)).
		deterministic(Goal) :-
			call_cleanup(Goal, Deterministic = true),
			(	var(Deterministic) ->
				!,
				fail
			;	!
			).
	:- elif(current_logtalk_flag(prolog_dialect, gnu)).
		deterministic(Goal) :-
			call_det(Goal, Deterministic),
			!,
			Deterministic == true.
	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).
		deterministic(Goal) :-
			{sepia_kernel:get_cut(Before)},
			call(Goal),
			{sepia_kernel:get_cut(After)},
			!,
			Before == After.
	:- else.
		deterministic(_) :-
			throw(error(resource_error, deterministic/1)).
	:- endif.

	'=~='(Float1, Float2) :-
		(	% first test the absolute error, for meaningful results with numbers very close to zero:
			epsilon(Epsilon), abs(Float1 - Float2) < 100*Epsilon ->
			true
		;	% if that fails, test the relative error (protected by a catch/3 to avoid division errors)
			% by using as the divisor the larger float in order to make argument order irrelevant:
			abs(Float1) > abs(Float2) ->
			catch(abs((Float1 - Float2) / Float1) < 0.00001, _, fail)	% 99.999% accuracy
		;	catch(abs((Float1 - Float2) / Float2) < 0.00001, _, fail)
		).

	:- if((	current_logtalk_flag(prolog_dialect, Dialect),
			(Dialect == swi; Dialect == yap; Dialect == gnu; Dialect == b; Dialect == cx)
	)).
		epsilon(Epsilon) :-
			Epsilon is epsilon.
	:- else.
		epsilon(0.000000000001).
	:- endif.

	% predicate clause coverage support;
	% it requires entities compiled in debug mode

	:- multifile(logtalk::trace_event/2).
	:- dynamic(logtalk::trace_event/2).

	% the Logtalk runtime calls all defined logtalk::trace_event/2 hooks using
	% a failure-driven loop; thus we don't have to worry about handling all
	% events or failing after handling an event to give other hooks a chance
	logtalk::trace_event(fact(Entity, Fact, N, _), _) :-
		fired(Entity, Fact, N).
	logtalk::trace_event(rule(Entity, Head, N, _), _) :-
		fired(Entity, Head, N).

	fired(Entity, Other::Head, N) :-
		!,
		functor(Head, Functor, Arity),
		(	fired_(Entity, Other::Functor/Arity, N) ->
			true
		;	atom(Entity) ->
			assertz(fired_(Entity, Other::Functor/Arity, N))
		;	functor(Entity, EntityFunctor, EntityArity),
			functor(Template, EntityFunctor, EntityArity),
			assertz(fired_(Template, Other::Functor/Arity, N))
		).
	fired(_, ':'(_,_), _) :-
		% clauses of Prolog module multifile predicates currently have
		% an index of zero with no associated source data information
		!.
	fired(Entity, Head, N) :-
		functor(Head, Functor, Arity),
		(	fired_(Entity, Functor/Arity, N) ->
			true
		;	atom(Entity) ->
			assertz(fired_(Entity, Functor/Arity, N))
		;	functor(Entity, EntityFunctor, EntityArity),
			functor(Template, EntityFunctor, EntityArity),
			assertz(fired_(Template, Functor/Arity, N))
		).

	reset_coverage_results :-
		retractall(fired_(_, _, _)).

	write_coverage_results :-
		(	setof(TestedEntity, fired_entity(TestedEntity), TestedEntities) ->
			write_coverage_results(TestedEntities),
			setof(DeclaredEntity, ::cover(DeclaredEntity), DeclaredEntities),
			write_coverage_results_summary(DeclaredEntities, TestedEntities)
		;	print_message(information, lgtunit, no_code_coverage_information_collected)
		).

	fired_entity(Entity) :-
		::cover(Entity),
		\+ \+ fired_(Entity, _, _).

	write_coverage_results([]).
	write_coverage_results([Entity| Entities]) :-
		write_entity_coverage_information(Entity),
		write_coverage_results(Entities).

	write_entity_coverage_information(Entity) :-
		% do not consider dynamic clauses asserted at runtime (which have an index of zero)
		setof(N, (fired_(Entity, Other::Functor/Arity, N), N > 0), Ns),
		number_of_clauses(Entity, Other::Functor/Arity, Total),
		length(Ns, Covered),
		(	Covered =< Total ->
			assertz(covered_(Covered, Total))
		;	% likely a dynamic predicate with clauses asserted at runtime
			assertz(covered_(Covered, Covered))
		),
		print_message(information, lgtunit, entity_clause_coverage(Entity, Other::Functor/Arity, Covered/Total, Ns)),
		fail.
	write_entity_coverage_information(Entity) :-
		% do not consider dynamic clauses asserted at runtime (which have an index of zero)
		setof(N, (fired_(Entity, Functor/Arity, N), N > 0), Ns),
		number_of_clauses(Entity, Functor/Arity, Total),
		length(Ns, Covered),
		(	Covered =< Total ->
			assertz(covered_(Covered, Total))
		;	% likely a dynamic predicate with clauses asserted at runtime
			assertz(covered_(Covered, Covered))
		),
		print_message(information, lgtunit, entity_clause_coverage(Entity, Functor/Arity, Covered/Total, Ns)),
		fail.
	write_entity_coverage_information(Entity) :-
		current_object(Entity),
		object_property(Entity, defines(Functor/Arity, Properties)),
		% do not consider dynamic clauses asserted at runtime (which have an index of zero)
		\+ (fired_(Entity, Functor/Arity, N), N > 0),
		memberchk(number_of_clauses(Total), Properties),
		\+ memberchk(auxiliary, Properties),
		assertz(covered_(0, Total)),
		print_message(information, lgtunit, entity_clause_coverage(Entity, Functor/Arity, 0/Total, [])),
		fail.
	write_entity_coverage_information(Entity) :-
		current_category(Entity),
		category_property(Entity, defines(Functor/Arity, Properties)),
		% do not consider dynamic clauses asserted at runtime (which have an index of zero)
		\+ (fired_(Entity, Functor/Arity, N), N > 0),
		memberchk(number_of_clauses(Total), Properties),
		\+ memberchk(auxiliary, Properties),
		assertz(covered_(0, Total)),
		print_message(information, lgtunit, entity_clause_coverage(Entity, Functor/Arity, 0/Total, [])),
		fail.
	write_entity_coverage_information(_).

	number_of_clauses(Entities, Total) :-
		entities_number_of_clauses(Entities, 0, Total).

	entities_number_of_clauses([], Total, Total).
	entities_number_of_clauses([Entity| Entities], Total0, Total) :-
		(	current_object(Entity) ->
			object_property(Entity, number_of_user_clauses(EntityTotal))
		;	current_category(Entity) ->
			category_property(Entity, number_of_user_clauses(EntityTotal))
		;	% protocol
			EntityTotal = 0
		),
		Total1 is Total0 + EntityTotal,
		entities_number_of_clauses(Entities, Total1, Total).

	number_of_clauses(Entity, Other::Functor/Arity, Total) :-
		current_object(Entity),
		object_property(Entity, provides(Functor/Arity, Other, Properties)),
		memberchk(number_of_clauses(Total), Properties),
		!.
	number_of_clauses(Entity, Functor/Arity, Total) :-
		current_object(Entity),
		object_property(Entity, defines(Functor/Arity, Properties)),
		memberchk(number_of_clauses(Total), Properties),
		!.
	number_of_clauses(Entity, Other::Functor/Arity, Total) :-
		current_category(Entity),
		category_property(Entity, provides(Functor/Arity, Other, Properties)),
		memberchk(number_of_clauses(Total), Properties),
		!.
	number_of_clauses(Entity, Functor/Arity, Total) :-
		current_category(Entity),
		category_property(Entity, defines(Functor/Arity, Properties)),
		memberchk(number_of_clauses(Total), Properties),
		!.
	number_of_clauses(_, _, 0).

	write_coverage_results_summary(DeclaredEntities, TestedEntities) :-
		length(DeclaredEntities, TotalDeclaredEntities),
		number_of_clauses(DeclaredEntities, TotalClauses),
		length(TestedEntities, TotalTestedEntities),
		%number_of_clauses(TestedEntities, Clauses),
		covered(Coverage, TestedClauses),
		Percentage is Coverage * 100 / TotalClauses,
		print_message(information, lgtunit, declared_entities_and_clause_numbers(TotalDeclaredEntities, TotalClauses)),
		print_message(information, lgtunit, covered_entities_and_clause_numbers(TotalTestedEntities, TestedClauses)),
		print_message(information, lgtunit, covered_clause_numbers(Coverage, TotalClauses, Percentage)).

	covered(Coverage, Clauses) :-
		findall(Covered-Total, retract(covered_(Covered, Total)), List),
		sum_coverage(List, Coverage, Clauses).

	sum_coverage(List, Coverage, Clauses) :-
		sum_coverage(List, 0, Coverage, 0, Clauses).

	sum_coverage([], Coverage, Coverage, Clauses, Clauses).
	sum_coverage([Covered-Total| List], Coverage0, Coverage, Clauses0, Clauses) :-
		Coverage1 is Coverage0 + Covered,
		Clauses1 is Clauses0 + Total,
		sum_coverage(List, Coverage1, Coverage, Clauses1, Clauses).

	% auxiliary predicates; we could use the Logtalk standard library but we
	% prefer to make this object self-contained given its testing purpose

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	length(List, Length) :-
		length(List, 0, Length).

	length([], Length, Length).
	length([_| Tail], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Tail, Acc2, Length).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

	memberchk(Element, [Element| _]) :-
		!.
	memberchk(Element, [_| List]) :-
		memberchk(Element, List).

:- end_object.
