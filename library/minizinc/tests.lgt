%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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
		date is 2026-11-10,
		comment is 'Unit tests for the minizinc library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, member/2, msort/2
	]).

	:- uses(os, [
		delete_file/1, directory_files/2, resolve_command_path/2, temporary_directory/1
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	cover(minizinc_program).
	cover(minizinc_output).
	cover(minizinc_expansion).
	cover(minizinc_process).
	cover(minizinc).

	% -----------------------------------------------------------------------
	% minizinc_program: statement and expression serialization
	% -----------------------------------------------------------------------

	test(minizinc_program_empty, deterministic(Codes == [])) :-
		minizinc_program::serialize([], Codes).

	test(minizinc_program_decl_par, deterministic(Atom == 'int: n = 5;\n')) :-
		minizinc_program::serialize([decl(par(int), n, int(5))], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_decl_var_range, deterministic(Atom == 'var 1..10: x;\n')) :-
		minizinc_program::serialize([decl(var(range(int(1), int(10))), x, none)], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_array_decl, deterministic(Atom == 'array[1..3] of int: a = [1,2,3];\n')) :-
		minizinc_program::serialize([
			array_decl(par(int), [range(int(1), int(3))], a, array([int(1), int(2), int(3)]))
		], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_array2d, deterministic(Atom == 'int: m = [|1,2|3,4|];\n')) :-
		minizinc_program::serialize([
			decl(par(int), m, array2d([[int(1), int(2)], [int(3), int(4)]]))
		], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_operator_precedence, deterministic(Atom == 'constraint x = ((a * 2) + 3);\n')) :-
		minizinc_program::serialize([
			constraint(op('=', id(x), op('+', op('*', id(a), int(2)), int(3))))
		], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_comprehension_forall, deterministic(Atom == 'constraint forall(i in 1..3)(a[i] > 0);\n')) :-
		minizinc_program::serialize([
			constraint(comprehension(call(forall), op('>', index(id(a), [id(i)]), int(0)), [
				generator([i], range(int(1), int(3)), none)
			]))
		], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_if_then_else, deterministic(Atom == 'var bool: flag = if true then 1 else 0 endif;\n')) :-
		minizinc_program::serialize([
			decl(var(bool), flag, if_then_else([bool(true)-int(1)], int(0)))
		], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_if_then_no_else, deterministic(Atom == 'constraint if true then true endif;\n')) :-
		minizinc_program::serialize([
			constraint(if_then_else([bool(true)-bool(true)], none))
		], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_enum_named_cases, deterministic(Atom == 'enum shift = {day,evening,night};\n')) :-
		minizinc_program::serialize([enum(shift, cases([day, evening, night]))], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_enum_anonymous, deterministic(Atom == 'enum worker = _(1..3);\n')) :-
		minizinc_program::serialize([enum(worker, anon(range(int(1), int(3))))], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_enum_declared, deterministic(Atom == 'enum shift;\n')) :-
		minizinc_program::serialize([enum(shift, declared)], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_enum_index_set, deterministic(Atom == 'array[worker] of var shift: assigned;\n')) :-
		minizinc_program::serialize([
			array_decl(var(id(shift)), [id(worker)], assigned, none)
		], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_include, deterministic(Atom == 'include "alldifferent.mzn";\n')) :-
		minizinc_program::serialize([include('alldifferent.mzn')], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_raw, deterministic(Atom == '% raw comment\n')) :-
		atom_codes('% raw comment', RawCodes),
		minizinc_program::serialize([raw(RawCodes)], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_string_escaping, deterministic(Atom == 'string: greeting = "hi\\nthere";\n')) :-
		atom_codes('hi\nthere', GreetingCodes),
		minizinc_program::serialize([decl(par(string), greeting, string(GreetingCodes))], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_solve_goals, deterministic(Atom == 'solve satisfy;\nsolve minimize x;\nsolve maximize x;\n')) :-
		minizinc_program::serialize([solve(satisfy), solve(minimize(id(x))), solve(maximize(id(x)))], Codes),
		atom_codes(Atom, Codes).

	test(minizinc_program_invalid_identifier, error(domain_error(minizinc_statement, _))) :-
		minizinc_program::serialize([decl(par(int), enum, int(1))], _).

	test(minizinc_program_invalid_statement, error(domain_error(minizinc_statement, _))) :-
		minizinc_program::serialize([not_a_statement], _).

	test(minizinc_program_source_map, deterministic(SourceMap == [line(1, 1, unknown), line(2, 2, source(f, 10, 12))])) :-
		minizinc_program::serialize(
			[decl(par(int), n, int(1)), decl(par(int), m, int(2))],
			[unknown, source(f, 10, 12)],
			_, SourceMap
		).

	% -----------------------------------------------------------------------
	% minizinc_output: JSON-stream message decoding
	% -----------------------------------------------------------------------

	test(minizinc_output_decode_optimal, true) :-
		fixture_codes('optimal.jsonl', Codes),
		minizinc_output::decode(Codes, exit(0), exit(0), [], Result),
		Result = minizinc_result(satisfiable, [solution(Values, Objective, Time)], Properties),
		assertion(Values == [x-9, y-1, '_objective'-9]),
		assertion(Objective == 9),
		assertion(Time == 249),
		assertion(member(solver_status(optimal_solution), Properties)),
		assertion(member(optimality(proven), Properties)),
		assertion(member(enumeration_complete(true), Properties)).

	test(minizinc_output_decode_unsatisfiable, true) :-
		fixture_codes('unsatisfiable.jsonl', Codes),
		minizinc_output::decode(Codes, exit(0), exit(0), [], Result),
		Result = minizinc_result(unsatisfiable, [], Properties),
		assertion(member(solver_status(unsatisfiable), Properties)),
		assertion(member(warnings([warning(_, _)]), Properties)).

	test(minizinc_output_decode_all_solutions, true) :-
		fixture_codes('all_solutions.jsonl', Codes),
		minizinc_output::decode(Codes, exit(0), exit(0), [], Result),
		Result = minizinc_result(satisfiable, Solutions, Properties),
		assertion(length(Solutions, 3)),
		assertion(member(solver_status(all_solutions), Properties)),
		assertion(member(enumeration_complete(true), Properties)).

	test(minizinc_output_decode_error, true) :-
		fixture_codes('syntax_error.jsonl', Codes),
		minizinc_output::decode(Codes, exit(1), exit(1), [], Result),
		Result = minizinc_result(error, [], Properties),
		assertion(member(errors([error('syntax error', _, location(_, 1, 11, 1, 11))]), Properties)).

	test(minizinc_output_decode_malformed_json, error(domain_error(minizinc_output, _))) :-
		fixture_codes('malformed.jsonl', Codes),
		minizinc_output::decode(Codes, exit(1), exit(1), [], _).

	test(minizinc_output_decode_safety_net, error(domain_error(minizinc_process_status, exit(125)))) :-
		minizinc_output::decode([], exit(125), exit(125), [], _).

	% -----------------------------------------------------------------------
	% minizinc_expansion and problem fixture objects
	% -----------------------------------------------------------------------

	test(minizinc_expansion_queens_statements, true) :-
		findall(Statement, minizinc_queens::minizinc_statement(Statement, _), Statements),
		assertion(member(include('alldifferent.mzn'), Statements)),
		assertion(member(decl(par(int), n, int(8)), Statements)),
		assertion(member(solve(satisfy), Statements)).

	test(minizinc_program_generation_queens, true) :-
		minizinc::program(object(minizinc_queens), Codes),
		atom_codes(Atom, Codes),
		assertion(sub_atom(Atom, _, _, _, 'alldifferent(q)')),
		assertion(sub_atom(Atom, _, _, _, 'solve satisfy;')).

	test(minizinc_program_generation_shift_schedule, true) :-
		minizinc::program(object(minizinc_shift_schedule), Codes),
		atom_codes(Atom, Codes),
		assertion(sub_atom(Atom, _, _, _, 'enum shift = {day,evening,night};')),
		assertion(sub_atom(Atom, _, _, _, 'enum worker = _(1..3);')).

	% -----------------------------------------------------------------------
	% minizinc facade: result accessors (no solver process involved)
	% -----------------------------------------------------------------------

	test(minizinc_model_enumeration, deterministic(Solutions == [solution([x-1], none, none), solution([x-2], none, none)])) :-
		findall(
			Solution,
			minizinc::model(
				minizinc_result(satisfiable, [solution([x-1], none, none), solution([x-2], none, none)], []),
				Solution
			),
			Solutions
		).

	test(minizinc_result_property_enumeration, true) :-
		findall(
			Property,
			minizinc::result_property(
				minizinc_result(satisfiable, [], [solver_status(optimal_solution), optimality(proven)]),
				Property
			),
			Properties
		),
		assertion(Properties == [status(satisfiable), solver_status(optimal_solution), optimality(proven)]).

	test(minizinc_result_property_invalid, error(domain_error(minizinc_result, _))) :-
		minizinc::result_property(not_a_result, _).

	% -----------------------------------------------------------------------
	% minizinc facade: option validation
	% -----------------------------------------------------------------------

	test(minizinc_default_options, deterministic) :-
		assertion(minizinc::default_option(solver(gecode))),
		assertion(minizinc::default_option(executable(minizinc))),
		assertion(minizinc::default_option(all_solutions(false))),
		assertion(minizinc::default_option(data(none))),
		assertion(minizinc::default_option(json_data([]))).

	test(minizinc_valid_options, deterministic) :-
		assertion(minizinc::valid_option(solver(chuffed))),
		assertion(minizinc::valid_option(time_limit(1000))),
		assertion(minizinc::valid_option(all_solutions(true))),
		assertion(minizinc::valid_option(num_solutions(5))),
		assertion(minizinc::valid_option(random_seed(auto))),
		assertion(minizinc::valid_option(parallel(4))),
		assertion(minizinc::valid_option(data(none))),
		assertion(minizinc::valid_option(json_data([capacity-int(10)]))).

	test(minizinc_invalid_options, deterministic) :-
		assertion(\+ minizinc::valid_option(solver(''))),
		assertion(\+ minizinc::valid_option(time_limit(-1))),
		assertion(\+ minizinc::valid_option(num_solutions(-1))),
		assertion(\+ minizinc::valid_option(json_data([capacity-op('+', int(1), int(2))]))).

	test(minizinc_solve_invalid_option, error(domain_error(option, time_limit(-1)))) :-
		minizinc::solve(codes([]), _, [time_limit(-1)]).

	test(minizinc_solve_missing_executable, error(existence_error(command, logtalk_minizinc_missing_executable))) :-
		minizinc::solve(codes([]), _, [executable(logtalk_minizinc_missing_executable)]).

	% -----------------------------------------------------------------------
	% minizinc facade: solving (conditional on the MiniZinc CLI being available)
	% -----------------------------------------------------------------------

	test(minizinc_solve_optimize_gecode, deterministic(Status == satisfiable), [condition(resolve_command_path(minizinc, _))]) :-
		check_solve_optimize(gecode, Status).

	test(minizinc_solve_optimize_cbc, deterministic(Status == satisfiable), [condition(resolve_command_path(minizinc, _))]) :-
		check_solve_optimize(cbc, Status).

	test(minizinc_solve_queens, true, [condition(resolve_command_path(minizinc, _))]) :-
		minizinc::solve(object(minizinc_queens), Result, [solver(gecode)]),
		Result = minizinc_result(satisfiable, [solution(Values, _, _)], _),
		member('q'-Positions, Values),
		length(Positions, 8),
		msort(Positions, Sorted),
		assertion(Sorted == [1, 2, 3, 4, 5, 6, 7, 8]).

	test(minizinc_solve_knapsack_json_data, deterministic(Objective == 15), [condition(resolve_command_path(minizinc, _))]) :-
		minizinc::solve(object(minizinc_knapsack), Result, [solver(gecode), json_data([capacity-int(10)])]),
		Result = minizinc_result(satisfiable, [solution(_, Objective, _)], _).

	test(minizinc_solve_knapsack_data_file, deterministic(Objective == 15), [condition(resolve_command_path(minizinc, _))]) :-
		^^file_path('test_files/knapsack_data.json', Path),
		minizinc::solve(object(minizinc_knapsack), Result, [solver(gecode), data(Path)]),
		Result = minizinc_result(satisfiable, [solution(_, Objective, _)], _).

	test(minizinc_solve_shift_schedule, true, [condition(resolve_command_path(minizinc, _))]) :-
		minizinc::solve(object(minizinc_shift_schedule), Result, [solver(gecode)]),
		Result = minizinc_result(satisfiable, [solution(Values, _, _)], _),
		member(assigned-Shifts, Values),
		msort(Shifts, Sorted),
		assertion(Sorted == [enum(day), enum(evening), enum(night)]).

	test(minizinc_solve_all_solutions, deterministic(Count == 3), [condition(resolve_command_path(minizinc, _))]) :-
		atom_codes('var 1..3: x; constraint x > 0; solve satisfy;', Program),
		minizinc::solve(codes(Program), Result, [solver(gecode), all_solutions(true)]),
		findall(Solution, minizinc::model(Result, Solution), Solutions),
		length(Solutions, Count),
		once(minizinc::result_property(Result, enumeration_complete(true))).

	test(minizinc_solve_unsatisfiable, deterministic(Status == unsatisfiable), [condition(resolve_command_path(minizinc, _))]) :-
		atom_codes('var 1..2: x; constraint x > 10; solve satisfy;', Program),
		minizinc::solve(codes(Program), minizinc_result(Status, [], _), [solver(gecode)]).

	test(minizinc_solve_syntax_diagnostics, deterministic, [condition(resolve_command_path(minizinc, _))]) :-
		workspace_names(Before),
		atom_codes('var 1..10 x solve satisfy;', Program),
		minizinc::solve(codes(Program), Result, [solver(gecode)]),
		Result = minizinc_result(error, [], Properties),
		assertion(member(errors([error('syntax error', _, _)]), Properties)),
		workspace_names(After),
		assertion(Before == After).

	test(minizinc_transport_cleanup_success, deterministic, [condition(resolve_command_path(minizinc, _))]) :-
		workspace_names(Before),
		check_solve_optimize(gecode, satisfiable),
		workspace_names(After),
		assertion(Before == After).

	test(minizinc_print_result, true) :-
		with_output_to_atom(
			minizinc::print_result(minizinc_result(satisfiable, [solution([x-1], none, none)], [])),
			Atom
		),
		assertion(sub_atom(Atom, _, _, _, 'x = 1;')),
		assertion(sub_atom(Atom, _, _, _, '==========')).

	% -----------------------------------------------------------------------
	% Auxiliary predicates
	% -----------------------------------------------------------------------

	check_solve_optimize(Solver, Status) :-
		minizinc::solve(terms([
			decl(var(range(int(1), int(10))), x, none),
			decl(var(range(int(1), int(10))), y, none),
			constraint(op('=', op('+', id(x), id(y)), int(10))),
			solve(maximize(id(x)))
		]), Result, [solver(Solver)]),
		Result = minizinc_result(Status, [solution(Values, Objective, _)], _),
		assertion(member(x-9, Values)),
		assertion(Objective == 9).

	fixture_codes(Name, Codes) :-
		atom_concat('test_files/', Name, Relative),
		^^file_path(Relative, Path),
		file_to_codes(Path, Codes).

	workspace_names(Names) :-
		temporary_directory(Temporary),
		directory_files(Temporary, Files),
		findall(
			Name,
			(	member(Name, Files),
				sub_atom(Name, 0, _, _, 'logtalk-minizinc')
			),
			Unsorted
		),
		sort(Unsorted, Names).

	with_output_to_atom(Goal, Atom) :-
		^^file_path('mzn_print_result.tmp', Path),
		open(Path, write, Stream),
		catch(
			(	current_output(Current),
				set_output(Stream),
				catch(call(Goal), Error, (set_output(Current), close(Stream), throw(Error))),
				set_output(Current),
				close(Stream)
			),
			Error2,
			(	catch(close(Stream), _, true),
				throw(Error2)
			)
		),
		file_to_codes(Path, Codes),
		atom_codes(Atom, Codes),
		delete_file(Path).

:- end_object.
