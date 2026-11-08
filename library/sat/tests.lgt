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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-08,
		comment is 'Unit tests for the sat library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(os, [
		directory_files/2, operating_system_type/1, resolve_command_path/2, temporary_directory/1
	]).

	:- uses(list, [
		member/2, memberchk/2, msort/2, length/2
	]).

	cover(sat_program).
	cover(sat_output).
	cover(sat_expansion).
	cover(sat).
	cover(sat_process).

	% sat_program tests

	test(sat_program_empty, deterministic(Atom == 'p cnf 0 0\n')) :-
		sat_program::serialize([], Codes),
		atom_codes(Atom, Codes).

	test(sat_program_unit_clause, deterministic(Atom == 'p cnf 1 1\n1 0\n')) :-
		sat_program::serialize([clause([pos(p)])], Codes),
		atom_codes(Atom, Codes).

	test(sat_program_multi_literal, deterministic(Atom == 'p cnf 2 1\n1 -2 0\n')) :-
		sat_program::serialize([clause([pos(p), neg(q)])], Codes),
		atom_codes(Atom, Codes).

	test(sat_program_variable_order, deterministic(Atom == 'p cnf 3 2\n1 2 0\n-2 3 0\n')) :-
		sat_program::serialize([clause([pos(p), pos(q)]), clause([neg(q), pos(r)])], Codes),
		atom_codes(Atom, Codes).

	test(sat_program_repeated_term, deterministic(Atom == 'p cnf 1 2\n1 0\n-1 0\n')) :-
		sat_program::serialize([clause([pos(p)]), clause([neg(p)])], Codes),
		atom_codes(Atom, Codes).

	test(sat_program_empty_clause, deterministic(Atom == 'p cnf 0 1\n0\n')) :-
		sat_program::serialize([clause([])], Codes),
		atom_codes(Atom, Codes).

	test(sat_program_comment, deterministic(Atom == 'p cnf 0 0\nc hello\n')) :-
		atom_codes(hello, HelloCodes),
		sat_program::serialize([comment(HelloCodes)], Codes),
		atom_codes(Atom, Codes).

	test(sat_program_raw, deterministic(Atom == 'p cnf 0 0\nc trusted\n')) :-
		atom_codes('c trusted', RawCodes),
		sat_program::serialize([raw(RawCodes)], Codes),
		atom_codes(Atom, Codes).

	test(sat_program_source_map, deterministic) :-
		atom_codes(hello, HelloCodes),
		sat_program::serialize([comment(HelloCodes), clause([pos(p)])], [source('fixture.lgt', 3, 4), unknown], Codes, Map),
		atom_codes(Atom, Codes),
		assertion(Atom == 'p cnf 1 1\nc hello\n1 0\n'),
		assertion(Map == [line(2, 2, source('fixture.lgt', 3, 4)), line(3, 3, unknown)]).

	test(sat_program_variables, deterministic(Variables == [p-1, q-2])) :-
		sat_program::variables([clause([pos(p), neg(q)])], Variables).

	test(sat_program_unbound, error(instantiation_error)) :-
		sat_program::serialize(_, _).

	test(sat_program_non_ground, error(instantiation_error)) :-
		sat_program::serialize([clause([pos(_)])], _).

	test(sat_program_non_list, error(type_error(list, invalid))) :-
		sat_program::serialize(invalid, _).

	test(sat_program_invalid_statement, error(domain_error(sat_statement, call(halt)))) :-
		sat_program::serialize([call(halt)], _).

	test(sat_program_invalid_literal, error(domain_error(sat_statement, clause([p])))) :-
		sat_program::serialize([clause([p])], _).

	test(sat_program_invalid_comment_newline, error(domain_error(sat_statement, comment([98,97,100,10,120])))) :-
		sat_program::serialize([comment([98,97,100,10,120])], _).

	test(sat_program_invalid_origins, error(domain_error(sat_origins, [wrong]))) :-
		sat_program::serialize([clause([pos(p)])], [wrong], _, _).

	% sat_expansion tests

	test(sat_expansion_trivial_sat, deterministic(Statements == [clause([pos(p)]), clause([neg(q), pos(p)])])) :-
		findall(Statement, sat_trivial_sat::cnf_statement(Statement, _), Statements).

	test(sat_expansion_pigeonhole, deterministic(Statements == [clause([pos(in(1))]), clause([pos(in(2))]), clause([neg(in(1)), neg(in(2))])])) :-
		findall(Statement, sat_pigeonhole_unsat::cnf_statement(Statement, _), Statements).

	test(sat_expansion_empty_clause, deterministic(Statements == [clause([pos(p)]), clause([])])) :-
		findall(Statement, sat_empty_clause_fixture::cnf_statement(Statement, _), Statements).

	test(sat_expansion_raw, deterministic) :-
		findall(Statement, sat_raw_fixture::cnf_statement(Statement, _), Statements),
		atom_codes('c trusted raw comment', Expected),
		assertion(Statements == [raw(Expected), raw(Expected), raw(Expected), raw(Expected)]).

	test(sat_expansion_comment, deterministic) :-
		findall(Statement, sat_comment_fixture::cnf_statement(Statement, _), Statements),
		atom_codes('a readable comment', Expected),
		assertion(Statements == [comment(Expected), clause([pos(p)])]).

	test(sat_expansion_unrelated, deterministic(Value == ordinary)) :-
		sat_unrelated_fixture::value(Value),
		assertion(\+ sat_unrelated_fixture::current_predicate(cnf_statement/2)).

	% sat tests (program generation and printing)

	test(sat_program_object_source, deterministic(Codes == Expected)) :-
		sat::program(object(sat_trivial_sat), Codes),
		findall(Statement, sat_trivial_sat::cnf_statement(Statement, _), Statements),
		sat::program(terms(Statements), Expected).

	test(sat_program_codes_source, deterministic(Codes == [112, 46, 10])) :-
		sat::program(codes([112, 46, 10]), Codes).

	test(sat_program_wrong_object, error(domain_error(sat_source, object(sat_unrelated_fixture)))) :-
		sat::program(object(sat_unrelated_fixture), _).

	test(sat_program_print_codes, deterministic) :-
		atom_codes('p cnf 1 1\n1 0\n', Codes),
		^^set_text_output(''),
		sat::program(codes(Codes)),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p cnf 1 1\n1 0\n').

	test(sat_program_print_terms, deterministic) :-
		^^set_text_output(''),
		sat::program(terms([clause([pos(p)])])),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p cnf 1 1\n1 0\n').

	test(sat_program_print_object, deterministic) :-
		sat::program(object(sat_queens), Codes),
		atom_codes(Expected, Codes),
		^^set_text_output(''),
		sat::program(object(sat_queens)),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == Expected).

	test(sat_program_print_invalid, error(domain_error(sat_source, invalid))) :-
		sat::program(invalid).

	test(sat_program_print_unbound, error(instantiation_error)) :-
		sat::program(_).

	% sat_output tests

	test(sat_output_sat, deterministic) :-
		atom_codes('s SATISFIABLE\nv 1 -2 0\n', Codes),
		sat_output::decode(kissat, Codes, exit(10), exit(10), [], [], [p-1, q-2], Result),
		Result = sat_result(Status, Model, Properties),
		assertion(Status == satisfiable),
		assertion(Model == assignment([pos(p), neg(q)])),
		assertion(member(termination(completed), Properties)),
		assertion(member(variable_count(2), Properties)),
		assertion(member(solver(kissat), Properties)).

	test(sat_output_sat_multiline_witness, deterministic(Model == assignment([pos(p), neg(q), pos(r)]))) :-
		atom_codes('s SATISFIABLE\nv 1 -2\nv 3 0\n', Codes),
		sat_output::decode(kissat, Codes, exit(10), exit(10), [], [], [p-1, q-2, r-3], Result),
		Result = sat_result(_, Model, _).

	test(sat_output_sat_identity, deterministic(Model == assignment([pos(1), neg(2)]))) :-
		atom_codes('s SATISFIABLE\nv 1 -2 0\n', Codes),
		sat_output::decode(kissat, Codes, exit(10), exit(10), [], [], none, Result),
		Result = sat_result(_, Model, _).

	test(sat_output_sat_no_witness, deterministic(Model == none)) :-
		atom_codes('s SATISFIABLE\n', Codes),
		sat_output::decode(kissat, Codes, exit(10), exit(10), [], [], none, Result),
		Result = sat_result(_, Model, _).

	test(sat_output_unsat, deterministic(Status-Model == unsatisfiable-none)) :-
		atom_codes('s UNSATISFIABLE\n', Codes),
		sat_output::decode(kissat, Codes, exit(20), exit(20), [], [], none, Result),
		Result = sat_result(Status, Model, _).

	test(sat_output_unknown, deterministic) :-
		atom_codes('s UNKNOWN\n', Codes),
		sat_output::decode(kissat, Codes, exit(0), exit(0), [], [], none, Result),
		Result = sat_result(Status, Model, Properties),
		assertion(Status == unknown),
		assertion(Model == none),
		assertion(member(termination(resource_limit), Properties)).

	test(sat_output_cadical_unknown, deterministic) :-
		atom_codes('c UNKNOWN\n', Codes),
		sat_output::decode(cadical, Codes, exit(0), exit(0), [], [], none, Result),
		Result = sat_result(Status, Model, Properties),
		assertion(Status == unknown),
		assertion(Model == none),
		assertion(member(solver(cadical), Properties)).

	test(sat_output_missing_status, error(domain_error(sat_output, 'Result'-missing))) :-
		atom_codes('c just a comment\n', Codes),
		sat_output::decode(kissat, Codes, exit(10), exit(10), [], [], none, _).

	test(sat_output_cadical_unknown_not_for_other_solvers, error(domain_error(sat_output, 'Result'-missing))) :-
		atom_codes('c UNKNOWN\n', Codes),
		sat_output::decode(kissat, Codes, exit(0), exit(0), [], [], none, _).

	% Z3 exits 0 regardless of the reported status; exit(0) is accepted for all
	% three status labels, with the status label alone distinguishing completed
	% from resource-limited results.
	test(sat_output_z3_satisfiable_exit_zero, deterministic(Status-Termination == satisfiable-completed)) :-
		atom_codes('s SATISFIABLE\nv 1 0\n', Codes),
		sat_output::decode(z3, Codes, exit(0), exit(0), [], [], [p-1], Result),
		Result = sat_result(Status, _, Properties),
		memberchk(termination(Termination), Properties).

	test(sat_output_z3_unsatisfiable_exit_zero, deterministic(Status-Termination == unsatisfiable-completed)) :-
		atom_codes('s UNSATISFIABLE\n', Codes),
		sat_output::decode(z3, Codes, exit(0), exit(0), [], [], none, Result),
		Result = sat_result(Status, _, Properties),
		memberchk(termination(Termination), Properties).

	test(sat_output_z3_unknown_exit_zero, deterministic(Status-Termination == unknown-resource_limit)) :-
		atom_codes('s UNKNOWN\n', Codes),
		sat_output::decode(z3, Codes, exit(0), exit(0), [], [], none, Result),
		Result = sat_result(Status, _, Properties),
		memberchk(termination(Termination), Properties).

	test(sat_output_z3_nonzero_exit_fatal, error(domain_error(sat_process_status, exit(10)))) :-
		atom_codes('s SATISFIABLE\nv 1 0\n', Codes),
		sat_output::decode(z3, Codes, exit(10), exit(10), [], [], [p-1], _).

	test(sat_output_invalid_label, error(domain_error(sat_output, 'Result'-'MAYBE'))) :-
		atom_codes('s MAYBE\n', Codes),
		sat_output::decode(kissat, Codes, exit(10), exit(10), [], [], none, _).

	test(sat_output_missing_terminator, error(domain_error(sat_output, witness_terminator_missing))) :-
		atom_codes('s SATISFIABLE\nv 1 2\n', Codes),
		sat_output::decode(kissat, Codes, exit(10), exit(10), [], [], [p-1, q-2], _).

	test(sat_output_unknown_variable, error(domain_error(sat_output, variable(3)))) :-
		atom_codes('s SATISFIABLE\nv 3 0\n', Codes),
		sat_output::decode(kissat, Codes, exit(10), exit(10), [], [], [p-1, q-2], _).

	test(sat_output_inconsistent_status, error(domain_error(sat_process_status, exit(10)))) :-
		atom_codes('s UNSATISFIABLE\n', Codes),
		sat_output::decode(kissat, Codes, exit(10), exit(10), [], [], none, _).

	test(sat_output_fatal, error(domain_error(sat_process_status, exit(65)))) :-
		atom_codes('s SATISFIABLE\nv 1 0\n', Codes),
		sat_output::decode(kissat, Codes, exit(65), exit(65), [], [], [p-1], _).

	test(sat_output_interrupted, deterministic(Termination == interrupted)) :-
		atom_codes('s UNKNOWN\n', Codes),
		sat_output::decode(kissat, Codes, killed(2), killed(2), [], [], none, Result),
		Result = sat_result(_, _, Properties),
		memberchk(termination(Termination), Properties).

	test(sat_output_unbound, error(instantiation_error)) :-
		sat_output::decode(kissat, _, exit(10), exit(10), [], [], none, _).

	test(sat_output_invalid_variables_type, error(type_error(list(pair), invalid))) :-
		atom_codes('s SATISFIABLE\nv 1 0\n', Codes),
		sat_output::decode(kissat, Codes, exit(10), exit(10), [], [], invalid, _).

	% sat result accessor tests

	test(sat_result_literal, deterministic(Literals == [pos(p), neg(q)])) :-
		Result = sat_result(satisfiable, assignment([pos(p), neg(q)]), []),
		findall(Literal, sat::literal(Result, Literal), Literals).

	test(sat_result_literal_none, deterministic(Literals == [])) :-
		Result = sat_result(unsatisfiable, none, []),
		findall(Literal, sat::literal(Result, Literal), Literals).

	test(sat_result_property) :-
		Result = sat_result(satisfiable, assignment([pos(p)]), [solver(kissat), variable_count(1)]),
		sat::result_property(Result, status(Status)),
		assertion(Status == satisfiable),
		sat::result_property(Result, variable_count(Count)),
		assertion(Count == 1).

	test(sat_result_invalid, error(domain_error(sat_result, invalid))) :-
		sat::literal(invalid, _).

	test(sat_result_unbound, error(instantiation_error)) :-
		sat::literal(_, _).

	% sat print_result tests

	test(sat_print_result_satisfiable, deterministic) :-
		Result = sat_result(satisfiable, assignment([pos(p), neg(q)]), [variables([p-1, q-2])]),
		assert_printed_result(Result, 's SATISFIABLE\nv 1 -2 0\n').

	test(sat_print_result_satisfiable_identity, deterministic) :-
		Result = sat_result(satisfiable, assignment([pos(1), neg(2)]), [variables(none)]),
		assert_printed_result(Result, 's SATISFIABLE\nv 1 -2 0\n').

	test(sat_print_result_satisfiable_no_witness, deterministic) :-
		Result = sat_result(satisfiable, none, []),
		assert_printed_result(Result, 's SATISFIABLE\n').

	test(sat_print_result_unsatisfiable, deterministic) :-
		Result = sat_result(unsatisfiable, none, []),
		assert_printed_result(Result, 's UNSATISFIABLE\n').

	test(sat_print_result_unknown, deterministic) :-
		Result = sat_result(unknown, none, []),
		assert_printed_result(Result, 's UNKNOWN\n').

	test(sat_print_result_invalid, error(domain_error(sat_result, invalid))) :-
		sat::print_result(invalid).

	test(sat_print_result_unbound, error(instantiation_error)) :-
		sat::print_result(_).

	% sat print_program tests

	test(sat_print_program_empty, deterministic) :-
		^^set_text_output(''),
		sat::print_program([]),
		^^text_output_contents(Chars),
		assertion(Chars == []).

	test(sat_print_program_layout, deterministic) :-
		atom_codes('p cnf 1 1\n1 0\n', Codes),
		^^set_text_output(''),
		sat::print_program(Codes),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p cnf 1 1\n1 0\n').

	test(sat_print_program_invalid, error(type_error(integer, p))) :-
		sat::print_program([p]).

	test(sat_print_program_invalid_code, error(domain_error(character_code, -1))) :-
		sat::print_program([112, -1]).

	test(sat_print_program_unbound, error(instantiation_error)) :-
		sat::print_program(_).

	test(sat_print_program_partial, error(instantiation_error)) :-
		sat::print_program([112| _]).

	% process transport tests

	test(sat_shell_posix_literal, deterministic(Codes == [39,97,39,34,39,34,39,98,59,36,40,41,39])) :-
		atom_codes(Argument, [97,39,98,59,36,40,41]),
		sat_process::quote(posix, Argument, Quoted),
		atom_codes(Quoted, Codes).

	test(sat_shell_windows_literal, deterministic(Codes == [39,97,39,39,98,59,36,40,41,39])) :-
		atom_codes(Argument, [97,39,98,59,36,40,41]),
		sat_process::quote(windows, Argument, Quoted),
		atom_codes(Quoted, Codes).

	test(sat_transport_large_captures) :-
		workspace_names(Before),
		transport_fixture(capture, Executable, Arguments, Path),
		sat_process::run(Executable, Arguments, file(Path), Output, Diagnostics, _, Status),
		length(Output, OutputLength),
		length(Diagnostics, ErrorLength),
		assertion(OutputLength == 131072),
		assertion(ErrorLength == 131072),
		assertion(Status == exit(0)),
		workspace_names(After),
		assertion(Before == After).

	test(sat_transport_failed_child_cleanup, deterministic) :-
		workspace_names(Before),
		transport_fixture(failure, Executable, Arguments, Path),
		sat_process::run(Executable, Arguments, file(Path), _, Diagnostics, _, Status),
		assertion(Status \== exit(0)),
		assertion(Diagnostics \== []),
		workspace_names(After),
		assertion(Before == After).

	% external Kissat, CaDiCaL, Glucose, and Z3 CLI solving tests (conditional on availability)

	test(sat_solve_trivial_sat_kissat, deterministic, [condition(resolve_command_path(kissat, _))]) :-
		check_solve_trivial_sat(kissat).

	test(sat_solve_trivial_sat_cadical, deterministic, [condition(resolve_command_path(cadical, _))]) :-
		check_solve_trivial_sat(cadical).

	test(sat_solve_trivial_sat_glucose, deterministic, [condition(resolve_command_path('glucose-syrup', _))]) :-
		check_solve_trivial_sat(glucose).

	test(sat_solve_trivial_sat_z3, deterministic, [condition(resolve_command_path(z3, _))]) :-
		check_solve_trivial_sat(z3).

	test(sat_solve_pigeonhole_object_kissat, deterministic(Status-Model == unsatisfiable-none), [condition(resolve_command_path(kissat, _))]) :-
		sat::solve(object(sat_pigeonhole_unsat), sat_result(Status, Model, _), [solver(kissat)]).

	test(sat_solve_pigeonhole_object_cadical, deterministic(Status-Model == unsatisfiable-none), [condition(resolve_command_path(cadical, _))]) :-
		sat::solve(object(sat_pigeonhole_unsat), sat_result(Status, Model, _), [solver(cadical)]).

	test(sat_solve_pigeonhole_object_glucose, deterministic(Status-Model == unsatisfiable-none), [condition(resolve_command_path('glucose-syrup', _))]) :-
		sat::solve(object(sat_pigeonhole_unsat), sat_result(Status, Model, _), [solver(glucose)]).

	test(sat_solve_pigeonhole_object_z3, deterministic(Status-Model == unsatisfiable-none), [condition(resolve_command_path(z3, _))]) :-
		sat::solve(object(sat_pigeonhole_unsat), sat_result(Status, Model, _), [solver(z3)]).

	test(sat_solve_pigeonhole_file_kissat, deterministic(Status-Model == unsatisfiable-none), [condition(resolve_command_path(kissat, _))]) :-
		check_solve_pigeonhole_file(kissat, Status, Model).

	test(sat_solve_pigeonhole_file_cadical, deterministic(Status-Model == unsatisfiable-none), [condition(resolve_command_path(cadical, _))]) :-
		check_solve_pigeonhole_file(cadical, Status, Model).

	test(sat_solve_pigeonhole_file_glucose, deterministic(Status-Model == unsatisfiable-none), [condition(resolve_command_path('glucose-syrup', _))]) :-
		check_solve_pigeonhole_file(glucose, Status, Model).

	test(sat_solve_pigeonhole_file_z3, deterministic(Status-Model == unsatisfiable-none), [condition(resolve_command_path(z3, _))]) :-
		check_solve_pigeonhole_file(z3, Status, Model).

	% Glucose exposes no conflict or decision budget, so the resource-limit result
	% (a graceful "unknown" status) is not exercised for Glucose; Z3 exposes a
	% conflict budget ("sat.max_conflicts="), so it is exercised like Kissat and CaDiCaL.
	test(sat_solve_resource_limit_kissat, deterministic, [condition(resolve_command_path(kissat, _))]) :-
		check_solve_resource_limit(kissat).

	test(sat_solve_resource_limit_cadical, deterministic, [condition(resolve_command_path(cadical, _))]) :-
		check_solve_resource_limit(cadical).

	test(sat_solve_resource_limit_z3, deterministic, [condition(resolve_command_path(z3, _))]) :-
		check_solve_resource_limit(z3).

	test(sat_solve_witness_false_kissat, deterministic(Status-Model == satisfiable-none), [condition(resolve_command_path(kissat, _))]) :-
		sat::solve(object(sat_trivial_sat), sat_result(Status, Model, _), [solver(kissat), witness(false)]).

	test(sat_solve_witness_false_cadical, deterministic(Status-Model == satisfiable-none), [condition(resolve_command_path(cadical, _))]) :-
		sat::solve(object(sat_trivial_sat), sat_result(Status, Model, _), [solver(cadical), witness(false)]).

	test(sat_solve_witness_false_glucose, deterministic(Status-Model == satisfiable-none), [condition(resolve_command_path('glucose-syrup', _))]) :-
		sat::solve(object(sat_trivial_sat), sat_result(Status, Model, _), [solver(glucose), witness(false)]).

	% Z3 always reports a satisfying assignment in DIMACS mode, so witness(false)
	% is validated but has no effect: the model stays populated, unlike the
	% other three solvers.
	test(sat_solve_witness_false_z3, deterministic(Status == satisfiable), [condition(resolve_command_path(z3, _))]) :-
		sat::solve(object(sat_trivial_sat), sat_result(Status, Model, _), [solver(z3), witness(false)]),
		assertion(Model \== none).

	test(sat_solve_four_queens_kissat, true, [condition(resolve_command_path(kissat, _))]) :-
		check_solve_four_queens(kissat).

	test(sat_solve_four_queens_cadical, true, [condition(resolve_command_path(cadical, _))]) :-
		check_solve_four_queens(cadical).

	test(sat_solve_four_queens_glucose, true, [condition(resolve_command_path('glucose-syrup', _))]) :-
		check_solve_four_queens(glucose).

	test(sat_solve_four_queens_z3, true, [condition(resolve_command_path(z3, _))]) :-
		check_solve_four_queens(z3).

	test(sat_solve_codes_source_kissat, deterministic(Status == unsatisfiable), [condition(resolve_command_path(kissat, _))]) :-
		check_solve_codes_source(kissat, Status).

	test(sat_solve_codes_source_cadical, deterministic(Status == unsatisfiable), [condition(resolve_command_path(cadical, _))]) :-
		check_solve_codes_source(cadical, Status).

	test(sat_solve_codes_source_glucose, deterministic(Status == unsatisfiable), [condition(resolve_command_path('glucose-syrup', _))]) :-
		check_solve_codes_source(glucose, Status).

	test(sat_solve_codes_source_z3, deterministic(Status == unsatisfiable), [condition(resolve_command_path(z3, _))]) :-
		check_solve_codes_source(z3, Status).

	test(sat_solve_terms_source_kissat, deterministic, [condition(resolve_command_path(kissat, _))]) :-
		check_solve_terms_source(kissat).

	test(sat_solve_terms_source_cadical, deterministic, [condition(resolve_command_path(cadical, _))]) :-
		check_solve_terms_source(cadical).

	test(sat_solve_terms_source_glucose, deterministic, [condition(resolve_command_path('glucose-syrup', _))]) :-
		check_solve_terms_source(glucose).

	test(sat_solve_terms_source_z3, deterministic, [condition(resolve_command_path(z3, _))]) :-
		check_solve_terms_source(z3).

	test(sat_solve_invalid_option, error(domain_error(option, time_limit(-1)))) :-
		sat::solve(codes([]), _, [time_limit(-1)]).

	test(sat_solve_invalid_solver, error(domain_error(option, solver(invalid)))) :-
		sat::solve(codes([]), _, [solver(invalid)]).

	test(sat_solve_missing_executable, error(existence_error(command, logtalk_sat_missing_executable))) :-
		sat::solve(codes([]), _, [executable(logtalk_sat_missing_executable)]).

	test(sat_solve_syntax_diagnostics_kissat, deterministic, [condition(resolve_command_path(kissat, _))]) :-
		check_solve_syntax_diagnostics(kissat).

	test(sat_solve_syntax_diagnostics_cadical, deterministic, [condition(resolve_command_path(cadical, _))]) :-
		check_solve_syntax_diagnostics(cadical).

	test(sat_solve_syntax_diagnostics_glucose, deterministic, [condition(resolve_command_path('glucose-syrup', _))]) :-
		check_solve_syntax_diagnostics(glucose).

	% Z3's DIMACS parser is lenient and does not reliably raise an error for the
	% malformed.cnf fixture used by check_solve_syntax_diagnostics/1 (it reports a
	% trivial "s SATISFIABLE" result instead); a missing input file is used instead
	% to exercise the sat_solver_error path and cleanup for Z3.
	test(sat_solve_missing_file_z3, deterministic, [condition(resolve_command_path(z3, _))]) :-
		check_solve_missing_file(z3).

	% auxiliary predicates

	check_solve_trivial_sat(Solver) :-
		sat::solve(object(sat_trivial_sat), Result, [solver(Solver)]),
		Result = sat_result(satisfiable, assignment(Literals), Properties),
		assertion(sat::literal(Result, pos(p))),
		assertion(length(Literals, 2)),
		assertion(member(source_map([_| _]), Properties)),
		assertion(member(variables([p-1, q-2]), Properties)),
		assertion(member(solver(Solver), Properties)).

	check_solve_pigeonhole_file(Solver, Status, Model) :-
		^^file_path('test_files/pigeon6.cnf', Path),
		sat::solve(file(Path), sat_result(Status, Model, _), [solver(Solver)]).

	check_solve_resource_limit(Solver) :-
		^^file_path('test_files/pigeon6.cnf', Path),
		sat::solve(file(Path), Result, [solver(Solver), conflicts(1)]),
		Result = sat_result(Status, Model, Properties),
		assertion(Status == unknown),
		assertion(Model == none),
		assertion(member(termination(resource_limit), Properties)).

	check_solve_four_queens(Solver) :-
		sat::solve(object(sat_queens), Result, [solver(Solver)]),
		Result = sat_result(Status, assignment(Literals), _),
		assertion(Status == satisfiable),
		findall(Row-Column, member(pos(queen(Row, Column)), Literals), Placements),
		assertion(valid_queens_solution(Placements)).

	check_solve_codes_source(Solver, Status) :-
		atom_codes('p cnf 1 2\n1 0\n-1 0\n', Program),
		sat::solve(codes(Program), sat_result(Status, _, _), [solver(Solver)]).

	check_solve_terms_source(Solver) :-
		sat::solve(terms([clause([pos(p)])]), Result, [solver(Solver)]),
		Result = sat_result(Status, assignment(Literals), Properties),
		assertion(Status == satisfiable),
		assertion(Literals == [pos(p)]),
		assertion(member(variables([p-1]), Properties)).

	check_solve_syntax_diagnostics(Solver) :-
		workspace_names(Before),
		^^file_path('test_files/malformed.cnf', Path),
		catch(sat::solve(file(Path), _, [solver(Solver)]), error(Formal, _), true),
		Formal = sat_solver_error(_, _, Diagnostics, []),
		assertion(Diagnostics \== []),
		workspace_names(After),
		assertion(Before == After).

	check_solve_missing_file(Solver) :-
		workspace_names(Before),
		^^file_path('test_files/does_not_exist.cnf', Path),
		catch(sat::solve(file(Path), _, [solver(Solver)]), error(Formal, _), true),
		Formal = sat_solver_error(_, _, Diagnostics, []),
		assertion(Diagnostics \== []),
		workspace_names(After),
		assertion(Before == After).

	assert_printed_result(Result, Expected) :-
		^^set_text_output(''),
		sat::print_result(Result),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == Expected).

	valid_queens_solution(Placements) :-
		length(Placements, 4),
		findall(Row, member(Row-_, Placements), Rows),
		msort(Rows, [1, 2, 3, 4]),
		findall(Column, member(_-Column, Placements), Columns),
		sort(Columns, SortedColumns),
		length(SortedColumns, 4),
		\+ (
			member(Row1-Column1, Placements),
			member(Row2-Column2, Placements),
			Row1 \== Row2,
			abs(Row1 - Row2) =:= abs(Column1 - Column2)
		).

	workspace_names(Names) :-
		temporary_directory(Temporary),
		directory_files(Temporary, Files),
		findall(
			Name,
			(	member(Name, Files),
				sub_atom(Name, 0, _, _, 'logtalk-sat')
			),
			Unsorted
		),
		sort(Unsorted, Names).

	transport_fixture(Name, Executable, Arguments, Path) :-
		atom_concat('test_files/', Name, Prefix),
		(	operating_system_type(windows) ->
			resolve_command_path(pwsh, Executable),
			Arguments = ['-NoProfile', '-NonInteractive', '-ExecutionPolicy', 'Bypass', '-File'],
			atom_concat(Prefix, '.ps1', Relative)
		;	Executable = '/bin/sh',
			Arguments = [],
			atom_concat(Prefix, '.sh', Relative)
		),
		^^file_path(Relative, Path).

:- end_object.
