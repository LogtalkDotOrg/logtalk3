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
		date is 2026-11-27,
		comment is 'Unit tests for the "max_sat" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		append/3, memberchk/2, nth1/3, subtract/3
	]).

	:- uses(numberlist, [
		min/2
	]).

	cover(max_sat).
	cover(max_sat_process).
	cover(max_sat_program).
	cover(max_sat_output).

	% max_sat_program tests

	test(max_sat_program_empty, deterministic(Atom == 'p wcnf 0 0 1\n')) :-
		max_sat_program::serialize([], Codes),
		atom_codes(Atom, Codes).

	test(max_sat_program_hard_and_soft, deterministic(Atom == 'p wcnf 2 2 4\n4 1 -2 0\n3 2 0\n')) :-
		max_sat_program::serialize([(+p ; -q), soft(3, q)], Codes),
		atom_codes(Atom, Codes).

	test(max_sat_program_bare_literals, deterministic(Atom == 'p wcnf 2 1 1\n1 1 2 0\n')) :-
		max_sat_program::serialize([(p ; q)], Codes),
		atom_codes(Atom, Codes).

	test(max_sat_program_soft_default_weight, deterministic(Atom == 'p wcnf 1 1 2\n1 1 0\n')) :-
		max_sat_program::serialize([soft(p)], Codes),
		atom_codes(Atom, Codes).

	test(max_sat_program_top_weight_sum, deterministic(Atom == 'p wcnf 3 3 11\n2 1 0\n3 2 0\n5 3 0\n')) :-
		max_sat_program::serialize([soft(2, p), soft(3, q), soft(5, r)], Codes),
		atom_codes(Atom, Codes).

	test(max_sat_program_hard_empty_clause, deterministic(Atom == 'p wcnf 0 1 1\n1 0\n')) :-
		max_sat_program::serialize([false], Codes),
		atom_codes(Atom, Codes).

	test(max_sat_program_soft_empty_clause, deterministic(Atom == 'p wcnf 0 1 3\n2 0\n')) :-
		max_sat_program::serialize([soft(2, false)], Codes),
		atom_codes(Atom, Codes).

	test(max_sat_program_comment_and_raw_not_counted, deterministic(Atom == 'p wcnf 1 1 1\nc hi\nraw line\n1 1 0\n')) :-
		max_sat_program::serialize([comment(hi), raw('raw line'), p], Codes),
		atom_codes(Atom, Codes).

	test(max_sat_program_new_format, deterministic(Atom == 'h 1 -2 0\n3 2 0\nh 0\n')) :-
		max_sat_program::serialize([(+p ; -q), soft(3, q), false], new, Codes),
		atom_codes(Atom, Codes).

	test(max_sat_program_old_format_explicit, deterministic(Atom == 'p wcnf 1 1 1\n1 1 0\n')) :-
		max_sat_program::serialize([p], old, Codes),
		atom_codes(Atom, Codes).

	test(max_sat_program_variables, deterministic(Variables == [p-1, q-2, r-3])) :-
		max_sat_program::variables([(+p ; -q), soft(2, (r ; p))], Variables).

	test(max_sat_program_variables_structured_terms, deterministic(Variables == [queen(1,2)-1, in(a)-2])) :-
		max_sat_program::variables([(queen(1,2) ; -in(a))], Variables).

	test(max_sat_program_top_weight, deterministic(Top == 6)) :-
		max_sat_program::top_weight([soft(2, p), soft(3, q), r], Top).

	test(max_sat_program_source_map_old, deterministic(Map == [line(2, 2, unknown), line(3, 3, source(f, 1, 5))])) :-
		max_sat_program::serialize([p, soft(1, q)], [unknown, source(f, 1, 5)], old, _, Map).

	test(max_sat_program_source_map_new, deterministic(Map == [line(1, 1, unknown), line(2, 2, unknown)])) :-
		max_sat_program::serialize([p, soft(1, q)], [unknown, unknown], new, _, Map).

	test(max_sat_program_invalid_origins, error(domain_error(max_sat_origins, [unknown]))) :-
		max_sat_program::serialize([p, q], [unknown], old, _, _).

	test(max_sat_program_zero_weight, error(domain_error(max_sat_statement, soft(0, p)))) :-
		max_sat_program::serialize([soft(0, p)], _).

	test(max_sat_program_negative_weight, error(domain_error(max_sat_statement, soft(-1, p)))) :-
		max_sat_program::serialize([soft(-1, p)], _).

	test(max_sat_program_non_integer_weight, error(domain_error(max_sat_statement, soft(1.5, p)))) :-
		max_sat_program::serialize([soft(1.5, p)], _).

	test(max_sat_program_atom_weight, error(domain_error(max_sat_statement, soft(a, p)))) :-
		max_sat_program::serialize([soft(a, p)], _).

	test(max_sat_program_invalid_format, error(domain_error(wcnf_format, foo))) :-
		max_sat_program::serialize([p], foo, _).

	test(max_sat_program_not_a_list, error(type_error(list, foo))) :-
		max_sat_program::serialize(foo, _).

	test(max_sat_program_partial_list, error(instantiation_error)) :-
		max_sat_program::serialize([p| _], _).

	test(max_sat_program_variable_statement, error(instantiation_error)) :-
		max_sat_program::serialize([_], _).

	test(max_sat_program_numeric_statement, error(domain_error(max_sat_statement, 1))) :-
		max_sat_program::serialize([1], _).

	% fixture objects (expanded using the max_sat_expansion hook)

	test(max_sat_expansion_statements, deterministic(Statements == [(x1 ; x2), (-x1 ; -x2), soft(3, x1), soft(2, x2), soft(1, x3)])) :-
		findall(Statement, max_sat_example::wcnf_statement(Statement, _), Statements).

	test(max_sat_expansion_origins, deterministic) :-
		forall(
			max_sat_example::wcnf_statement(_, Origin),
			assertion(( Origin = source(File, Start, End), atom(File), integer(Start), integer(End) ))
		).

	test(max_sat_expansion_directives, deterministic(Statements == [raw('c trusted raw comment'), comment('a readable comment')])) :-
		findall(Statement, max_sat_raw_fixture::wcnf_statement(Statement, _), Statements).

	test(max_sat_expansion_empty_clauses, deterministic(Statements == [p, false, soft(2, false)])) :-
		findall(Statement, max_sat_empty_clause_fixture::wcnf_statement(Statement, _), Statements).

	% max_sat_process quoting tests

	test(max_sat_process_quote_posix, deterministic(Quoted == '\'abc\'')) :-
		max_sat_process::quote(posix, abc, Quoted).

	test(max_sat_process_quote_posix_single_quote, deterministic(Quoted == '\'a\'"\'"\'b\'')) :-
		max_sat_process::quote(posix, 'a\'b', Quoted).

	test(max_sat_process_quote_windows_single_quote, deterministic(Quoted == '\'a\'\'b\'')) :-
		max_sat_process::quote(windows, 'a\'b', Quoted).

	test(max_sat_process_quote_invalid_platform, error(domain_error(max_sat_shell_platform, plan9))) :-
		max_sat_process::quote(plan9, abc, _).

	test(max_sat_process_quote_unbound_argument, error(instantiation_error)) :-
		max_sat_process::quote(posix, _, _).

	test(max_sat_process_run_invalid_source, error(domain_error(max_sat_process_source, foo))) :-
		max_sat_process::run(sh, [], foo, _, _, _, _).

	% max_sat program generation tests

	test(max_sat_program_object_embeds_variables, deterministic) :-
		max_sat::program(object(max_sat_example), Codes),
		atom_codes(Atom, Codes),
		assertion(sub_atom(Atom, 0, _, _, 'p wcnf 3 5 7\nc v 1 x1\nc v 2 x2\nc v 3 x3\n')).

	test(max_sat_program_object_clauses, deterministic(Atom == 'p wcnf 3 5 7\n7 1 2 0\n7 -1 -2 0\n3 1 0\n2 2 0\n1 3 0\n')) :-
		max_sat::program(object(max_sat_example), Codes, [embed_variables(false)]),
		atom_codes(Atom, Codes).

	test(max_sat_program_object_new_format, deterministic(Atom == 'h 1 2 0\nh -1 -2 0\n3 1 0\n2 2 0\n1 3 0\n')) :-
		max_sat::program(object(max_sat_example), Codes, [embed_variables(false), wcnf_format(new)]),
		atom_codes(Atom, Codes).

	test(max_sat_program_terms_source, deterministic(Atom == 'p wcnf 1 1 2\nc v 1 p\n1 1 0\n')) :-
		max_sat::program(terms([soft(p)]), Codes),
		atom_codes(Atom0, Codes),
		Atom0 = Atom.

	test(max_sat_program_codes_source, deterministic(Codes == [112, 46, 10])) :-
		max_sat::program(codes([112, 46, 10]), Codes).

	test(max_sat_program_file_source, deterministic) :-
		object_property(tests, file(_, Directory)),
		atom_concat(Directory, 'test_files/example.wcnf', Path),
		max_sat::program(file(Path), Codes),
		assertion(Codes = [0'c, 0' | _]).

	test(max_sat_program_wrong_object, error(domain_error(max_sat_source, object(max_sat_unrelated_fixture)))) :-
		max_sat::program(object(max_sat_unrelated_fixture), _).

	test(max_sat_program_wrong_source, error(domain_error(max_sat_source, foo))) :-
		max_sat::program(foo, _).

	test(max_sat_program_unbound, error(instantiation_error)) :-
		max_sat::program(_).

	test(max_sat_program_print_codes, deterministic) :-
		atom_codes('p wcnf 1 1 1\n1 1 0\n', Codes),
		^^set_text_output(''),
		max_sat::program(codes(Codes)),
		^^text_output_contents(Chars),
		subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p wcnf 1 1 1\n1 1 0\n').

	% max_sat_output tests

	test(max_sat_output_open_wbo_optimum, deterministic) :-
		atom_codes('c Open-WBO banner\nc\no 3\no 2\ns OPTIMUM FOUND\nv 1 -2 3 \n', Codes),
		max_sat_output::decode(open_wbo, Codes, exit(30), exit(30), [], [], [x1-1, x2-2, x3-3], false, Result),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+x1, -x2, +x3])),
		assertion(memberchk(cost(2), Properties)),
		assertion(memberchk(termination(completed), Properties)),
		assertion(memberchk(variable_count(3), Properties)),
		assertion(memberchk(solver(open_wbo), Properties)).

	test(max_sat_output_open_wbo_best_known_accepted, deterministic) :-
		atom_codes('o 9\no 5\ns SATISFIABLE\nv 1 -2 3\n', Codes),
		max_sat_output::decode(open_wbo, Codes, exit(40), exit(40), [], [], [x1-1, x2-2, x3-3], true, Result),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == satisfiable),
		assertion(Model == assignment([+x1, -x2, +x3])),
		assertion(memberchk(cost(5), Properties)),
		assertion(memberchk(termination(resource_limit), Properties)),
		assertion(memberchk(anytime(true), Properties)).

	test(max_sat_output_open_wbo_best_known_rejected, deterministic) :-
		atom_codes('o 9\no 5\ns SATISFIABLE\nv 1 -2 3\n', Codes),
		max_sat_output::decode(open_wbo, Codes, exit(40), exit(40), [], [], [x1-1, x2-2, x3-3], false, Result),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == unknown),
		assertion(Model == none),
		assertion(\+ memberchk(cost(_), Properties)),
		assertion(memberchk(termination(resource_limit), Properties)).

	test(max_sat_output_open_wbo_unsatisfiable, deterministic) :-
		atom_codes('c banner\ns UNSATISFIABLE\n', Codes),
		max_sat_output::decode(open_wbo, Codes, exit(20), exit(20), [], [], none, false, Result),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == unsatisfiable),
		assertion(Model == none),
		assertion(memberchk(termination(completed), Properties)).

	test(max_sat_output_open_wbo_unknown, deterministic(Status == unknown)) :-
		atom_codes('s UNKNOWN\n', Codes),
		max_sat_output::decode(open_wbo, Codes, exit(0), exit(0), [], [], none, false, max_sat_result(Status, _, _)).

	test(max_sat_output_identity_variables, deterministic(Model == assignment([+1, -(2)]))) :-
		atom_codes('o 0\ns OPTIMUM FOUND\nv 1 -2\n', Codes),
		max_sat_output::decode(open_wbo, Codes, exit(30), exit(30), [], [], none, false, max_sat_result(_, Model, _)).

	test(max_sat_output_multiline_witness_with_terminator, deterministic(Model == assignment([+p, -q, +r]))) :-
		atom_codes('s OPTIMUM FOUND\nv 1 -2\nv 3 0\n', Codes),
		max_sat_output::decode(rc2, Codes, exit(0), exit(0), [], [], [p-1, q-2, r-3], false, max_sat_result(_, Model, _)).

	test(max_sat_output_rc2_optimum, deterministic) :-
		atom_codes('c formula: 3 vars, 2 hard, 3 soft\ns OPTIMUM FOUND\no 2\nv 1 -2 3\nc oracle time: 0.0000\n', Codes),
		max_sat_output::decode(rc2, Codes, exit(0), exit(0), [], [], [x1-1, x2-2, x3-3], false, Result),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+x1, -x2, +x3])),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_output_rc2_no_witness, deterministic) :-
		atom_codes('s OPTIMUM FOUND\no 2\n', Codes),
		max_sat_output::decode(rc2, Codes, exit(0), exit(0), [], [], [x1-1], false, Result),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == none),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_output_rc2_interrupted, deterministic) :-
		atom_codes('c cost: 7; core sz: 3; soft sz: 10\nc interrupted; processing the last found core\nc oracle time: 2.7\n', Codes),
		max_sat_output::decode(rc2, Codes, exit(0), exit(0), [], [], none, true, Result),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == unknown),
		assertion(Model == none),
		assertion(memberchk(termination(resource_limit), Properties)).

	test(max_sat_output_rc2_interrupted_without_message, deterministic) :-
		atom_codes('c cost: 789; core sz: 261; soft sz: 826\nc oracle time: 1.6401\n', Codes),
		max_sat_output::decode(rc2, Codes, exit(0), exit(0), [], [], none, true, Result),
		Result = max_sat_result(Status, Model, _),
		assertion(Status == unknown),
		assertion(Model == none).

	test(max_sat_output_rc2_unsatisfiable, deterministic(Status == unsatisfiable)) :-
		atom_codes('c formula: 2 vars, 2 hard, 1 soft\ns UNSATISFIABLE\n', Codes),
		max_sat_output::decode(rc2, Codes, exit(0), exit(0), [], [], none, false, max_sat_result(Status, _, _)).

	test(max_sat_output_z3_optimum, deterministic) :-
		atom_codes('sat\n(define-fun k!1 () Bool\n  true)\n(define-fun k!2 () Bool\n  false)\n(define-fun k!3 () Bool\n  true)\n   2\n', Codes),
		max_sat_output::decode(z3, Codes, exit(0), exit(0), [], [], [x1-1, x2-2, x3-3], false, Result),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+x1, -x2, +x3])),
		assertion(memberchk(cost(2), Properties)),
		assertion(memberchk(termination(completed), Properties)).

	test(max_sat_output_z3_missing_variable_is_false, deterministic(Model == assignment([+x1, -x2, +x3]))) :-
		atom_codes('sat\n(define-fun k!1 () Bool\n  true)\n(define-fun k!3 () Bool\n  true)\n   2\n', Codes),
		max_sat_output::decode(z3, Codes, exit(0), exit(0), [], [], [x1-1, x2-2, x3-3], false, max_sat_result(_, Model, _)).

	test(max_sat_output_z3_identity_variables, deterministic(Model == assignment([+1, -(2)]))) :-
		atom_codes('sat\n(define-fun k!1 () Bool\n  true)\n(define-fun k!2 () Bool\n  false)\n   0\n', Codes),
		max_sat_output::decode(z3, Codes, exit(0), exit(0), [], [], none, false, max_sat_result(_, Model, _)).

	test(max_sat_output_z3_timeout, deterministic(Status-Model == unknown-none)) :-
		atom_codes('timeout\n', Codes),
		max_sat_output::decode(z3, Codes, exit(0), exit(0), [], [], none, true, max_sat_result(Status, Model, _)).

	test(max_sat_output_z3_unsatisfiable, deterministic(Status == unsatisfiable)) :-
		atom_codes('unsat\n', Codes),
		max_sat_output::decode(z3, Codes, exit(0), exit(0), [], [], none, false, max_sat_result(Status, _, _)).

	test(max_sat_output_crlf_lines, deterministic(Status == optimal)) :-
		atom_codes('o 0\r\ns OPTIMUM FOUND\r\nv 1\r\n', Codes),
		max_sat_output::decode(open_wbo, Codes, exit(30), exit(30), [], [], [p-1], false, max_sat_result(Status, _, _)).

	test(max_sat_output_unknown_exit_code_accepted, deterministic) :-
		% backends that cannot normalize nonzero exit codes (e.g., GNU Prolog)
		atom_codes('o 2\ns OPTIMUM FOUND\nv 1 -2\n', Codes),
		max_sat_output::decode(open_wbo, Codes, 30, unknown(30), [], [], none, false, max_sat_result(Status, _, Properties)),
		assertion(Status == optimal),
		assertion(memberchk(termination(completed), Properties)).

	test(max_sat_output_unknown_status_interrupted, deterministic) :-
		atom_codes('s UNKNOWN\n', Codes),
		max_sat_output::decode(open_wbo, Codes, 143, unknown(143), [], [], none, false, max_sat_result(_, _, Properties)),
		assertion(memberchk(termination(interrupted), Properties)).

	test(max_sat_output_killed_process, deterministic) :-
		atom_codes('s UNKNOWN\n', Codes),
		max_sat_output::decode(open_wbo, Codes, killed(9), killed(9), [], [], none, false, max_sat_result(_, _, Properties)),
		assertion(memberchk(termination(interrupted), Properties)).

	test(max_sat_output_missing_status, error(domain_error(max_sat_output, 'Result'-missing))) :-
		atom_codes('c just a comment\n', Codes),
		max_sat_output::decode(open_wbo, Codes, exit(0), exit(0), [], [], none, false, _).

	test(max_sat_output_unsupported_status, error(domain_error(max_sat_output, 'Result'-'SOMETHING ELSE'))) :-
		atom_codes('s SOMETHING ELSE\n', Codes),
		max_sat_output::decode(open_wbo, Codes, exit(0), exit(0), [], [], none, false, _).

	test(max_sat_output_unknown_variable, error(domain_error(max_sat_output, variable(4)))) :-
		atom_codes('s OPTIMUM FOUND\nv 1 -4\n', Codes),
		max_sat_output::decode(rc2, Codes, exit(0), exit(0), [], [], [p-1], false, _).

	test(max_sat_output_unexpected_exit_code, error(domain_error(max_sat_process_status, exit(7)))) :-
		atom_codes('s UNSATISFIABLE\n', Codes),
		max_sat_output::decode(open_wbo, Codes, exit(7), exit(7), [], [], none, false, _).

	test(max_sat_output_unexpected_rc2_exit_code, error(domain_error(max_sat_process_status, exit(1)))) :-
		atom_codes('s UNSATISFIABLE\n', Codes),
		max_sat_output::decode(rc2, Codes, exit(1), exit(1), [], [], none, false, _).

	test(max_sat_output_fatal_status, error(domain_error(max_sat_process_status, error))) :-
		atom_codes('s UNSATISFIABLE\n', Codes),
		max_sat_output::decode(rc2, Codes, error, error, [], [], none, false, _).

	test(max_sat_output_unbound_solver, error(instantiation_error)) :-
		max_sat_output::decode(_, [], exit(0), exit(0), [], [], none, false, _).

	test(max_sat_output_non_atom_solver, error(type_error(atom, 1))) :-
		max_sat_output::decode(1, [], exit(0), exit(0), [], [], none, false, _).

	test(max_sat_output_invalid_variables, error(type_error(list(pair), foo))) :-
		max_sat_output::decode(rc2, [], exit(0), exit(0), [], [], foo, false, _).

	% result inspection tests

	test(max_sat_literal_enumeration, deterministic(Literals == [+p, -q])) :-
		Result = max_sat_result(optimal, assignment([+p, -q]), [cost(1)]),
		findall(Literal, max_sat::literal(Result, Literal), Literals).

	test(max_sat_literal_no_model, fail) :-
		max_sat::literal(max_sat_result(unsatisfiable, none, []), _).

	test(max_sat_cost, deterministic(Cost == 2)) :-
		max_sat::cost(max_sat_result(optimal, none, [cost(2)]), Cost).

	test(max_sat_cost_missing, fail) :-
		max_sat::cost(max_sat_result(unsatisfiable, none, []), _).

	test(max_sat_result_property, deterministic(Properties == [status(optimal), cost(2), solver(z3)])) :-
		Result = max_sat_result(optimal, none, [cost(2), solver(z3)]),
		findall(Property, max_sat::result_property(Result, Property), Properties).

	test(max_sat_print_result_optimal, deterministic) :-
		Result = max_sat_result(optimal, assignment([+p, -q]), [cost(2), variables([p-1, q-2])]),
		assert_printed_result(Result, 'o 2\ns OPTIMUM FOUND\nv 1 -2\n').

	test(max_sat_print_result_identity, deterministic) :-
		Result = max_sat_result(optimal, assignment([+1, -(2)]), [cost(0), variables(none)]),
		assert_printed_result(Result, 'o 0\ns OPTIMUM FOUND\nv 1 -2\n').

	test(max_sat_print_result_satisfiable, deterministic) :-
		Result = max_sat_result(satisfiable, assignment([+p]), [cost(4), variables([p-1])]),
		assert_printed_result(Result, 'o 4\ns SATISFIABLE\nv 1\n').

	test(max_sat_print_result_unsatisfiable, deterministic) :-
		assert_printed_result(max_sat_result(unsatisfiable, none, []), 's UNSATISFIABLE\n').

	test(max_sat_print_result_unknown, deterministic) :-
		assert_printed_result(max_sat_result(unknown, none, []), 's UNKNOWN\n').

	test(max_sat_print_result_invalid, error(domain_error(max_sat_result, invalid))) :-
		max_sat::print_result(invalid).

	test(max_sat_print_result_unbound, error(instantiation_error)) :-
		max_sat::print_result(_).

	% solve/2-3 error handling tests (no solver required)

	test(max_sat_solve_missing_executable, error(existence_error(command, logtalk_max_sat_missing_executable))) :-
		max_sat::solve(codes([]), _, [executable(logtalk_max_sat_missing_executable)]).

	test(max_sat_solve_unbound_source, error(instantiation_error)) :-
		max_sat::solve(_, _).

	test(max_sat_solve_invalid_option_solver, error(domain_error(option, solver(kissat)))) :-
		max_sat::solve(terms([p]), _, [solver(kissat)]).

	test(max_sat_solve_invalid_option_time_limit, error(domain_error(option, time_limit(-1)))) :-
		max_sat::solve(terms([p]), _, [time_limit(-1)]).

	test(max_sat_solve_invalid_option_anytime, error(domain_error(option, anytime(maybe)))) :-
		max_sat::solve(terms([p]), _, [anytime(maybe)]).

	test(max_sat_solve_invalid_option_format, error(domain_error(option, wcnf_format(newer)))) :-
		max_sat::solve(terms([p]), _, [wcnf_format(newer)]).

	test(max_sat_solve_unbound_options, error(instantiation_error)) :-
		max_sat::solve(terms([p]), _, _).

	test(max_sat_solve_wrong_source, error(domain_error(max_sat_source, foo)), [condition(os::resolve_command_path(z3, _))]) :-
		max_sat::solve(foo, _, [solver(z3)]).

	test(max_sat_solve_wrong_object, error(domain_error(max_sat_source, object(max_sat_unrelated_fixture))), [condition(os::resolve_command_path(z3, _))]) :-
		max_sat::solve(object(max_sat_unrelated_fixture), _, [solver(z3)]).

	test(max_sat_solve_invalid_statement, error(domain_error(max_sat_statement, soft(0, p))), [condition(os::resolve_command_path(z3, _))]) :-
		max_sat::solve(terms([soft(0, p)]), _, [solver(z3)]).

	% external RC2, Open-WBO, and Z3 CLI solving tests (conditional on availability)

	test(max_sat_solve_example_object_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(rc2)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+x1, -x2, +x3])),
		assertion(memberchk(cost(2), Properties)),
		assertion(memberchk(termination(completed), Properties)),
		assertion(memberchk(solver(rc2), Properties)),
		assertion(memberchk(variable_count(3), Properties)).

	test(max_sat_solve_example_terms_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		Statements = [(x1 ; x2), (-x1 ; -x2), soft(3, x1), soft(2, x2), soft(1, x3)],
		max_sat::solve(terms(Statements), Result, [solver(rc2)]),
		max_sat::cost(Result, Cost),
		assertion(Cost == 2),
		findall(Literal, max_sat::literal(Result, Literal), Literals),
		assertion(Literals == [+x1, -x2, +x3]).

	test(max_sat_solve_example_file_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		object_property(tests, file(_, Directory)),
		atom_concat(Directory, 'test_files/example.wcnf', Path),
		max_sat::solve(file(Path), Result, [solver(rc2)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+x1, -x2, +x3])),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_solve_example_codes_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		atom_codes('p wcnf 3 5 7\n7 1 2 0\n7 -1 -2 0\n3 1 0\n2 2 0\n1 3 0\n', Codes),
		max_sat::solve(codes(Codes), Result, [solver(rc2)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+1, -(2), +3])),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_solve_new_format_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(rc2), wcnf_format(new)]),
		max_sat::cost(Result, Cost),
		assertion(Cost == 2).

	test(max_sat_solve_vertex_cover_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		max_sat::solve(object(max_sat_vertex_cover), Result, [solver(rc2)]),
		Result = max_sat_result(Status, _, Properties),
		assertion(Status == optimal),
		assertion(memberchk(cost(3), Properties)),
		findall(Literal, max_sat::literal(Result, Literal), Literals),
		assertion(memberchk(-in(a), Literals)),
		assertion(memberchk(+in(b), Literals)),
		assertion(memberchk(+in(c), Literals)).

	test(max_sat_solve_all_soft_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		max_sat::solve(object(max_sat_all_soft), Result, [solver(rc2)]),
		max_sat::cost(Result, Cost),
		assertion(Cost == 1).

	test(max_sat_solve_hard_unsatisfiable_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		max_sat::solve(object(max_sat_hard_unsat), Result, [solver(rc2)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == unsatisfiable),
		assertion(Model == none),
		assertion(\+ memberchk(cost(_), Properties)),
		assertion(memberchk(termination(completed), Properties)).

	test(max_sat_solve_time_limit_option_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(rc2), time_limit(30)]),
		Result = max_sat_result(Status, _, Properties),
		assertion(Status == optimal),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_solve_print_result_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(rc2)]),
		assert_printed_result(Result, 'o 2\ns OPTIMUM FOUND\nv 1 -2 3\n').

	test(max_sat_solve_random_instances_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		random_instance_count(Count), forall_random_instance(Count, 1001, rc2).

	test(max_sat_solve_anytime_true_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		hard_instance(Statements),
		max_sat::solve(terms(Statements), Result, [solver(rc2), time_limit(1), anytime(true), embed_variables(false)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(memberchk(Status, [optimal, satisfiable, unknown])),
		(	Status == unknown ->
			assertion(Model == none)
		;	assertion(Model = assignment(_)),
			assertion(memberchk(cost(_), Properties))
		).

	test(max_sat_solve_anytime_false_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		hard_instance(Statements),
		max_sat::solve(terms(Statements), Result, [solver(rc2), time_limit(1), anytime(false)]),
		Result = max_sat_result(Status, Model, _),
		assertion(memberchk(Status, [optimal, unknown])),
		(	Status == unknown ->
			assertion(Model == none)
		;	assertion(Model = assignment(_))
		).

	test(max_sat_solve_example_object_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(open_wbo)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+x1, -x2, +x3])),
		assertion(memberchk(cost(2), Properties)),
		assertion(memberchk(termination(completed), Properties)),
		assertion(memberchk(solver(open_wbo), Properties)),
		assertion(memberchk(variable_count(3), Properties)).

	test(max_sat_solve_example_terms_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		Statements = [(x1 ; x2), (-x1 ; -x2), soft(3, x1), soft(2, x2), soft(1, x3)],
		max_sat::solve(terms(Statements), Result, [solver(open_wbo)]),
		max_sat::cost(Result, Cost),
		assertion(Cost == 2),
		findall(Literal, max_sat::literal(Result, Literal), Literals),
		assertion(Literals == [+x1, -x2, +x3]).

	test(max_sat_solve_example_file_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		object_property(tests, file(_, Directory)),
		atom_concat(Directory, 'test_files/example.wcnf', Path),
		max_sat::solve(file(Path), Result, [solver(open_wbo)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+x1, -x2, +x3])),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_solve_example_codes_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		atom_codes('p wcnf 3 5 7\n7 1 2 0\n7 -1 -2 0\n3 1 0\n2 2 0\n1 3 0\n', Codes),
		max_sat::solve(codes(Codes), Result, [solver(open_wbo)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+1, -(2), +3])),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_solve_vertex_cover_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		max_sat::solve(object(max_sat_vertex_cover), Result, [solver(open_wbo)]),
		Result = max_sat_result(Status, _, Properties),
		assertion(Status == optimal),
		assertion(memberchk(cost(3), Properties)),
		findall(Literal, max_sat::literal(Result, Literal), Literals),
		assertion(memberchk(-in(a), Literals)),
		assertion(memberchk(+in(b), Literals)),
		assertion(memberchk(+in(c), Literals)).

	test(max_sat_solve_all_soft_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		max_sat::solve(object(max_sat_all_soft), Result, [solver(open_wbo)]),
		max_sat::cost(Result, Cost),
		assertion(Cost == 1).

	test(max_sat_solve_hard_unsatisfiable_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		max_sat::solve(object(max_sat_hard_unsat), Result, [solver(open_wbo)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == unsatisfiable),
		assertion(Model == none),
		assertion(\+ memberchk(cost(_), Properties)),
		assertion(memberchk(termination(completed), Properties)).

	test(max_sat_solve_time_limit_option_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(open_wbo), time_limit(30)]),
		Result = max_sat_result(Status, _, Properties),
		assertion(Status == optimal),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_solve_print_result_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(open_wbo)]),
		assert_printed_result(Result, 'o 2\ns OPTIMUM FOUND\nv 1 -2 3\n').

	test(max_sat_solve_random_instances_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		random_instance_count(Count), forall_random_instance(Count, 2002, open_wbo).

	test(max_sat_solve_anytime_true_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		hard_instance(Statements),
		max_sat::solve(terms(Statements), Result, [solver(open_wbo), time_limit(1), anytime(true), embed_variables(false)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(memberchk(Status, [optimal, satisfiable, unknown])),
		(	Status == unknown ->
			assertion(Model == none)
		;	assertion(Model = assignment(_)),
			assertion(memberchk(cost(_), Properties))
		).

	test(max_sat_solve_anytime_false_open_wbo, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		hard_instance(Statements),
		max_sat::solve(terms(Statements), Result, [solver(open_wbo), time_limit(1), anytime(false)]),
		Result = max_sat_result(Status, Model, _),
		assertion(memberchk(Status, [optimal, unknown])),
		(	Status == unknown ->
			assertion(Model == none)
		;	assertion(Model = assignment(_))
		).

	test(max_sat_solve_example_object_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(z3)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+x1, -x2, +x3])),
		assertion(memberchk(cost(2), Properties)),
		assertion(memberchk(termination(completed), Properties)),
		assertion(memberchk(solver(z3), Properties)),
		assertion(memberchk(variable_count(3), Properties)).

	test(max_sat_solve_example_terms_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		Statements = [(x1 ; x2), (-x1 ; -x2), soft(3, x1), soft(2, x2), soft(1, x3)],
		max_sat::solve(terms(Statements), Result, [solver(z3)]),
		max_sat::cost(Result, Cost),
		assertion(Cost == 2),
		findall(Literal, max_sat::literal(Result, Literal), Literals),
		assertion(Literals == [+x1, -x2, +x3]).

	test(max_sat_solve_example_file_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		object_property(tests, file(_, Directory)),
		atom_concat(Directory, 'test_files/example.wcnf', Path),
		max_sat::solve(file(Path), Result, [solver(z3)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+x1, -x2, +x3])),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_solve_example_codes_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		atom_codes('p wcnf 3 5 7\n7 1 2 0\n7 -1 -2 0\n3 1 0\n2 2 0\n1 3 0\n', Codes),
		max_sat::solve(codes(Codes), Result, [solver(z3)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == optimal),
		assertion(Model == assignment([+1, -(2), +3])),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_solve_new_format_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(z3), wcnf_format(new)]),
		max_sat::cost(Result, Cost),
		assertion(Cost == 2).

	test(max_sat_solve_vertex_cover_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		max_sat::solve(object(max_sat_vertex_cover), Result, [solver(z3)]),
		Result = max_sat_result(Status, _, Properties),
		assertion(Status == optimal),
		assertion(memberchk(cost(3), Properties)),
		findall(Literal, max_sat::literal(Result, Literal), Literals),
		assertion(memberchk(-in(a), Literals)),
		assertion(memberchk(+in(b), Literals)),
		assertion(memberchk(+in(c), Literals)).

	test(max_sat_solve_all_soft_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		max_sat::solve(object(max_sat_all_soft), Result, [solver(z3)]),
		max_sat::cost(Result, Cost),
		assertion(Cost == 1).

	test(max_sat_solve_hard_unsatisfiable_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		max_sat::solve(object(max_sat_hard_unsat), Result, [solver(z3)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(Status == unsatisfiable),
		assertion(Model == none),
		assertion(\+ memberchk(cost(_), Properties)),
		assertion(memberchk(termination(completed), Properties)).

	test(max_sat_solve_time_limit_option_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(z3), time_limit(30)]),
		Result = max_sat_result(Status, _, Properties),
		assertion(Status == optimal),
		assertion(memberchk(cost(2), Properties)).

	test(max_sat_solve_print_result_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		max_sat::solve(object(max_sat_example), Result, [solver(z3)]),
		assert_printed_result(Result, 'o 2\ns OPTIMUM FOUND\nv 1 -2 3\n').

	test(max_sat_solve_random_instances_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		random_instance_count(Count), forall_random_instance(Count, 3003, z3).

	test(max_sat_solve_anytime_true_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		hard_instance(Statements),
		max_sat::solve(terms(Statements), Result, [solver(z3), time_limit(1), anytime(true), embed_variables(false)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(memberchk(Status, [optimal, satisfiable, unknown])),
		(	Status == unknown ->
			assertion(Model == none)
		;	assertion(Model = assignment(_)),
			assertion(memberchk(cost(_), Properties))
		).

	test(max_sat_solve_anytime_false_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		hard_instance(Statements),
		max_sat::solve(terms(Statements), Result, [solver(z3), time_limit(1), anytime(false)]),
		Result = max_sat_result(Status, Model, _),
		assertion(memberchk(Status, [optimal, unknown])),
		(	Status == unknown ->
			assertion(Model == none)
		;	assertion(Model = assignment(_))
		).

	% Open-WBO 2.1 does not support the new WCNF format
	test(max_sat_solve_new_format_unsupported_open_wbo, error(max_sat_solver_error(domain_error(max_sat_output, 'Result'-missing), _, _, _)), [condition(os::resolve_command_path('open-wbo', _))]) :-
		max_sat::solve(object(max_sat_example), _, [solver(open_wbo), wcnf_format(new)]).

	% Open-WBO is an anytime solver; with the hard instance and a short time limit it reports a best-known solution
	test(max_sat_solve_anytime_open_wbo_reports_best_known, deterministic, [condition(os::resolve_command_path('open-wbo', _))]) :-
		hard_instance(Statements),
		max_sat::solve(terms(Statements), Result, [solver(open_wbo), time_limit(1), anytime(true), embed_variables(false)]),
		Result = max_sat_result(Status, Model, Properties),
		assertion(memberchk(Status, [optimal, satisfiable])),
		assertion(Model = assignment(_)),
		assertion(memberchk(cost(_), Properties)).

	test(max_sat_solve_syntax_diagnostics_rc2, deterministic, [condition(os::resolve_command_path('rc2.py', _))]) :-
		catch(max_sat::solve(codes([0'x, 10]), _, [solver(rc2)]), error(max_sat_solver_error(_, _, Diagnostics, _), _), true),
		assertion(nonvar(Diagnostics)).

	% auxiliary predicates

	assert_printed_result(Result, Expected) :-
		^^set_text_output(''),
		max_sat::print_result(Result),
		^^text_output_contents(Chars),
		subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == Expected).

	% deterministic pseudo-random instance generation (linear congruential generator)

	next_seed(Seed0, Seed) :-
		% Split the seed into 16-bit parts to avoid overflow before reduction modulo 2^31.
		Low is Seed0 mod 65536,
		High is ((Seed0 // 65536) * 1103515245) mod 32768,
		Seed is (Low * 1103515245 + High * 65536 + 12345) mod 2147483648.

	random_int(Low, High, Seed0, Seed, Value) :-
		next_seed(Seed0, Seed),
		Value is Low + (Seed // 65536) mod (High - Low + 1).

	random_literals(0, _, Seed, Seed, []) :-
		!.
	random_literals(Count, Variables, Seed0, Seed, [Literal| Literals]) :-
		random_int(1, Variables, Seed0, Seed1, Index),
		random_int(0, 1, Seed1, Seed2, Sign),
		(	Sign =:= 0 ->
			Literal = +x(Index)
		;	Literal = -x(Index)
		),
		Count1 is Count - 1,
		random_literals(Count1, Variables, Seed2, Seed, Literals).

	random_clauses(Count, Variables, Size, MaxWeight, Seed0, Seed, Clauses) :-
		random_clauses(Count, Variables, 1, Size, MaxWeight, Seed0, Seed, Clauses).

	random_clauses(0, _, _, _, _, Seed, Seed, []) :-
		!.
	random_clauses(Count, Variables, MinSize, Size, MaxWeight, Seed0, Seed, [Clause| Clauses]) :-
		random_int(MinSize, Size, Seed0, Seed1, Length),
		random_literals(Length, Variables, Seed1, Seed2, Literals),
		(	MaxWeight =:= 0 ->
			Clause = hard(Literals),
			Seed3 = Seed2
		;	random_int(1, MaxWeight, Seed2, Seed3, Weight),
			Clause = soft(Weight, Literals)
		),
		Count1 is Count - 1,
		random_clauses(Count1, Variables, MinSize, Size, MaxWeight, Seed3, Seed, Clauses).

	% a random weighted partial MaxSAT instance: a few hard clauses followed by soft clauses

	random_instance(Seed0, Seed, Variables, Clauses) :-
		random_int(3, 6, Seed0, Seed1, Variables),
		random_int(0, 3, Seed1, Seed2, HardCount),
		random_int(3, 8, Seed2, Seed3, SoftCount),
		random_clauses(HardCount, Variables, 3, 0, Seed3, Seed4, Hard),
		random_clauses(SoftCount, Variables, 3, 5, Seed4, Seed, Soft),
		append(Hard, Soft, Clauses).

	clause_statements([], []).
	clause_statements([hard(Literals)| Clauses], [Disjunction| Statements]) :-
		!,
		disjunction(Literals, Disjunction),
		clause_statements(Clauses, Statements).
	clause_statements([soft(Weight, Literals)| Clauses], [soft(Weight, Disjunction)| Statements]) :-
		disjunction(Literals, Disjunction),
		clause_statements(Clauses, Statements).

	disjunction([Literal], Literal) :-
		!.
	disjunction([Literal| Literals], (Literal ; Disjunction)) :-
		disjunction(Literals, Disjunction).

	% brute force oracle: the optimal cost over all assignments (or none if the hard clauses are unsatisfiable)

	oracle_cost(Variables, Clauses, Cost) :-
		findall(
			AssignmentCost,
			(	assignment(Variables, Assignment),
				assignment_cost(Clauses, Assignment, AssignmentCost)
			),
			Costs
		),
		(	Costs == [] ->
			Cost = none
		;	min(Costs, Cost)
		).

	assignment(0, []) :-
		!.
	assignment(Variables, [Value| Values]) :-
		(	Value = true
		;	Value = false
		),
		Variables1 is Variables - 1,
		assignment(Variables1, Values).

	% an assignment violating a hard clause has no cost (it is not generated)
	assignment_cost([], _, 0).
	assignment_cost([hard(Literals)| Clauses], Assignment, Cost) :-
		clause_true(Literals, Assignment),
		assignment_cost(Clauses, Assignment, Cost).
	assignment_cost([soft(Weight, Literals)| Clauses], Assignment, Cost) :-
		assignment_cost(Clauses, Assignment, Cost0),
		(	clause_true(Literals, Assignment) ->
			Cost = Cost0
		;	Cost is Cost0 + Weight
		).

	clause_true([Literal| Literals], Assignment) :-
		(	literal_true(Literal, Assignment) ->
			true
		;	clause_true(Literals, Assignment)
		).

	literal_true(+x(Index), Assignment) :-
		nth1(Index, Assignment, true).
	literal_true(-x(Index), Assignment) :-
		nth1(Index, Assignment, false).

	% the cost of a returned model (variables not in the model are false)

	model_cost(Clauses, Literals, Cost) :-
		model_cost(Clauses, Literals, true, Cost).

	model_cost([], _, _, 0).
	model_cost([hard(Literals)| Clauses], Model, Feasible, Cost) :-
		!,
		model_clause_true(Literals, Model),
		model_cost(Clauses, Model, Feasible, Cost).
	model_cost([soft(Weight, Literals)| Clauses], Model, Feasible, Cost) :-
		model_cost(Clauses, Model, Feasible, Cost0),
		(	model_clause_true(Literals, Model) ->
			Cost = Cost0
		;	Cost is Cost0 + Weight
		).

	model_clause_true([Literal| Literals], Model) :-
		(	model_literal_true(Literal, Model) ->
			true
		;	model_clause_true(Literals, Model)
		).

	model_literal_true(+Term, Model) :-
		memberchk(+Term, Model).
	model_literal_true(-Term, Model) :-
		\+ memberchk(+Term, Model).

	forall_random_instance(0, _, _) :-
		!.
	forall_random_instance(Count, Seed0, Solver) :-
		random_instance(Seed0, Seed, Variables, Clauses),
		clause_statements(Clauses, Statements),
		oracle_cost(Variables, Clauses, Expected),
		max_sat::solve(terms(Statements), Result, [solver(Solver)]),
		Result = max_sat_result(Status, Model, Properties),
		(	Expected == none ->
			assertion(Status == unsatisfiable)
		;	assertion(Status == optimal),
			memberchk(cost(Cost), Properties),
			assertion(Cost =:= Expected),
			Model = assignment(Literals),
			model_cost(Clauses, Literals, ModelCost),
			assertion(ModelCost =:= Expected)
		),
		Count1 is Count - 1,
		forall_random_instance(Count1, Seed, Solver).

	% a harder instance for the time limit tests: 200 variables, 250 hard 3-clauses, 1000 soft 2-clauses

	hard_instance(Statements) :-
		hard_instance_size(Variables, HardCount, SoftCount),
		random_clauses(HardCount, Variables, 3, 3, 0, 424242, Seed, Hard),
		random_clauses(SoftCount, Variables, 2, 2, 30, Seed, _, Soft),
		append(Hard, Soft, Clauses),
		clause_statements(Clauses, Statements).

	% fewer random instances on GNU Prolog for the same reason
	:- if(current_logtalk_flag(prolog_dialect, gnu)).
		random_instance_count(6).
	:- else.
		random_instance_count(25).
	:- endif.

	% GNU Prolog has a small default global stack, which the tests running in debug
	% mode with coverage can exhaust when using larger instances
	:- if(current_logtalk_flag(prolog_dialect, gnu)).
		hard_instance_size(80, 100, 400).
	:- else.
		hard_instance_size(200, 250, 1000).
	:- endif.

:- end_object.
