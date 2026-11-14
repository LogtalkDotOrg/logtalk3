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
		date is 2026-11-14,
		comment is 'Unit tests for the "omt" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	cover(omt).

	test(omt_program_object, deterministic) :-
		omt::program(object(omt_fixture), Codes),
		atom_codes(Atom, Codes),
		assertion(Atom == '(set-logic QF_LIA)\n(declare-const x Int)\n(assert (>= x 2))\n(minimize x)\n(check-sat)\n(get-objectives)\n').

	test(omt_expansion_isolation, deterministic(Value == ordinary)) :-
		omt_unrelated::value(Value),
		assertion(\+ omt_unrelated::current_predicate(smt_statement/2)).

	test(omt_multiple_objectives, error(domain_error(omt_objective_scope, maximize(y)))) :-
		omt::program(terms([minimize(x), push(1), maximize(y)]), _).

	test(omt_pop_underflow, error(domain_error(omt_pop_scope, 1))) :-
		omt::program(terms([pop(1)]), _).

	test(omt_scoped_objectives, deterministic) :-
		omt::program(terms([push(1), minimize(x), pop(1), maximize(y)]), _).

	test(omt_exact_objectives, deterministic) :-
		atom_codes('sat\n(objectives (x (/ 0.123456789012345678901 3.0)))\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [Result]),
		once(omt::objective(Result, x, Bound)),
		assertion(Bound == /(decimal('0.123456789012345678901'), decimal('3.0'))).

	test(omt_malformed_objectives, error(domain_error(smt_output, objectives([[x]])))) :-
		atom_codes('sat\n(objectives (x))\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], _).

	test(omt_unknown_objective_range, deterministic) :-
		atom_codes('unknown\n(objectives (x unknown), range: [ (- oo), oo ])\n', Codes),
		smt_output::decode(optimathsat, Codes, exit(0), exit(0), [], [], [Result]),
		once(omt::status(Result, unknown)),
		once(omt::objective(Result, x, annotated(unknown, range(-(oo), oo)))).

	test(omt_unbounded_output, deterministic) :-
		atom_codes('sat\n(objectives (x (* (- 1) oo)))\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [Result]),
		once(omt::objective(Result, x, *(-(1), oo))).

	test(omt_no_objective_response, deterministic(\+ omt::objective(Result, _, _))) :-
		atom_codes('sat\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [Result]).

	test(omt_objectives_enumerated, deterministic(Objectives == [x-1, y-2])) :-
		atom_codes('sat\n(objectives (x 1) (y 2))\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [Result]),
		findall(Expression-Bound, omt::objective(Result, Expression, Bound), Objectives).

	test(omt_print_results_objectives, deterministic) :-
		^^set_text_output(''),
		omt::print_results([
			smt_result(satisfiable, none, [core(none), values([]), objectives([objective(x, 1), objective(y, 2)])]),
			smt_result(unsatisfiable, none, [core(none), values([])])
		]),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'status: satisfiable\nobjective: x = 1\nobjective: y = 2\n\nstatus: unsatisfiable\n').

	test(omt_program_prints_objective, deterministic) :-
		^^set_text_output(''),
		omt::program(terms([declare_const(x, int), minimize(x), check_sat, get_objectives])),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == '(declare-const x Int)\n(minimize x)\n(check-sat)\n(get-objectives)\n').

	test(omt_invalid_solver, error(domain_error(option, solver(cvc5)))) :-
		omt::solve(terms([]), _, [solver(cvc5)]).

	test(omt_unbound_solver, error(domain_error(option, solver(_)))) :-
		omt::solve(terms([]), _, [solver(_)]).

	test(omt_invalid_time_limit, error(domain_error(option, time_limit(-1)))) :-
		omt::solve(terms([]), _, [time_limit(-1)]).

	test(omt_invalid_provider, error(domain_error(omt_source, object(omt_unrelated)))) :-
		omt::solve(object(omt_unrelated), _).

	test(omt_missing_executable, error(existence_error(command, logtalk_omt_missing_executable))) :-
		omt::solve(terms([]), _, [executable(logtalk_omt_missing_executable)]).

	test(omt_optimathsat_timeout, error(domain_error(optimathsat_time_limit, 1))) :-
		omt::solve(terms([]), _, [solver(optimathsat), time_limit(1)]).

	test(omt_z3_time_limit, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		omt::solve(object(omt_fixture), [Result], [time_limit(30)]),
		once(omt::objective(Result, x, 2)).

	test(omt_minimize_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		solve_fixture(z3).

	test(omt_minimize_optimathsat, deterministic, [condition(os::resolve_command_path(optimathsat, _))]) :-
		solve_fixture(optimathsat).

	test(omt_incremental_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		solve_incremental(z3).

	test(omt_incremental_optimathsat, deterministic, [condition(os::resolve_command_path(optimathsat, _))]) :-
		solve_incremental(optimathsat).

	test(omt_assuming_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		solve_assuming(z3, Result),
		once(omt::status(Result, satisfiable)),
		once(omt::objective(Result, x, 3)).

	test(omt_assuming_optimathsat, deterministic, [condition(os::resolve_command_path(optimathsat, _))]) :-
		solve_assuming(optimathsat, Result),
		once(omt::status(Result, satisfiable)),
		once(omt::objective(Result, x, _)).

	test(omt_unsat_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		solve_unsat(z3).

	test(omt_unsat_optimathsat, deterministic, [condition(os::resolve_command_path(optimathsat, _))]) :-
		solve_unsat(optimathsat).

	test(omt_strict_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		strict_bound(z3, epsilon).

	test(omt_strict_optimathsat, deterministic, [condition(os::resolve_command_path(optimathsat, _))]) :-
		strict_bound(optimathsat, +(0, epsilon)).

	test(omt_rational_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		rational_bound(z3, /(decimal('1.0'), decimal('3.0'))).

	test(omt_rational_optimathsat, deterministic, [condition(os::resolve_command_path(optimathsat, _))]) :-
		rational_bound(optimathsat, /(1, 3)).

	test(omt_codes_source, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		atom_codes('(declare-const x Int)\n(assert (>= x 3))\n(minimize x)\n(check-sat)\n(get-objectives)\n', Codes),
		omt::solve(codes(Codes), [Result]),
		once(omt::objective(Result, x, 3)).

	test(omt_file_source_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		native_file(z3).

	test(omt_file_source_optimathsat, deterministic, [condition(os::resolve_command_path(optimathsat, _))]) :-
		native_file(optimathsat).

	test(omt_model_differs_from_bound, deterministic(Value \== epsilon), [condition(os::resolve_command_path(z3, _))]) :-
		omt::solve(terms([
			set_logic('QF_LRA'), declare_const(x, real), assert(>(x, 0)),
			minimize(x), check_sat, get_objectives, get_model
		]), [Result]),
		once(omt::objective(Result, x, epsilon)),
		once(omt::model_value(Result, x, Value)).

	test(omt_unbounded_z3, deterministic, [condition(os::resolve_command_path(z3, _))]) :-
		omt::solve(terms([
			set_logic('QF_LRA'), declare_const(x, real), minimize(x), check_sat, get_objectives
		]), [Result]),
		once(omt::objective(Result, x, *(-(1), oo))).

	test(omt_bad_solver_output, error(omt_solver_error(smt_solver_reported_error(_), _, _, _)), [condition(os::resolve_command_path(z3, _))]) :-
		omt::solve(terms([raw('(assert)'), check_sat]), _).

	test(omt_unsupported_command, error(omt_solver_error(smt_solver_reported_error(unsupported), _, _, _)), [condition(os::resolve_command_path(z3, _))]) :-
		omt::solve(terms([raw('(bad-command)'), check_sat]), _).

	test(omt_failed_solver_after_status, error(omt_solver_error(smt_process_status(exit(1)), exit(1), _, _)), [condition((\+ os::operating_system_type(windows), os::resolve_command_path(sh, _)))]) :-
		^^file_path('test_files/failed_solver.sh', Path),
		omt::solve(file(Path), _, [executable('/bin/sh')]).

	% auxiliary predicates

	native_file(Solver) :-
		^^file_path('test_files/native.smt2', Path),
		omt::solve(file(Path), [Result], [solver(Solver)]),
		once(omt::objective(Result, x, 7)).

	solve_fixture(Solver) :-
		omt::solve(object(omt_fixture), [Result], [solver(Solver)]),
		once(omt::status(Result, satisfiable)),
		once(omt::objective(Result, x, 2)),
		once(omt::result_property(Result, source_map(Map))),
		assertion(Map \== []).

	solve_incremental(Solver) :-
		Statements = [
			set_logic('QF_LIA'), declare_const(x, int),
			push(1), assert(>=(x, 2)), minimize(x), check_sat, get_objectives, pop(1),
			push(1), assert('<='(x, 8)), maximize(x), check_sat, get_objectives, pop(1)
		],
		omt::solve(terms(Statements), [First, Second], [solver(Solver)]),
		once(omt::objective(First, x, 2)),
		once(omt::objective(Second, x, 8)).

	solve_assuming(Solver, Result) :-
		omt::solve(terms([
			set_logic('QF_LIA'), declare_const(x, int), declare_const(p, bool),
			assert(>=(x, 0)), assert('=>'(p, >=(x, 3))),
			minimize(x), check_sat_assuming([p]), get_objectives
		]), [Result], [solver(Solver)]).

	solve_unsat(Solver) :-
		omt::solve(terms([
			set_logic('QF_LIA'), declare_const(x, int),
			assert(<(x, 0)), assert(>(x, 0)), minimize(x), check_sat, get_objectives
		]), [Result], [solver(Solver)]),
		once(omt::status(Result, unsatisfiable)).

	strict_bound(Solver, Bound) :-
		omt::solve(terms([
			set_logic('QF_LRA'), declare_const(x, real), assert(>(x, 0)),
			minimize(x), check_sat, get_objectives
		]), [Result], [solver(Solver)]),
		once(omt::objective(Result, x, Bound)).

	rational_bound(Solver, Bound) :-
		omt::solve(terms([
			set_logic('QF_LRA'), declare_const(x, real), assert(>=(x, /(1, 3))),
			minimize(x), check_sat, get_objectives
		]), [Result], [solver(Solver)]),
		once(omt::objective(Result, x, Bound)).

:- end_object.
