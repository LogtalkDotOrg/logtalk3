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
		comment is 'Unit tests for the smt library.'
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

	cover(smt_program).
	cover(smt_output).
	cover(smt_expansion).
	cover(smt).
	cover(smt_process).

	% smt_program tests

	test(smt_program_empty, deterministic(Atom == '')) :-
		smt_program::serialize([], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_set_logic, deterministic(Atom == '(set-logic QF_LIA)\n')) :-
		smt_program::serialize([set_logic('QF_LIA')], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_basic, deterministic(Atom == '(declare-const x Int)\n(assert (> x 0))\n(check-sat)\n')) :-
		smt_program::serialize([declare_const(x, int), assert(>(x, 0)), check_sat], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_declare_fun, deterministic(Atom == '(declare-fun f (Int Int) Bool)\n')) :-
		smt_program::serialize([declare_fun(f, [int, int], bool)], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_define_fun, deterministic(Atom == '(define-fun f ((x Int) (y Int)) Bool (> x y))\n')) :-
		smt_program::serialize([define_fun(f, [x-int, y-int], bool, >(x, y))], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_bitvector, deterministic(Atom == '(declare-const x (_ BitVec 8))\n(assert (= x #b00000101))\n')) :-
		smt_program::serialize([declare_const(x, bv(8)), assert(=(x, bv(5, 8)))], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_array, deterministic(Atom == '(declare-const a (Array Int Int))\n')) :-
		smt_program::serialize([declare_const(a, array(int, int))], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_quantifier, deterministic(Atom == '(assert (forall ((x Int)) (> x 0)))\n')) :-
		smt_program::serialize([assert(forall([x-int], >(x, 0)))], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_extract, deterministic(Atom == '(assert (= ((_ extract 3 0) x) #b0001))\n')) :-
		smt_program::serialize([assert(=(extract(3, 0, x), bv(1, 4)))], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_negative_numeral, deterministic(Atom == '(assert (= x (- 5)))\n')) :-
		smt_program::serialize([assert(=(x, -5))], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_push_pop, deterministic(Atom == '(push 1)\n(pop 1)\n')) :-
		smt_program::serialize([push(1), pop(1)], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_comment, deterministic(Atom == '; hello\n')) :-
		atom_codes(hello, HelloCodes),
		smt_program::serialize([comment(HelloCodes)], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_raw, deterministic(Atom == '; trusted\n')) :-
		atom_codes('; trusted', RawCodes),
		smt_program::serialize([raw(RawCodes)], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_auto_produce_models, deterministic(Atom == '(set-option :produce-models true)\n(check-sat)\n(get-model)\n')) :-
		smt_program::serialize([check_sat, get_model], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_auto_produce_unsat_cores_from_get, deterministic(Atom == '(set-option :produce-unsat-cores true)\n(check-sat)\n(get-unsat-core)\n')) :-
		smt_program::serialize([check_sat, get_unsat_core], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_auto_produce_unsat_cores_from_named, deterministic(Atom == '(set-option :produce-unsat-cores true)\n(assert (! (> x 0) :named a1))\n(check-sat)\n')) :-
		smt_program::serialize([assert_named(a1, >(x, 0)), check_sat], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_no_duplicate_option, deterministic(Atom == '(set-option :produce-models true)\n(check-sat)\n(get-model)\n')) :-
		smt_program::serialize([set_option(produce_models, true), check_sat, get_model], Codes),
		atom_codes(Atom, Codes).

	test(smt_program_source_map, deterministic) :-
		atom_codes(hello, HelloCodes),
		smt_program::serialize([comment(HelloCodes), declare_const(x, int)], [source('fixture.lgt', 3, 4), unknown], Codes, Map),
		atom_codes(Atom, Codes),
		assertion(Atom == '; hello\n(declare-const x Int)\n'),
		assertion(Map == [line(1, 1, source('fixture.lgt', 3, 4)), line(2, 2, unknown)]).

	test(smt_program_unbound, error(instantiation_error)) :-
		smt_program::serialize(_, _).

	test(smt_program_non_ground, error(instantiation_error)) :-
		smt_program::serialize([assert(_)], _).

	test(smt_program_non_list, error(type_error(list, invalid))) :-
		smt_program::serialize(invalid, _).

	test(smt_program_invalid_statement, error(domain_error(smt_statement, call(halt)))) :-
		smt_program::serialize([call(halt)], _).

	test(smt_program_invalid_sort_arity, error(domain_error(smt_statement, declare_sort(foo, -1)))) :-
		smt_program::serialize([declare_sort(foo, -1)], _).

	test(smt_program_invalid_origins, error(domain_error(smt_origins, [wrong]))) :-
		smt_program::serialize([check_sat], [wrong], _, _).

	% smt_expansion tests

	test(smt_expansion_trivial_sat, deterministic(Statements == [set_logic('QF_BV'), declare_const(x, bv(8)), assert(bvugt(x, bv(0, 8))), check_sat, get_model])) :-
		findall(Statement, smt_trivial_sat::smt_statement(Statement, _), Statements).

	test(smt_expansion_push_pop, deterministic(Length == 9)) :-
		findall(Statement, smt_push_pop_fixture::smt_statement(Statement, _), Statements),
		length(Statements, Length).

	test(smt_expansion_raw, deterministic) :-
		findall(Statement, smt_raw_fixture::smt_statement(Statement, _), Statements),
		assertion(Statements == [
			raw('; trusted raw comment'),
			raw([59,32,116,114,117,115,116,101,100,32,114,97,119,32,99,111,109,109,101,110,116]),
			raw([';',' ',t,r,u,s,t,e,d,' ',r,a,w,' ',c,o,m,m,e,n,t]),
			raw("; trusted raw comment")
		]).

	test(smt_expansion_comment, deterministic) :-
		findall(Statement, smt_comment_fixture::smt_statement(Statement, _), Statements),
		assertion(Statements == [comment('a readable comment'), set_logic('QF_LIA')]).

	test(smt_expansion_unrelated, deterministic(Value == ordinary)) :-
		smt_unrelated_fixture::value(Value),
		assertion(\+ smt_unrelated_fixture::current_predicate(smt_statement/2)).

	% smt_output tests

	test(smt_output_sat, deterministic(Results == [smt_result(satisfiable, none, Properties)])) :-
		atom_codes('sat\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], Results),
		Results = [smt_result(_, _, Properties)],
		assertion(memberchk(core(none), Properties)),
		assertion(memberchk(values([]), Properties)).

	test(smt_output_unsat, deterministic(Status == unsatisfiable)) :-
		atom_codes('unsat\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [smt_result(Status, _, _)]).

	test(smt_output_unknown, deterministic(Status == unknown)) :-
		atom_codes('unknown\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [smt_result(Status, _, _)]).

	% Z3 5.3.3 reports a resource-limit result as "timeout" rather than "unknown".
	test(smt_output_z3_timeout, deterministic(Status == unknown)) :-
		atom_codes('timeout\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [smt_result(Status, _, _)]).

	test(smt_output_model_no_wrapper, deterministic(Bindings == [binding(x, 1)])) :-
		atom_codes('sat\n((define-fun x () Int 1))\n', Codes),
		smt_output::decode(cvc5, Codes, exit(0), exit(0), [], [], [smt_result(_, bindings(Bindings), _)]).

	test(smt_output_model_with_wrapper, deterministic(Bindings == [binding(x, 1)])) :-
		atom_codes('sat\n(model (define-fun x () Int 1))\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [smt_result(_, bindings(Bindings), _)]).

	test(smt_output_model_bitvector, deterministic(Bindings == [binding(x, bv(5, 8))])) :-
		atom_codes('sat\n((define-fun x () (_ BitVec 8) #b00000101))\n', Codes),
		smt_output::decode(bitwuzla, Codes, exit(0), exit(0), [], [], [smt_result(_, bindings(Bindings), _)]).

	test(smt_output_unsat_core, deterministic(Core == [a1, a2])) :-
		atom_codes('unsat\n(a1 a2)\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [smt_result(_, _, Properties)]),
		memberchk(core(Core), Properties).

	test(smt_output_empty_unsat_core, deterministic(Core == [])) :-
		atom_codes('unsat\n()\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [smt_result(_, _, Properties)]),
		memberchk(core(Core), Properties).

	test(smt_output_get_value, deterministic(Values == [value(x, 5), value(y, 6)])) :-
		atom_codes('sat\n((x 5) (y 6))\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [smt_result(_, _, Properties)]),
		memberchk(values(Values), Properties).

	test(smt_output_get_value_negative, deterministic(Values == [value(x, -5)])) :-
		atom_codes('sat\n((x (- 5)))\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], [smt_result(_, _, Properties)]),
		memberchk(values(Values), Properties).

	test(smt_output_multiple_episodes, deterministic(Statuses == [satisfiable, unsatisfiable, satisfiable])) :-
		atom_codes('sat\nunsat\nsat\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], Results),
		findall(Status, member(smt_result(Status, _, _), Results), Statuses).

	test(smt_output_error_response, error(smt_solver_reported_error(boom))) :-
		atom_codes('(error "boom")\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], _).

	test(smt_output_unexpected_model, error(domain_error(smt_output, 'Response'-unexpected))) :-
		atom_codes('((define-fun x () Int 1))\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], _).

	test(smt_output_unbalanced_parens, error(domain_error(smt_output, unterminated_list))) :-
		atom_codes('sat\n(a b\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], _).

	test(smt_output_unexpected_close_paren, error(domain_error(smt_output, unexpected_close_paren))) :-
		atom_codes('sat)\n', Codes),
		smt_output::decode(z3, Codes, exit(0), exit(0), [], [], _).

	% A nonzero exit or an interrupted process with no parsed results surfaces as a
	% fatal error instead of silently returning an empty result list (verified
	% against cvc5 1.3.0, which can SIGABRT on a tight --tlimit, and Bitwuzla
	% 0.9.1, which reports parse errors only on stderr with a nonzero exit).
	test(smt_output_abnormal_exit_no_results, error(domain_error(smt_process_status, exit(134)))) :-
		atom_codes('cvc5 interrupted by timeout.\n', Diagnostics),
		smt_output::decode(cvc5, [], exit(134), exit(134), Diagnostics, [], _).

	test(smt_output_clean_exit_no_results, deterministic(Results == [])) :-
		smt_output::decode(z3, [], exit(0), exit(0), [], [], Results).

	test(smt_output_interrupted, error(domain_error(smt_process_status, killed(2)))) :-
		smt_output::decode(z3, [], killed(2), killed(2), [], [], _).

	test(smt_output_invalid_process_status, error(domain_error(smt_process_status, weird(1)))) :-
		atom_codes('sat\n', Codes),
		smt_output::decode(z3, Codes, weird(1), weird(1), [], [], _).

	test(smt_output_unbound, error(instantiation_error)) :-
		smt_output::decode(z3, _, exit(0), exit(0), [], [], _).

	% smt result accessor tests

	test(smt_result_status, deterministic(Status == satisfiable)) :-
		Result = smt_result(satisfiable, none, []),
		smt::status(Result, Status).

	test(smt_result_model_binding, deterministic(Bindings == [x-5, y-6])) :-
		Result = smt_result(satisfiable, bindings([binding(x, 5), binding(y, 6)]), []),
		findall(Name-Value, smt::model_binding(Result, Name-Value), Bindings).

	test(smt_result_model_value, deterministic(Value == 5)) :-
		Result = smt_result(satisfiable, bindings([binding(x, 5)]), []),
		smt::model_value(Result, x, Value).

	test(smt_result_model_value_missing, deterministic) :-
		Result = smt_result(satisfiable, bindings([binding(x, 5)]), []),
		assertion(\+ smt::model_value(Result, z, _)).

	test(smt_result_unsat_core, deterministic(Names == [a1, a2])) :-
		Result = smt_result(unsatisfiable, none, [core([a1, a2])]),
		findall(Name, smt::unsat_core_name(Result, Name), Names).

	test(smt_result_unsat_core_none, deterministic) :-
		Result = smt_result(unsatisfiable, none, [core(none)]),
		assertion(\+ smt::unsat_core_name(Result, _)).

	test(smt_result_property) :-
		Result = smt_result(satisfiable, bindings([binding(x, 5)]), [solver(z3), core(none)]),
		smt::result_property(Result, status(Status)),
		assertion(Status == satisfiable),
		smt::result_property(Result, solver(Solver)),
		assertion(Solver == z3).

	test(smt_result_invalid, error(domain_error(smt_result, invalid))) :-
		smt::status(invalid, _).

	test(smt_result_unbound, error(instantiation_error)) :-
		smt::status(_, _).

	% smt print_result tests

	test(smt_print_result_model, deterministic) :-
		Result = smt_result(satisfiable, bindings([binding(x, 5)]), [core(none), values([])]),
		assert_printed_result(Result, 'status: satisfiable\n  x = 5\n').

	test(smt_print_result_no_model, deterministic) :-
		Result = smt_result(unsatisfiable, none, [core(none), values([])]),
		assert_printed_result(Result, 'status: unsatisfiable\n').

	test(smt_print_result_core, deterministic) :-
		Result = smt_result(unsatisfiable, none, [core([a1, a2]), values([])]),
		assert_printed_result(Result, 'status: unsatisfiable\nunsat core: [a1,a2]\n').

	test(smt_print_result_invalid, error(domain_error(smt_result, invalid))) :-
		smt::print_result(invalid).

	test(smt_print_result_unbound, error(instantiation_error)) :-
		smt::print_result(_).

	test(smt_print_results_multi, deterministic) :-
		^^set_text_output(''),
		smt::print_results([
			smt_result(satisfiable, none, [core(none), values([])]),
			smt_result(unsatisfiable, none, [core(none), values([])])
		]),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'status: satisfiable\n\nstatus: unsatisfiable\n').

	% smt print_program tests

	test(smt_print_program_empty, deterministic) :-
		^^set_text_output(''),
		smt::print_program([]),
		^^text_output_contents(Chars),
		assertion(Chars == []).

	test(smt_print_program_layout, deterministic) :-
		atom_codes('(check-sat)\n', Codes),
		^^set_text_output(''),
		smt::print_program(Codes),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == '(check-sat)\n').

	test(smt_print_program_invalid, error(type_error(integer, p))) :-
		smt::print_program([p]).

	test(smt_print_program_unbound, error(instantiation_error)) :-
		smt::print_program(_).

	% capability tests

	test(smt_capability_z3_quantifiers, deterministic) :-
		assertion(smt::capability(z3, quantifiers)).

	test(smt_capability_bitwuzla_no_linear_arithmetic, deterministic) :-
		assertion(\+ smt::capability(bitwuzla, linear_arithmetic)).

	% process transport tests

	test(smt_shell_posix_literal, deterministic(Codes == [39,97,39,34,39,34,39,98,59,36,40,41,39])) :-
		atom_codes(Argument, [97,39,98,59,36,40,41]),
		smt_process::quote(posix, Argument, Quoted),
		atom_codes(Quoted, Codes).

	test(smt_shell_windows_literal, deterministic(Codes == [39,97,39,39,98,59,36,40,41,39])) :-
		atom_codes(Argument, [97,39,98,59,36,40,41]),
		smt_process::quote(windows, Argument, Quoted),
		atom_codes(Quoted, Codes).

	test(smt_transport_large_captures) :-
		workspace_names(Before),
		transport_fixture(capture, Executable, Arguments, Path),
		smt_process::run(Executable, Arguments, file(Path), Output, Diagnostics, _, Status),
		length(Output, OutputLength),
		length(Diagnostics, ErrorLength),
		assertion(OutputLength == 131072),
		assertion(ErrorLength == 131072),
		assertion(Status == exit(0)),
		workspace_names(After),
		assertion(Before == After).

	test(smt_transport_failed_child_cleanup, deterministic) :-
		workspace_names(Before),
		transport_fixture(failure, Executable, Arguments, Path),
		smt_process::run(Executable, Arguments, file(Path), _, Diagnostics, _, Status),
		assertion(Status \== exit(0)),
		assertion(Diagnostics \== []),
		workspace_names(After),
		assertion(Before == After).

	% external Z3, cvc5, Bitwuzla, and Yices 2 CLI solving tests (conditional on availability)

	test(smt_solve_trivial_sat_z3, deterministic, [condition(resolve_command_path(z3, _))]) :-
		check_solve_trivial_sat(z3).

	test(smt_solve_trivial_sat_cvc5, deterministic, [condition(resolve_command_path(cvc5, _))]) :-
		check_solve_trivial_sat(cvc5).

	test(smt_solve_trivial_sat_bitwuzla, deterministic, [condition(resolve_command_path(bitwuzla, _))]) :-
		check_solve_trivial_sat(bitwuzla).

	test(smt_solve_trivial_sat_yices2, deterministic, [condition(resolve_command_path('yices-smt2', _))]) :-
		check_solve_trivial_sat(yices2).

	test(smt_solve_trivial_unsat_z3, deterministic(Status == unsatisfiable), [condition(resolve_command_path(z3, _))]) :-
		smt::solve(object(smt_trivial_unsat), [smt_result(Status, _, _)], [solver(z3)]).

	test(smt_solve_trivial_unsat_cvc5, deterministic(Status == unsatisfiable), [condition(resolve_command_path(cvc5, _))]) :-
		smt::solve(object(smt_trivial_unsat), [smt_result(Status, _, _)], [solver(cvc5)]).

	test(smt_solve_trivial_unsat_bitwuzla, deterministic(Status == unsatisfiable), [condition(resolve_command_path(bitwuzla, _))]) :-
		smt::solve(object(smt_trivial_unsat), [smt_result(Status, _, _)], [solver(bitwuzla)]).

	test(smt_solve_trivial_unsat_yices2, deterministic(Status == unsatisfiable), [condition(resolve_command_path('yices-smt2', _))]) :-
		smt::solve(object(smt_trivial_unsat), [smt_result(Status, _, _)], [solver(yices2)]).

	test(smt_solve_unsat_core_z3, deterministic(Names == [a1, a2]), [condition(resolve_command_path(z3, _))]) :-
		check_solve_unsat_core(z3, Names).

	test(smt_solve_unsat_core_cvc5, deterministic(Names == [a1, a2]), [condition(resolve_command_path(cvc5, _))]) :-
		check_solve_unsat_core(cvc5, Names).

	test(smt_solve_unsat_core_bitwuzla, deterministic, [condition(resolve_command_path(bitwuzla, _))]) :-
		check_solve_unsat_core(bitwuzla, Names),
		assertion(msort(Names, [a1, a2])).

	test(smt_solve_unsat_core_yices2, deterministic(Names == [a1, a2]), [condition(resolve_command_path('yices-smt2', _))]) :-
		check_solve_unsat_core(yices2, Names).

	test(smt_solve_push_pop_z3, deterministic, [condition(resolve_command_path(z3, _))]) :-
		check_solve_push_pop(z3).

	test(smt_solve_push_pop_cvc5, deterministic, [condition(resolve_command_path(cvc5, _))]) :-
		check_solve_push_pop(cvc5).

	test(smt_solve_push_pop_bitwuzla, deterministic, [condition(resolve_command_path(bitwuzla, _))]) :-
		check_solve_push_pop(bitwuzla).

	test(smt_solve_push_pop_yices2, deterministic, [condition(resolve_command_path('yices-smt2', _))]) :-
		check_solve_push_pop(yices2).

	test(smt_solve_file_source_z3, deterministic(Status == satisfiable), [condition(resolve_command_path(z3, _))]) :-
		check_solve_file_source(z3, Status).

	test(smt_solve_file_source_cvc5, deterministic(Status == satisfiable), [condition(resolve_command_path(cvc5, _))]) :-
		check_solve_file_source(cvc5, Status).

	test(smt_solve_file_source_bitwuzla, deterministic(Status == satisfiable), [condition(resolve_command_path(bitwuzla, _))]) :-
		check_solve_file_source(bitwuzla, Status).

	test(smt_solve_file_source_yices2, deterministic(Status == satisfiable), [condition(resolve_command_path('yices-smt2', _))]) :-
		check_solve_file_source(yices2, Status).

	test(smt_solve_codes_source_z3, deterministic(Status == satisfiable), [condition(resolve_command_path(z3, _))]) :-
		check_solve_codes_source(z3, Status).

	test(smt_solve_terms_source_z3, deterministic, [condition(resolve_command_path(z3, _))]) :-
		check_solve_terms_source(z3).

	test(smt_solve_invalid_option, error(domain_error(option, time_limit(-1)))) :-
		smt::solve(codes([]), _, [time_limit(-1)]).

	test(smt_solve_invalid_solver, error(domain_error(option, solver(invalid)))) :-
		smt::solve(codes([]), _, [solver(invalid)]).

	test(smt_solve_missing_executable, error(existence_error(command, logtalk_smt_missing_executable))) :-
		smt::solve(codes([]), _, [executable(logtalk_smt_missing_executable)]).

	% cvc5's parser rejects the arity error and reports it with an "(error ...)"
	% response on stdout.
	test(smt_solve_syntax_diagnostics_cvc5, deterministic, [condition(resolve_command_path(cvc5, _))]) :-
		check_solve_syntax_error(cvc5).

	% Yices rejects the arity error and reports it with an "(error ...)" response
	% on stdout.
	test(smt_solve_syntax_diagnostics_yices2, deterministic, [condition(resolve_command_path('yices-smt2', _))]) :-
		check_solve_syntax_error(yices2).

	% Bitwuzla reports parse errors only on stderr, with a nonzero exit and no
	% parsed stdout response, surfaced via the process-status safety net instead.
	test(smt_solve_syntax_diagnostics_bitwuzla, deterministic, [condition(resolve_command_path(bitwuzla, _))]) :-
		check_solve_process_error(bitwuzla, 'test_files/malformed.smt2').

	% Z3's SMT-LIB 2 parser is lenient and does not reliably raise an error for the
	% malformed.smt2 fixture (it reports a trivial "sat" result instead); a missing
	% input file is used instead to exercise the smt_solver_error path and cleanup.
	test(smt_solve_missing_file_z3, deterministic, [condition(resolve_command_path(z3, _))]) :-
		check_solve_process_error(z3, 'test_files/does_not_exist.smt2').

	% auxiliary predicates

	check_solve_trivial_sat(Solver) :-
		smt::solve(object(smt_trivial_sat), Results, [solver(Solver)]),
		Results = [Result],
		Result = smt_result(satisfiable, bindings(_), Properties),
		smt::model_value(Result, x, Value),
		assertion(valid_bitvector_value(Value)),
		assertion(member(source_map([_| _]), Properties)),
		assertion(member(solver(Solver), Properties)).

	valid_bitvector_value(bv(Value, 8)) :-
		integer(Value),
		Value >= 0,
		Value =< 255.

	check_solve_unsat_core(Solver, Names) :-
		smt::solve(object(smt_unsat_core_fixture), [Result], [solver(Solver)]),
		Result = smt_result(unsatisfiable, _, _),
		findall(Name, smt::unsat_core_name(Result, Name), Names).

	check_solve_push_pop(Solver) :-
		smt::solve(object(smt_push_pop_fixture), Results, [solver(Solver)]),
		Results = [R1, R2, R3],
		smt::status(R1, S1),
		smt::status(R2, S2),
		smt::status(R3, S3),
		assertion(S1 == satisfiable),
		assertion(S2 == unsatisfiable),
		assertion(S3 == satisfiable).

	check_solve_file_source(Solver, Status) :-
		^^file_path('test_files/trivial.smt2', Path),
		smt::solve(file(Path), [smt_result(Status, _, _)], [solver(Solver)]).

	check_solve_codes_source(Solver, Status) :-
		atom_codes('(set-logic QF_BV)\n(declare-const x (_ BitVec 8))\n(assert (bvugt x #x00))\n(check-sat)\n', Program),
		smt::solve(codes(Program), [smt_result(Status, _, _)], [solver(Solver)]).

	check_solve_terms_source(Solver) :-
		smt::solve(terms([set_logic('QF_BV'), declare_const(x, bv(8)), assert(bvugt(x, bv(0, 8))), check_sat]), Results, [solver(Solver)]),
		Results = [smt_result(Status, _, _)],
		assertion(Status == satisfiable).

	check_solve_syntax_error(Solver) :-
		workspace_names(Before),
		^^file_path('test_files/malformed.smt2', Path),
		catch(smt::solve(file(Path), _, [solver(Solver)]), error(Formal, _), true),
		assertion(Formal = smt_solver_error(smt_solver_reported_error(_), _, _, _)),
		workspace_names(After),
		assertion(Before == After).

	check_solve_process_error(Solver, RelativePath) :-
		workspace_names(Before),
		^^file_path(RelativePath, Path),
		catch(smt::solve(file(Path), _, [solver(Solver)]), error(Formal, _), true),
		assertion(Formal = smt_solver_error(_, _, _, _)),
		workspace_names(After),
		assertion(Before == After).

	assert_printed_result(Result, Expected) :-
		^^set_text_output(''),
		smt::print_result(Result),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == Expected).

	workspace_names(Names) :-
		temporary_directory(Temporary),
		directory_files(Temporary, Files),
		findall(
			Name,
			(	member(Name, Files),
				sub_atom(Name, 0, _, _, 'logtalk-smt')
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
