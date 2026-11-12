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
		date is 2026-11-12,
		comment is 'Source, decoder, expansion, and CLI integration tests for ProbLog.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		member/2, msort/2
	]).

	:- uses(os, [
		resolve_command_path/2, directory_files/2, temporary_directory/1
	]).

	:- uses(pairs, [
		keys_values/3
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	cover(problog).
	cover(problog_program).
	cover(problog_output).
	cover(problog_process).

	test(problog_source_codes, deterministic(Codes == [97,46,10])) :-
		problog::program(codes([97,46,10]), Codes).

	test(problog_print_program, deterministic(Atom == 'a.\n')) :-
		term_io::with_output_to(atom(Atom), problog::program(codes([97,46,10]))).

	test(problog_source_file, deterministic(Codes == Expected)) :-
		^^file_path('test_files/imports.pl', Path),
		file_to_codes(Path, Expected),
		problog::program(file(Path), Codes).

	test(problog_source_unknown, error(domain_error(problog_source, unknown))) :-
		problog::program(unknown, _).

	test(problog_source_variable, error(instantiation_error)) :-
		problog::program(_, _).

	test(problog_source_wrong_provider, error(domain_error(problog_source, object(list)))) :-
		problog::program(object(list), _).

	test(problog_program_empty, deterministic(Codes == [])) :-
		problog_program::serialize([], Codes).

	test(problog_program_coin_golden, deterministic(Atom == '0.5::heads1.\nquery(heads1).\n')) :-
		problog::program(terms([
			probabilistic_fact(0.5, function(heads1, [])),
			query(function(heads1, []))
		]), Codes),
		atom_codes(Atom, Codes).

	test(problog_program_bad_probability, error(domain_error(problog_statement, _))) :-
		problog_program::serialize([probabilistic_fact(1.1, function(a, []))], _).

	test(problog_program_bad_statement, error(domain_error(problog_statement, bad))) :-
		problog_program::serialize([bad], _).

	test(problog_program_annotated_golden, deterministic(Atom == '0.3::coin(heads); 0.7::coin(tails).\n')) :-
		problog_program::serialize([
			annotated_disjunction([
				0.3-function(coin,[atom(heads)]),
				0.7-function(coin,[atom(tails)])
			], true)
		], Codes),
		atom_codes(Atom, Codes).

	test(problog_program_annotated_overfull, error(domain_error(problog_statement, _))) :-
		problog_program::serialize([
			annotated_disjunction([
				0.8-function(coin,[atom(heads)]),
				0.7-function(coin,[atom(tails)])
			], true)
		], _).

	test(problog_program_bad_origins, error(domain_error(problog_origins, _))) :-
		problog_program::serialize([fact(function(a, []))], [], _, _).

	test(problog_hook_isolation, deterministic) :-
		problog_hook_isolation::ping(pong).

	test(problog_expansion_closed_lists, deterministic) :-
		once(problog_lists::problog_statement(fact(function(proper_list, [Proper])), _)),
		assertion(Proper == list([atom(a), atom(b)])),
		once(problog_lists::problog_statement(fact(function(empty_list, [Empty])), _)),
		assertion(Empty == list([])),
		once(problog_lists::problog_statement(fact(function(improper_list, [Improper])), _)),
		assertion(Improper == list([atom(a)], atom(tail))).

	test(problog_expansion_open_list_sharing, deterministic) :-
		once(problog_lists::problog_statement(fact(function(open_list, [Open, Tail])), _)),
		assertion(Open == list([atom(a), atom(b)], variable('Tail'))),
		assertion(Tail == variable('Tail')).

	test(problog_expansion_anonymous_tail, deterministic) :-
		once(problog_lists::problog_statement(fact(function(anonymous_tail, [Open])), _)),
		assertion(Open == list([atom(a)], anonymous)).

	test(problog_object_source_map, deterministic) :-
		findall(S-O, problog_coins::problog_statement(S,O), Pairs),
		keys_values(Pairs, Statements, Origins),
		problog_program::serialize(Statements, Origins, Codes, SourceMap),
		problog::program(object(problog_coins), Expected),
		assertion(Codes == Expected),
		assertion(SourceMap = [line(1,_,_)|_]).

	test(problog_output_empty, deterministic(Probabilities == [])) :-
		decode('', problog_result(Probabilities, _)).

	test(problog_output_nested_quoted_lists, deterministic(Probabilities == [
		probability(p('a,b', [a,b], f('a b')), 0.25), probability(z, 0), probability(o, 1)
	])) :-
		decode('problog_result(p(\'a,b\', [a,b], f(\'a b\')), 0.25).\r\nproblog_result(z, 0).\nproblog_result(o, 1).',
			problog_result(Probabilities, _)).

	test(problog_output_scientific, deterministic) :-
		decode('problog_result(p(1e-09, 1.2e-05, \'1e-09\', atom1e), 1e-09).', Result),
		once(problog::probability(Result, p(Tiny, Small, '1e-09', atom1e), Probability)),
		assertion(abs(Tiny - 0.000000001) < 0.000000000001),
		assertion(abs(Small - 0.000012) < 0.000000000001),
		assertion(abs(Probability - 0.000000001) < 0.000000000001).

	test(problog_output_wrong_record, error(problog_solver_error(_, exit(0), [], []))) :-
		decode('other(a, 0.5).', _).

	test(problog_output_variable, error(problog_solver_error(_, exit(0), [], []))) :-
		decode('problog_result(p(X), 0.5).', _).

	test(problog_output_bad_probability, error(problog_solver_error(_, exit(0), [], []))) :-
		decode('problog_result(a, 1.01).', _).

	test(problog_output_negative_probability, error(problog_solver_error(_, exit(0), [], []))) :-
		decode('problog_result(a, -0.01).', _).

	test(problog_output_non_numeric_probability, error(problog_solver_error(_, exit(0), [], []))) :-
		decode('problog_result(a, nan).', _).

	test(problog_output_trailing_garbage, error(problog_solver_error(_, exit(0), [], []))) :-
		decode('problog_result(a, 0.5). !!!', _).

	test(problog_output_directive_not_executed, error(problog_solver_error(_, exit(0), [], []))) :-
		decode(':- halt.', _).

	test(problog_output_error_stdout, error(problog_solver_error(process_status(exit(1), stdout([98,97,100])), exit(1), [101], []))) :-
		problog_output::decode([98,97,100], [101], 1, exit(1), [], _).

	test(problog_output_error_source_map, error(problog_solver_error(_, exit(1), [], [line(1,1,source(test,2,2))]))) :-
		problog_output::decode([], [], 1, exit(1), [line(1,1,source(test,2,2))], _).

	test(problog_result_accessors, deterministic) :-
		decode('problog_result(a, 0.5).', Result),
		findall(Query-P, problog::probability(Result, Query, P), Probabilities),
		assertion(Probabilities == [a-0.5]),
		once(problog::result_property(Result, process_status(exit(0)))),
		once(problog::result_property(Result, diagnostics([]))).

	test(problog_result_invalid, error(domain_error(problog_result, _))) :-
		problog::probability(problog_result([probability(a, 3)], []), _, _).

	test(problog_result_variable, error(instantiation_error)) :-
		problog::probability(_, _, _).

	test(problog_print_result, deterministic(Atom == 'a: 0.5\n')) :-
		decode('problog_result(a, 0.5).', Result),
		term_io::with_output_to(atom(Atom), problog::print_result(Result)).

	test(problog_quote_posix, deterministic(Quoted == '\'a\'"\'"\' b\'')) :-
		problog_process::quote(posix, 'a\' b', Quoted).

	test(problog_quote_windows, deterministic(Quoted == '\'a\'\' b\'')) :-
		problog_process::quote(windows, 'a\' b', Quoted).

	test(problog_quote_null, error(domain_error(problog_shell_argument, _))) :-
		atom_codes(Argument, [97,0,98]),
		problog_process::quote(posix, Argument, _).

	test(problog_bad_option, error(domain_error(option, symbolic(true)))) :-
		problog::solve(terms([]), _, [symbolic(true)]).

	test(problog_approximate_backend_rejected, error(domain_error(option, knowledge(kbest)))) :-
		problog::solve(terms([]), _, [knowledge(kbest)]).

	test(problog_bad_timeout, error(domain_error(option, timeout(0)))) :-
		problog::solve(terms([]), _, [timeout(0)]).

	test(problog_variable_timeout, error(domain_error(option, timeout(_)))) :-
		problog::solve(terms([]), _, [timeout(_)]).

	test(problog_missing_executable, error(existence_error(command, 'logtalk-problog-not-an-executable'))) :-
		problog::solve(terms([]), _, [executable('logtalk-problog-not-an-executable')]).

	test(problog_object_coins, deterministic, [condition(problog_command_available)]) :-
		problog::solve(object(problog_coins), Result),
		once(problog::probability(Result, some_heads, P)),
		assertion(abs(P - 0.8) < 0.000001).

	test(problog_object_evidence, deterministic, [condition(problog_command_available)]) :-
		problog::solve(object(problog_coins_evidence), Result),
		once(problog::probability(Result, some_heads, P)),
		assertion(abs(P - 0.6) < 0.000001).

	test(problog_object_annotated_sharing, deterministic, [condition(problog_command_available)]) :-
		problog::solve(object(problog_annotated), Result),
		once(problog::probability(Result, coin(heads), P)),
		assertion(abs(P - 0.3) < 0.000001),
		findall(Same, problog::probability(Result, same_side(Same, Same), _), Sides),
		msort(Sides, Sorted),
		assertion(Sorted == [heads,tails]).

	test(problog_terms_solve, deterministic, [condition(problog_command_available)]) :-
		problog::solve(terms([
			probabilistic_fact(0.5, function(a, [])),
			query(function(a, []))
		]), Result),
		once(problog::probability(Result, a, 0.5)).

	test(problog_terms_arithmetic, deterministic, [condition(problog_command_available)]) :-
		problog::solve(terms([
			rule(function(twice, [variable('X')]), function(is, [
				variable('X'), arithmetic('*', [number(2), number(3)])
			])),
			query(function(twice, [variable('X')]))
		]), Result),
		once(problog::probability(Result, twice(6), P)),
		assertion(abs(P - 1.0) < 0.000001).

	test(problog_codes_zero_one_empty, deterministic, [condition(problog_command_available)]) :-
		atom_codes('0.0::a.\n1.0::b.\nquery(a).\nquery(b).\n', Codes),
		problog::solve(codes(Codes), Result),
		findall(Q-P, problog::probability(Result, Q, P), Pairs),
		msort(Pairs, Sorted),
		assertion(Sorted == [a-0.0,b-1.0]),
		atom_codes('0.5::a.\n', EmptyCodes),
		problog::solve(codes(EmptyCodes), Empty),
		assertion(\+ problog::probability(Empty, _, _)).

	test(problog_duplicate_option_first_wins, deterministic, [condition(problog_command_available)]) :-
		problog::solve(terms([]), Result, [executable(problog), executable('logtalk-problog-not-an-executable')]),
		assertion(\+ problog::probability(Result, _, _)).

	test(problog_relative_import_file_preserved, deterministic, [condition(problog_command_available)]) :-
		^^file_path('test_files/imports.pl', Path),
		file_to_codes(Path, Before),
		problog::solve(file(Path), Result),
		once(problog::probability(Result, imported, P)),
		assertion(abs(P - 0.25) < 0.000001),
		file_to_codes(Path, After),
		assertion(Before == After).

	test(problog_model_arguments, deterministic, [condition(problog_command_available)]) :-
		^^file_path('test_files/arguments.pl', Path),
		problog::solve(file(Path), Result, [arguments(['-hello', 'hello world'])]),
		once(problog::probability(Result, matches, P)),
		assertion(abs(P - 1.0) < 0.000001).

	test(problog_path_quoting, deterministic, [condition(problog_command_available)]) :-
		^^file_path('test_files/path with \' quote.pl', Path),
		problog::solve(file(Path), Result),
		once(problog::probability(Result, path_ok, P)),
		assertion(abs(P - 0.5) < 0.000001).

	test(problog_library_paths, deterministic, [condition(problog_command_available)]) :-
		^^file_path('test_files/library_imports.pl', Path),
		^^file_path('test_files', Library),
		problog::solve(file(Path), Result, [libraries([Library])]),
		once(problog::probability(Result, imported, P)),
		assertion(abs(P - 0.25) < 0.000001).

	test(problog_inference_options, deterministic, [condition(problog_command_available)]) :-
		problog::solve(object(problog_coins_evidence), Result, [
			knowledge(ddnnf), timeout(10), compile_timeout(10),
			logspace(false), propagate_evidence(false)
		]),
		once(problog::probability(Result, some_heads, P)),
		assertion(abs(P - 0.6) < 0.000001).

	test(problog_inconsistent_evidence, error(problog_solver_error(_, exit(1), _, [])), [condition(problog_command_available)]) :-
		atom_codes('0.0::a.\nevidence(a,true).\nquery(a).\n', Codes),
		problog::solve(codes(Codes), _).

	test(problog_transport_cleanup_success, deterministic, [condition(problog_command_available)]) :-
		workspace_names(Before),
		problog::solve(terms([]), _),
		workspace_names(After),
		assertion(Before == After).

	test(problog_transport_cleanup_error, deterministic, [condition(problog_command_available)]) :-
		workspace_names(Before),
		atom_codes('query(missing).\n', Codes),
		catch(problog::solve(codes(Codes), _), Error, true),
		assertion(nonvar(Error)),
		assertion(Error = error(problog_solver_error(_, exit(1), _, []), _)),
		workspace_names(After),
		assertion(Before == After).

	test(problog_diagnostics_capture, deterministic, [condition(problog_command_available)]) :-
		problog::solve(object(problog_diagnostics), Result),
		once(problog::probability(Result, captured, 0.5)),
		once(problog::result_property(Result, diagnostics(Diagnostics))),
		atom_codes(Output, Diagnostics),
		assertion(sub_atom(Output, _, _, _, inference_diagnostic)).

	% auxiliary predicates

	problog_command_available :-
		resolve_command_path(problog, _).

	decode(Atom, Result) :-
		atom_codes(Atom, Codes),
		problog_output::decode(Codes, [], 0, exit(0), [], Result).

	workspace_names(Names) :-
		temporary_directory(Temporary),
		directory_files(Temporary, Files),
		findall(Name, (member(Name, Files), sub_atom(Name, 0, _, _, 'logtalk-problog')), Unsorted),
		sort(Unsorted, Names).

:- end_object.
