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
		comment is 'Unit tests for the souffle library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, msort/2
	]).

	:- uses(os, [
		delete_file/1, directory_files/2, resolve_command_path/2, temporary_directory/1
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	cover(souffle_program).
	cover(souffle_output).
	cover(souffle_expansion).
	cover(souffle_process).
	cover(souffle).

	% -----------------------------------------------------------------------
	% souffle_program: statement and term serialization
	% -----------------------------------------------------------------------

	test(souffle_program_empty, deterministic(Codes == [])) :-
		souffle_program::serialize([], Codes).

	test(souffle_program_decl_single_column, deterministic(Atom == '.decl node(n:number)\n')) :-
		souffle_program::serialize([decl(node, [n-number])], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_decl_multi_column, deterministic(Atom == '.decl edge(x:number, y:number)\n')) :-
		souffle_program::serialize([decl(edge, [x-number, y-number])], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_input_output_printsize, deterministic(Atom == '.input edge\n.output path\n.printsize path\n')) :-
		souffle_program::serialize([input(edge), output(path), printsize(path)], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_fact, deterministic(Atom == 'edge(1,2).\n')) :-
		souffle_program::serialize([fact(function(edge, [number(1), number(2)]))], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_rule_positive_body, deterministic(Atom == 'path(X,Y) :- edge(X,Y).\n')) :-
		souffle_program::serialize([
			rule(function(path, [variable('X'), variable('Y')]), [function(edge, [variable('X'), variable('Y')])])
		], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_rule_negation, deterministic(Atom == 'isolated(X) :- node(X), !edge(X,_).\n')) :-
		souffle_program::serialize([
			rule(function(isolated, [variable('X')]), [
				function(node, [variable('X')]),
				not(function(edge, [variable('X'), anonymous]))
			])
		], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_rule_comparison, deterministic(Atom == 'big(X) :- node(X), X > 2.\n')) :-
		souffle_program::serialize([
			rule(function(big, [variable('X')]), [
				function(node, [variable('X')]),
				compare('>', variable('X'), number(2))
			])
		], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_aggregate_count, deterministic(Atom == 'c(X) :- X = count : { e(_,_) }.\n')) :-
		souffle_program::serialize([
			rule(function(c, [variable('X')]), [
				compare('=', variable('X'), aggregate(count, none, [function(e, [anonymous, anonymous])]))
			])
		], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_aggregate_max, deterministic(Atom == 'm(X) :- X = max Y : { e(Y,_) }.\n')) :-
		souffle_program::serialize([
			rule(function(m, [variable('X')]), [
				compare('=', variable('X'), aggregate(max, variable('Y'), [function(e, [variable('Y'), anonymous])]))
			])
		], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_unsigned, deterministic(Atom == 'u(3u).\n')) :-
		souffle_program::serialize([fact(function(u, [unsigned(3)]))], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_float, deterministic(Atom == 'f(2.5).\n')) :-
		souffle_program::serialize([fact(function(f, [float(2.5)]))], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_string_escaping, deterministic(Atom == 's("hi\\nthere").\n')) :-
		atom_codes('hi\nthere', GreetingCodes),
		souffle_program::serialize([fact(function(s, [string(GreetingCodes)]))], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_cat_and_arithmetic, deterministic(Atom == 'g(X) :- X = cat("a","b"), Y = (1+2).\n')) :-
		souffle_program::serialize([
			rule(function(g, [variable('X')]), [
				compare('=', variable('X'), cat(string([0'a]), string([0'b]))),
				compare('=', variable('Y'), arithmetic('+', [number(1), number(2)]))
			])
		], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_comment_and_raw, deterministic(Atom == '// a comment\n.pragma "foo" "bar"\n')) :-
		atom_codes('a comment', CommentCodes),
		atom_codes('.pragma "foo" "bar"', RawCodes),
		souffle_program::serialize([comment(CommentCodes), raw(RawCodes)], Codes),
		atom_codes(Atom, Codes).

	test(souffle_program_invalid_statement, error(domain_error(souffle_statement, not_a_statement))) :-
		souffle_program::serialize([not_a_statement], _).

	test(souffle_program_invalid_origins, error(domain_error(souffle_origins, _))) :-
		souffle_program::serialize([fact(function(a, []))], [bad_origin], _, _).

	test(souffle_program_valid_identifier, true(( souffle_program::valid_identifier(edge), \+ souffle_program::valid_identifier('Edge') ))).

	test(souffle_program_valid_column_type, true(( souffle_program::valid_column_type(symbol), \+ souffle_program::valid_column_type(string) ))).

	% -----------------------------------------------------------------------
	% souffle_output: CSV and printsize decoding
	% -----------------------------------------------------------------------

	test(souffle_output_decode_relations, deterministic(Relations == [relation(path, [number, number], [[1, 2], [1, 3]])])) :-
		atom_codes('1\t2\n1\t3\n', FileCodes),
		souffle_output::decode([], [], [path-codes(FileCodes)], exit(0), exit(0), [path-[number, number]], [], souffle_result(Relations, _, _)).

	test(souffle_output_decode_typed_columns, deterministic(Row == [3, 3, 2.5, hello])) :-
		atom_codes('3\t3\t2.5\thello\n', FileCodes),
		souffle_output::decode([], [], [t-codes(FileCodes)], exit(0), exit(0), [t-[number, unsigned, float, symbol]], [], souffle_result([relation(t, _, [Row])], _, _)).

	test(souffle_output_decode_float_whole_number, deterministic(Value =~= 1.0)) :-
		atom_codes('1\n', FileCodes),
		souffle_output::decode([], [], [f-codes(FileCodes)], exit(0), exit(0), [f-[float]], [], souffle_result([relation(f, _, [[Value]])], _, _)),
		float(Value).

	test(souffle_output_decode_empty_relation, deterministic(Rows == [])) :-
		souffle_output::decode([], [], [e-codes([])], exit(0), exit(0), [e-[number]], [], souffle_result([relation(e, _, Rows)], _, _)).

	test(souffle_output_decode_missing_relation, error(domain_error(souffle_output, missing-missing))) :-
		souffle_output::decode([], [], [other-codes([])], exit(0), exit(0), [missing-[number]], [], _).

	test(souffle_output_decode_nonzero_exit, error(domain_error(souffle_process_status, exit(1)))) :-
		souffle_output::decode([], [], [], exit(1), exit(1), [], [], _).

	test(souffle_output_decode_printsize, deterministic(Sizes == [size(path, 3)])) :-
		atom_codes('path\t3\n', StdoutCodes),
		souffle_output::decode(StdoutCodes, [], [], exit(0), exit(0), [], [path], souffle_result(_, Sizes, _)).

	test(souffle_output_decode_printsize_missing, error(domain_error(souffle_output, missing-missing))) :-
		souffle_output::decode([], [], [], exit(0), exit(0), [], [missing], _).

	% -----------------------------------------------------------------------
	% souffle_expansion: object-level translation (variable sharing/isolation)
	% -----------------------------------------------------------------------

	test(souffle_expansion_reachability_statements, true) :-
		findall(Statement, souffle_reachability::souffle_statement(Statement, _), Statements),
		assertion(memberchk(decl(edge, [x-number, y-number]), Statements)),
		assertion(memberchk(input(edge), Statements)),
		assertion(memberchk(output(path), Statements)),
		assertion(memberchk(printsize(path), Statements)),
		assertion(memberchk(rule(function(path, [variable('X'), variable('Y')]), [function(edge, [variable('X'), variable('Y')])]), Statements)).

	test(souffle_expansion_variable_isolation, true) :-
		% the two path rules both use X/Y/Z but must not leak bindings across clauses
		findall(Statement, souffle_reachability::souffle_statement(Statement, _), Statements),
		assertion(memberchk(rule(function(path, [variable('X'), variable('Y')]), [
			function(path, [variable('X'), variable('Z')]),
			function(edge, [variable('Z'), variable('Y')])
		]), Statements)).

	test(souffle_expansion_aggregates_and_negation, true) :-
		findall(Statement, souffle_graph_stats::souffle_statement(Statement, _), Statements),
		assertion(memberchk(rule(function(isolated, [variable('X')]), [
			function(node, [variable('X')]),
			not(function(edge, [variable('X'), anonymous])),
			not(function(edge, [anonymous, variable('X')]))
		]), Statements)),
		assertion(memberchk(rule(function(edge_count, [variable('X')]), [
			compare('=', variable('X'), aggregate(count, none, [function(edge, [anonymous, anonymous])]))
		]), Statements)).

	test(souffle_program_object_source, true) :-
		souffle::program(object(souffle_reachability), Codes),
		atom_codes(Atom, Codes),
		assertion(sub_atom(Atom, _, _, _, '.decl edge(x:number, y:number)')),
		assertion(sub_atom(Atom, _, _, _, '.input edge')),
		assertion(sub_atom(Atom, _, _, _, 'path(X,Y) :- edge(X,Y).')).

	% -----------------------------------------------------------------------
	% souffle facade: option validation
	% -----------------------------------------------------------------------

	test(souffle_solve_facts_mutual_exclusion, error(domain_error(souffle_facts_option, _))) :-
		souffle::solve(terms([]), _, [facts('/tmp'), facts_data([edge-[[1, 2]]])]).

	test(souffle_solve_missing_executable, error(existence_error(command, logtalk_souffle_missing_executable))) :-
		souffle::solve(terms([]), _, [executable(logtalk_souffle_missing_executable)]).

	% -----------------------------------------------------------------------
	% souffle facade: solving (conditional on the Souffle CLI being available)
	% -----------------------------------------------------------------------

	test(souffle_solve_reachability_object, deterministic(Sorted == [[1, 2], [1, 3], [2, 3]]), [condition(resolve_command_path(souffle, _))]) :-
		souffle::solve(
			object(souffle_reachability),
			Result,
			[facts_data([edge-[[1, 2], [2, 3]]]), outputs([path-[number, number]]), printsizes([path])]
		),
		once(souffle::relation(Result, path, _, Rows)),
		msort(Rows, Sorted),
		assertion(souffle::size(Result, path, 3)).

	test(souffle_solve_reachability_terms, deterministic(Sorted == [[1, 2], [1, 3], [2, 3]]), [condition(resolve_command_path(souffle, _))]) :-
		souffle::solve(
			terms([
				decl(edge, [x-number, y-number]), input(edge),
				decl(path, [x-number, y-number]), output(path),
				rule(function(path, [variable('X'), variable('Y')]), [function(edge, [variable('X'), variable('Y')])]),
				rule(function(path, [variable('X'), variable('Y')]), [function(path, [variable('X'), variable('Z')]), function(edge, [variable('Z'), variable('Y')])])
			]),
			Result,
			[facts_data([edge-[[1, 2], [2, 3]]]), outputs([path-[number, number]])]
		),
		once(souffle::relation(Result, path, _, Rows)),
		msort(Rows, Sorted).

	test(souffle_solve_reachability_codes, deterministic(Sorted == [[1, 2], [1, 3], [2, 3]]), [condition(resolve_command_path(souffle, _))]) :-
		souffle::program(object(souffle_reachability), Codes),
		souffle::solve(
			codes(Codes),
			Result,
			[facts_data([edge-[[1, 2], [2, 3]]]), outputs([path-[number, number]])]
		),
		once(souffle::relation(Result, path, _, Rows)),
		msort(Rows, Sorted).

	test(souffle_solve_reachability_file, deterministic(Sorted == [[1, 2], [1, 3], [2, 3]]), [condition(resolve_command_path(souffle, _))]) :-
		^^file_path('test_files/reachability.dl', Path),
		souffle::solve(
			file(Path),
			Result,
			[facts_data([edge-[[1, 2], [2, 3]]]), outputs([path-[number, number]])]
		),
		once(souffle::relation(Result, path, _, Rows)),
		msort(Rows, Sorted).

	test(souffle_solve_external_facts_directory, deterministic(Sorted == [[1, 2], [1, 3], [2, 3]]), [condition(resolve_command_path(souffle, _))]) :-
		^^file_path('test_files', Path),
		souffle::solve(
			object(souffle_reachability),
			Result,
			[facts(Path), outputs([path-[number, number]])]
		),
		once(souffle::relation(Result, path, _, Rows)),
		msort(Rows, Sorted).

	test(souffle_solve_missing_input_facts_is_error, error(souffle_solver_error(domain_error(souffle_process_status, exit(1)), _, _, _)), [condition(resolve_command_path(souffle, _))]) :-
		% a used .input relation (read by a rule) with no matching .facts file
		% is a hard Souffle error, not a silently empty relation (verified
		% empirically; only an otherwise-unused/dead-code relation is silent)
		souffle::solve(
			object(souffle_reachability),
			_,
			[outputs([path-[number, number]])]
		).

	test(souffle_solve_graph_stats, true, [condition(resolve_command_path(souffle, _))]) :-
		souffle::solve(
			object(souffle_graph_stats),
			Result,
			[outputs([
				node-[number], isolated-[number], edge_count-[number], max_x-[number],
				min_x-[number], sum_x-[number], mean_weight-[float], greeting-[symbol], big_x-[number]
			])]
		),
		souffle::relation(Result, node, _, NodeRows), msort(NodeRows, SortedNodes),
		assertion(SortedNodes == [[1], [2], [3], [4]]),
		souffle::relation(Result, isolated, _, IsolatedRows),
		assertion(IsolatedRows == []),
		souffle::relation(Result, edge_count, _, [[3]]),
		souffle::relation(Result, max_x, _, [[3]]),
		souffle::relation(Result, min_x, _, [[1]]),
		souffle::relation(Result, sum_x, _, [[6]]),
		souffle::relation(Result, mean_weight, _, [[Mean]]),
		assertion(abs(Mean - 2.0) < 0.0001),
		souffle::relation(Result, greeting, _, [['hello world']]),
		souffle::relation(Result, big_x, _, BigRows), msort(BigRows, SortedBig),
		assertion(SortedBig == [[3], [4]]).

	test(souffle_solve_syntax_error, deterministic, [condition(resolve_command_path(souffle, _))]) :-
		workspace_names(Before),
		atom_codes('this is not valid syntax !!!', Program),
		catch(
			souffle::solve(codes(Program), _, [outputs([x-[number]])]),
			error(souffle_solver_error(_, _, _, _), _),
			true
		),
		workspace_names(After),
		assertion(Before == After).

	test(souffle_transport_cleanup_success, deterministic, [condition(resolve_command_path(souffle, _))]) :-
		workspace_names(Before),
		souffle::solve(object(souffle_reachability), _, [facts_data([edge-[[1, 2]]]), outputs([path-[number, number]])]),
		workspace_names(After),
		assertion(Before == After).

	test(souffle_print_result, true) :-
		with_output_to_atom(
			souffle::print_result(souffle_result([relation(path, [number, number], [[1, 2]])], [size(path, 1)], [])),
			Atom
		),
		assertion(sub_atom(Atom, _, _, _, 'path:')),
		assertion(sub_atom(Atom, _, _, _, 'path = 1')).

	% -----------------------------------------------------------------------
	% Auxiliary predicates
	% -----------------------------------------------------------------------

	workspace_names(Names) :-
		temporary_directory(Temporary),
		directory_files(Temporary, Files),
		findall(
			Name,
			(	member(Name, Files),
				sub_atom(Name, 0, _, _, 'logtalk-souffle')
			),
			Unsorted
		),
		sort(Unsorted, Names).

	with_output_to_atom(Goal, Atom) :-
		^^file_path('souffle_print_result.tmp', Path),
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
