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
		comment is 'Unit tests for the clingo library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	:- uses(os, [
		directory_files/2, operating_system_type/1, resolve_command_path/2, temporary_directory/1
	]).

	:- uses(list, [
		member/2, msort/2
	]).

	cover(clingo_program).
	cover(clingo_symbols).
	cover(clingo_output).
	cover(clingo_expansion).
	cover(clingo).
	cover(clingo_process).

	test(clingo_program_empty, deterministic(Codes == [])) :-
		clingo_program::serialize([], Codes).

	test(clingo_program_fact, deterministic(Atom == 'p(1,-q).\n')) :-
		clingo_program::serialize([fact(function(p, [number(1), function(q, [], negative)], positive))], Codes),
		atom_codes(Atom, Codes).

	test(clingo_program_rule, deterministic(Atom == 'p(X) :- q(X), not r(X), X != 2.\n')) :-
		clingo_program::serialize([
			rule(atom(function(p, [variable('X')], positive)), [
				function(q, [variable('X')], positive),
				not(function(r, [variable('X')], positive)),
				compare('!=', variable('X'), number(2))
			])
		], Codes),
		atom_codes(Atom, Codes).

	test(clingo_program_body_conditional, deterministic(Atom == 'q :- p(X) : r(X); s.\n')) :-
		clingo_program::serialize([
			rule(atom(function(q, [], positive)), [
				conditional(function(p, [variable('X')], positive), [function(r, [variable('X')], positive)]),
				function(s, [], positive)
			])
		], Codes),
		atom_codes(Atom, Codes).

	test(clingo_program_empty_aggregate_tuple, deterministic(Atom == ':- #count{:} > 1.\n')) :-
		clingo_program::serialize([
			constraint([compare('>', aggregate(count, [aggregate_element([], [])]), number(1))])
		], Codes),
		atom_codes(Atom, Codes).

	test(clingo_program_empty_conditional, deterministic(Atom == 'q :- p : ; r.\n')) :-
		clingo_program::serialize([
			rule(atom(function(q, [], positive)), [conditional(function(p, [], positive), []), function(r, [], positive)])
		], Codes),
		atom_codes(Atom, Codes).

	test(clingo_program_choice, deterministic(Atom == '1 {p(X) : q(X); r} 2.\n')) :-
		clingo_program::serialize([
			rule(choice(number(1), [
				conditional(function(p, [variable('X')], positive), [function(q, [variable('X')], positive)]),
				conditional(function(r, [], positive), [])
			], number(2)), [])
		], Codes),
		atom_codes(Atom, Codes).

	test(clingo_program_aggregate, deterministic(Atom == ':- #sum{W,X : p(X,W)} > 3.\n')) :-
		clingo_program::serialize([
			constraint([compare('>', aggregate(sum, [
				aggregate_element([variable('W'), variable('X')], [function(p, [variable('X'), variable('W')], positive)])
			]), number(3))])
		], Codes),
		atom_codes(Atom, Codes).

	test(clingo_program_objectives, deterministic(Atom == '#minimize {-2@1,X : p(X)}.\n#maximize {}.\n:~ p. [1@0]\n')) :-
		clingo_program::serialize([
			minimize([objective(number(-2), number(1), [variable('X')], [function(p, [variable('X')], positive)])]),
			maximize([]),
			weak_constraint([function(p, [], positive)], number(1), number(0), [])
		], Codes),
		atom_codes(Atom, Codes).

	test(clingo_program_show_constant, deterministic(Atom == '#const n = (2**3).\n#show -p/1.\n#show.\n#show X : p(X).\n')) :-
		clingo_program::serialize([
			constant(n, arithmetic('**', [number(2), number(3)])),
			show(signature(p, 1, negative)),
			show(none),
			show_term(variable('X'), [function(p, [variable('X')], positive)])
		], Codes),
		atom_codes(Atom, Codes).

	test(clingo_program_tuple, deterministic(Atom == 'p((),(1,),(1,2),#inf,#sup,_).\n')) :-
		clingo_program::serialize([fact(function(p, [
			tuple([]), tuple([number(1)]), tuple([number(1), number(2)]), infimum, supremum, anonymous
		], positive))], Codes),
		atom_codes(Atom, Codes).

	test(clingo_program_string, deterministic(Codes == [112,40,34,97,92,110,92,34,92,92,34,41,46,10])) :-
		clingo_program::serialize([fact(function(p, [string([97,10,34,92])], positive))], Codes).

	test(clingo_program_source_map, deterministic) :-
		clingo_program::serialize([
			raw([112,46,10,113,46]),
			fact(function(r, [], positive))
		], [source('fixture.lgt', 3, 4), unknown], Codes, Map),
		assertion(Codes == [112,46,10,113,46,10,114,46,10]),
		assertion(Map == [line(1, 2, source('fixture.lgt', 3, 4)), line(3, 3, unknown)]).

	test(clingo_program_unbound, error(instantiation_error)) :-
		clingo_program::serialize(_, _).

	test(clingo_program_non_ground, error(instantiation_error)) :-
		clingo_program::serialize([fact(function(p, [_], positive))], _).

	test(clingo_program_non_list, error(type_error(list, invalid))) :-
		clingo_program::serialize(invalid, _).

	test(clingo_program_invalid_statement, error(domain_error(clingo_statement, call(halt)))) :-
		clingo_program::serialize([call(halt)], _).

	test(clingo_program_invalid_identifier, error(domain_error(clingo_statement, fact(function('P', [], positive))))) :-
		clingo_program::serialize([fact(function('P', [], positive))], _).

	test(clingo_program_variable_constant, error(domain_error(clingo_statement, constant(n, variable('X'))))) :-
		clingo_program::serialize([constant(n, variable('X'))], _).

	test(clingo_program_invalid_origins, error(domain_error(clingo_origins, []))) :-
		clingo_program::serialize([fact(function(p, [], positive))], [], _, _).

	test(clingo_symbols_nested, deterministic(Symbol == function(p, [number(-12), function(q, [], negative), tuple([number(1)]), infimum, supremum], positive))) :-
		atom_codes('p(-12,-q,(1,),#inf,#sup)', Codes),
		clingo_symbols::parse(Codes, Symbol).

	test(clingo_symbols_empty_tuple, deterministic(Symbol == tuple([]))) :-
		clingo_symbols::parse([0'(, 0')], Symbol).

	test(clingo_symbols_tuple, deterministic(Symbol == tuple([number(1), function(a, [], positive)]))) :-
		atom_codes('(1,a)', Codes),
		clingo_symbols::parse(Codes, Symbol).

	test(clingo_symbols_string, deterministic(Symbol == string([97,10,34,92]))) :-
		clingo_symbols::parse([34,97,92,110,92,34,92,92,34], Symbol).

	test(clingo_symbols_trailing_input, error(domain_error(clingo_symbol, [97,32,98]))) :-
		clingo_symbols::parse([97,32,98], _).

	test(clingo_symbols_unclosed_string, error(domain_error(clingo_symbol, [34,97]))) :-
		clingo_symbols::parse([34,97], _).

	test(clingo_symbols_not_prolog, error(domain_error(clingo_symbol, [39,97,39]))) :-
		clingo_symbols::parse([39,97,39], _).

	test(clingo_symbols_parenthesized_number, error(domain_error(clingo_symbol, [40,49,41]))) :-
		clingo_symbols::parse([40,49,41], _).

	test(clingo_symbols_invalid_identifier, error(domain_error(clingo_symbol, [95]))) :-
		clingo_symbols::parse([95], _).

	test(clingo_symbols_reserved_identifier, error(domain_error(clingo_symbol, [110,111,116]))) :-
		clingo_symbols::parse([110,111,116], _).

	test(clingo_output_optional_witnesses, deterministic) :-
		atom_codes('{"Result":"SATISFIABLE"}', Codes),
		clingo_output::decode(Codes, exit(10), exit(10), [], [], clingo_result(Status, Models, Properties)),
		assertion(Status == satisfiable),
		assertion(Models == []),
		assertion(member(enumeration_complete(unknown), Properties)).

	test(clingo_output_sat, deterministic) :-
		fixture_codes('sat.json', Codes),
		clingo_output::decode(Codes, exit(30), exit(30), [], [], clingo_result(Status, Models, Properties)),
		assertion(Status == satisfiable),
		assertion(Models == [model([], none, []), model([function(p, [], positive)], none, [p])]),
		assertion(member(enumeration_complete(true), Properties)).

	test(clingo_output_unsat, deterministic) :-
		fixture_codes('unsat.json', Codes),
		clingo_output::decode(Codes, 5120, exit(20), [], [], clingo_result(Status, Models, _)),
		assertion(Status == unsatisfiable),
		assertion(Models == []).

	test(clingo_output_optimum, deterministic) :-
		fixture_codes('optimum.json', Codes),
		clingo_output::decode(Codes, exit(30), exit(30), [119], [], clingo_result(Status, Models, Properties)),
		assertion(Status == satisfiable),
		assertion(Models == [
			model([function(p, [number(2)], positive)], [2], ['p(2)']),
			model([function(p, [number(1)], positive)], [1], ['p(1)'])
		]),
		assertion(member(optimality(proven), Properties)),
		assertion(member(costs([1]), Properties)),
		assertion(member(diagnostics([119]), Properties)).

	test(clingo_output_time_limit, deterministic) :-
		fixture_codes('time_limit.json', Codes),
		clingo_output::decode(Codes, exit(1), exit(1), [], [], clingo_result(Status, Models, Properties)),
		assertion(Status == unknown),
		assertion(Models == []),
		assertion(member(termination(time_limit), Properties)),
		assertion(member(enumeration_complete(false), Properties)).

	test(clingo_output_fatal, error(domain_error(clingo_process_status, exit(65)))) :-
		fixture_codes('sat.json', Codes),
		clingo_output::decode(Codes, exit(65), exit(65), [], [], _).

	test(clingo_output_inconsistent_status, error(domain_error(clingo_process_status, exit(20)))) :-
		fixture_codes('sat.json', Codes),
		clingo_output::decode(Codes, exit(20), exit(20), [], [], _).

	test(clingo_output_invalid_value, error(domain_error(clingo_output, 'Value'-42))) :-
		fixture_codes('invalid_value.json', Codes),
		clingo_output::decode(Codes, exit(10), exit(10), [], [], _).

	test(clingo_output_truncated, error(domain_error(json, _))) :-
		fixture_codes('truncated.json', Codes),
		clingo_output::decode(Codes, exit(10), exit(10), [], [], _).

	test(clingo_expansion_reachability, deterministic) :-
		findall(Statement, clingo_reachability::asp_statement(Statement, _), Statements),
		clingo_program::serialize(Statements, Codes),
		atom_codes(Program, Codes),
		assertion(Program == 'edge(a,b).\nedge(b,c).\nreach(Start,End) :- edge(Start,End).\nreach(Start,End) :- edge(Start,Middle), reach(Middle,End).\n#show reach/2.\n').

	test(clingo_expansion_variables, deterministic) :-
		clingo_variable_fixture::asp_statement(fact(function(p, Arguments, positive)), _),
		assertion(Arguments == [variable('Value'), variable('Value'), anonymous, anonymous, variable('_Named'), variable('_Named')]).

	test(clingo_expansion_unconditional_heads, deterministic) :-
		findall(Head, clingo_team_selection::asp_statement(rule(Head, []), _), Heads),
		assertion(Heads == [
			choice(function(size, [], positive), [
				conditional(function(selected, [variable('Person')], positive), [function(person, [variable('Person')], positive)])
			], function(size, [], positive)),
			choice(none, [
				conditional(function(reserve, [variable('Person')], positive), [function(person, [variable('Person')], positive)])
			], none),
			disjunction([function(onsite, [], positive), function(remote, [], positive)])
		]).

	test(clingo_expansion_raw_text, deterministic) :-
		atom_codes('#program base.', Codes),
		findall(Statement, clingo_raw_fixture::asp_statement(Statement, _), Statements),
		assertion(Statements == [raw(Codes), raw(Codes), raw(Codes), raw(Codes)]),
		clingo::program(object(clingo_raw_fixture), ProgramCodes),
		atom_codes(Program, ProgramCodes),
		assertion(Program == '#program base.\n#program base.\n#program base.\n#program base.\n').

	test(clingo_expansion_body_is_data) :-
		clingo_variable_fixture::asp_statement(rule(atom(function(rule_as_data, [], positive)), Body), _),
		assertion(Body == [not(function(halt, [], positive))]),
		assertion(\+ clingo_variable_fixture::current_predicate(rule_as_data/0)).

	test(clingo_expansion_unrelated) :-
		clingo_unrelated_fixture::value(Value),
		assertion(Value == ordinary),
		assertion(\+ clingo_unrelated_fixture::current_predicate(asp_statement/2)).

	test(clingo_program_object_source, deterministic(Codes == Expected)) :-
		clingo::program(object(clingo_reachability), Codes),
		findall(Statement, clingo_reachability::asp_statement(Statement, _), Statements),
		clingo::program(terms(Statements), Expected).

	test(clingo_program_codes_source, deterministic(Codes == [112,46,10])) :-
		clingo::program(codes([112,46,10]), Codes).

	test(clingo_program_wrong_object, error(domain_error(clingo_source, object(clingo_unrelated_fixture)))) :-
		clingo::program(object(clingo_unrelated_fixture), _).

	test(clingo_program_print_codes, deterministic) :-
		atom_codes('p.\n\tq("a b").', Codes),
		^^set_text_output(''),
		clingo::program(codes(Codes)),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p.\n\tq("a b").').

	test(clingo_program_print_terms, deterministic) :-
		^^set_text_output(''),
		clingo::program(terms([fact(function(p, [], positive))])),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p.\n').

	test(clingo_program_print_object, deterministic) :-
		clingo::program(object(clingo_queens), Codes),
		atom_codes(Expected, Codes),
		^^set_text_output(''),
		clingo::program(object(clingo_queens)),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == Expected).

	test(clingo_program_print_invalid, error(domain_error(clingo_source, invalid))) :-
		clingo::program(invalid).

	test(clingo_program_print_unbound, error(instantiation_error)) :-
		clingo::program(_).

	test(clingo_result_models) :-
		fixture_codes('sat.json', Codes),
		clingo_output::decode(Codes, exit(30), exit(30), [], [], Result),
		findall(Model, clingo::model(Result, Model), Models),
		assertion(Models == [model([], none, []), model([function(p, [], positive)], none, [p])]).

	test(clingo_result_property) :-
		fixture_codes('sat.json', Codes),
		clingo_output::decode(Codes, exit(30), exit(30), [], [], Result),
		clingo::result_property(Result, status(Status)),
		assertion(Status == satisfiable),
		clingo::result_property(Result, model_count(Count)),
		assertion(Count == 2).

	test(clingo_result_invalid, error(domain_error(clingo_result, invalid))) :-
		clingo::model(invalid, _).

	test(clingo_print_program_empty, deterministic) :-
		^^set_text_output(''),
		clingo::print_program([]),
		^^text_output_contents(Chars),
		assertion(Chars == []).

	test(clingo_print_program_layout, deterministic) :-
		atom_codes('p.\n\tq("a b").', Codes),
		^^set_text_output(''),
		clingo::print_program(Codes),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p.\n\tq("a b").').

	test(clingo_print_program_generated, deterministic) :-
		clingo::program(terms([fact(function(p, [], positive))]), Codes),
		^^set_text_output(''),
		clingo::print_program(Codes),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p.\n').

	test(clingo_print_program_invalid, error(type_error(integer, p))) :-
		clingo::print_program([p]).

	test(clingo_print_program_invalid_code, error(domain_error(character_code, -1))) :-
		clingo::print_program([112, -1]).

	test(clingo_print_program_unbound, error(instantiation_error)) :-
		clingo::print_program(_).

	test(clingo_print_program_partial, error(instantiation_error)) :-
		clingo::print_program([112| _]).

	test(clingo_print_result_answers, deterministic) :-
		Result = clingo_result(satisfiable, [model([], none, []), model([function(p, [], positive)], none, [p])], [
			solver('clingo version 5.8.3'), model_count(2), enumeration_complete(true)
		]),
		assert_printed_result(Result, 'clingo version 5.8.3\nAnswer: 1\n\nAnswer: 2\np\nSATISFIABLE\n\nModels       : 2\n').

	test(clingo_print_result_optimum, deterministic) :-
		Result = clingo_result(satisfiable, [
			model([], [2,-3], ['p(2)']),
			model([], [1,-4], ['p(1)'])
		], [model_count(2), enumeration_complete(true), optimality(proven), costs([1,-4])]),
		assert_printed_result(Result, 'Answer: 1\np(2)\nOptimization: 2 -3\nAnswer: 2\np(1)\nOptimization: 1 -4\nOPTIMUM FOUND\n\nModels       : 2\n  Optimum    : yes\nOptimization : 1 -4\n').

	test(clingo_print_result_unproven, deterministic) :-
		Result = clingo_result(satisfiable, [model([], [2], [p])], [
			model_count(1), enumeration_complete(false), optimality(unproven), costs([2])
		]),
		assert_printed_result(Result, 'Answer: 1\np\nOptimization: 2\nSATISFIABLE\n\nModels       : 1+\n  Optimum    : unknown\nOptimization : 2\n').

	test(clingo_print_result_unsatisfiable, deterministic) :-
		Result = clingo_result(unsatisfiable, [], [model_count(0), enumeration_complete(true)]),
		assert_printed_result(Result, 'UNSATISFIABLE\n\nModels       : 0\n').

	test(clingo_print_result_time_limit, deterministic) :-
		Result = clingo_result(unknown, [], [termination(time_limit), model_count(0), enumeration_complete(false)]),
		assert_printed_result(Result, 'UNKNOWN\n\nTIME LIMIT   : 1\nModels       : 0+\n').

	test(clingo_print_result_interrupted, deterministic) :-
		Result = clingo_result(unknown, [], [termination(interrupted), model_count(unknown), enumeration_complete(unknown)]),
		assert_printed_result(Result, 'UNKNOWN\n\nINTERRUPTED  : 1\nModels       : unknown?\n').

	test(clingo_print_result_raw_values, deterministic) :-
		Result = clingo_result(satisfiable, [model([], none, ['p("a b\\n")', '-q'])], [model_count(1), enumeration_complete(true)]),
		assert_printed_result(Result, 'Answer: 1\np("a b\\n") -q\nSATISFIABLE\n\nModels       : 1\n').

	test(clingo_print_result_time, deterministic) :-
		Result = clingo_result(unsatisfiable, [], [model_count(0), enumeration_complete(true), time(json(['Total'-2, 'CPU'-1]))]),
		assert_printed_result(Result, 'UNSATISFIABLE\n\nModels       : 0\nTime         : 2s\nCPU Time     : 1s\n').

	test(clingo_print_result_invalid, error(domain_error(clingo_result, invalid))) :-
		clingo::print_result(invalid).

	test(clingo_print_result_unbound, error(instantiation_error)) :-
		clingo::print_result(_).

	test(clingo_cli_reachability, true, [condition((current_object(process), resolve_command_path(clingo, _)))]) :-
		clingo::program(object(clingo_reachability), Program),
		cli_fixture_result(Program, Result),
		findall(Model, clingo::model(Result, Model), Models),
		Models = [model(Symbols, none, RawValues)],
		assertion(member(function(reach, [function(a, [], positive), function(c, [], positive)], positive), Symbols)),
		assertion(member('reach(a,c)', RawValues)),
		clingo::result_property(Result, enumeration_complete(Complete)),
		assertion(Complete == true),
		clingo::result_property(Result, diagnostics(Diagnostics)),
		assertion(Diagnostics == []).

	test(clingo_cli_empty_conditional, true, [condition((current_object(process), resolve_command_path(clingo, _)))]) :-
		clingo::program(terms([
			fact(function(p, [], positive)),
			fact(function(r, [], positive)),
			rule(atom(function(q, [], positive)), [conditional(function(p, [], positive), []), function(r, [], positive)]),
			show(signature(q, 0, positive))
		]), Program),
		cli_fixture_result(Program, Result),
		findall(Model, clingo::model(Result, Model), Models),
		assertion(Models == [model([function(q, [], positive)], none, [q])]).

	test(clingo_shell_posix_literal, deterministic(Codes == [39,97,39,34,39,34,39,98,59,36,40,41,39])) :-
		atom_codes(Argument, [97,39,98,59,36,40,41]),
		clingo_process::quote(posix, Argument, Quoted),
		atom_codes(Quoted, Codes).

	test(clingo_solve_reachability, deterministic(Status == satisfiable), [condition(resolve_command_path(clingo, _))]) :-
		clingo::solve(object(clingo_reachability), clingo_result(Status, [_], Properties)),
		assertion(member(source_map([_| _]), Properties)).

	test(clingo_solve_all_models, true, [condition(resolve_command_path(clingo, _))]) :-
		atom_codes('{p}.', Program),
		clingo::solve(codes(Program), Result, [models(0), models(1)]),
		findall(Model, clingo::model(Result, Model), Models),
		length(Models, Count),
		assertion(Count == 2),
		clingo::result_property(Result, enumeration_complete(Complete)),
		assertion(Complete == true).

	test(clingo_solve_constants, true, [condition(resolve_command_path(clingo, _))]) :-
		atom_codes('#const n=1. p(n).', Program),
		clingo::solve(codes(Program), Result, [constants([n-number(3)])]),
		findall(Model, clingo::model(Result, Model), Models),
		assertion(Models == [model([function(p, [number(3)], positive)], none, ['p(3)'])]).

	test(clingo_shell_windows_literal, deterministic(Codes == [39,97,39,39,98,59,36,40,41,39])) :-
		atom_codes(Argument, [97,39,98,59,36,40,41]),
		clingo_process::quote(windows, Argument, Quoted),
		atom_codes(Quoted, Codes).

	test(clingo_transport_large_captures) :-
		workspace_names(Before),
		transport_fixture(capture, Executable, Arguments, Path),
		clingo_process::run(Executable, Arguments, file(Path), Output, Diagnostics, _, Status),
		length(Output, OutputLength),
		length(Diagnostics, ErrorLength),
		assertion(OutputLength == 131072),
		assertion(ErrorLength == 131072),
		assertion(Status == exit(0)),
		workspace_names(After),
		assertion(Before == After).

	test(clingo_solve_native_include_utf8, true, [condition((resolve_command_path(clingo, _), \+ current_logtalk_flag(prolog_dialect, xvm)))]) :-
		^^file_path('test_files/shell data ''$();/main.lp', Path),
		file_to_codes(Path, Before),
		clingo::solve(file(Path), Result),
		findall(Model, clingo::model(Result, Model), Models),
		atom_codes(RawValue, [112,40,34,99,97,102,233,34,41]),
		assertion(Models == [model([function(p, [string([99,97,102,233])], positive)], none, [RawValue])]),
		file_to_codes(Path, After),
		assertion(Before == After).

	test(clingo_solve_four_queens, true, [condition(resolve_command_path(clingo, _))]) :-
		clingo::solve(object(clingo_queens), Result, [models(0), constants([n-number(4)])]),
		Result = clingo_result(Status, _, _),
		assertion(Status == satisfiable),
		findall(
			Sorted,
			(	clingo::model(Result, model(Symbols, none, _)),
				sort(Symbols, Sorted)
			),
			Solutions
		),
		msort(Solutions, SortedSolutions),
		assertion(SortedSolutions == [
			[
				function(q, [number(1), number(2)], positive),
				function(q, [number(2), number(4)], positive),
				function(q, [number(3), number(1)], positive),
				function(q, [number(4), number(3)], positive)
			],
			[
				function(q, [number(1), number(3)], positive),
				function(q, [number(2), number(1)], positive),
				function(q, [number(3), number(4)], positive),
				function(q, [number(4), number(2)], positive)
			]
		]),
		clingo::result_property(Result, enumeration_complete(Complete)),
		assertion(Complete == true).

	test(clingo_solve_minimum_vertex_cover, true, [condition(resolve_command_path(clingo, _))]) :-
		clingo::solve(object(clingo_vertex_cover), Result, [models(0)]),
		clingo::result_property(Result, optimality(Optimality)),
		clingo::result_property(Result, costs(Costs)),
		assertion(Optimality == proven),
		assertion(Costs == [1]),
		findall(Symbols, clingo::model(Result, model(Symbols, [1], _)), OptimalSolutions),
		assertion(OptimalSolutions == [[function(cover, [function(b, [], positive)], positive)]]).

	test(clingo_solve_team_selection_object, deterministic, [condition(resolve_command_path(clingo, _))]) :-
		clingo::solve(object(clingo_team_selection), Result, [models(0)]),
		check_team_result(Result).

	test(clingo_solve_unsatisfiable, deterministic, [condition(resolve_command_path(clingo, _))]) :-
		atom_codes(':- .', Program),
		clingo::solve(codes(Program), clingo_result(Status, Models, _)),
		assertion(Status == unsatisfiable),
		assertion(Models == []).

	test(clingo_solve_optimization, true, [condition(resolve_command_path(clingo, _))]) :-
		atom_codes('1 {p(1);p(2)} 1. #minimize {X:p(X)}.', Program),
		clingo::solve(codes(Program), Result, [models(0), statistics(1)]),
		clingo::result_property(Result, optimality(Optimality)),
		clingo::result_property(Result, costs(Costs)),
		assertion(Optimality == proven),
		assertion(Costs == [1]).

	test(clingo_solve_syntax_diagnostics, deterministic, [condition(resolve_command_path(clingo, _))]) :-
		workspace_names(Before),
		^^file_path('test_files/malformed.lp', Path),
		catch(clingo::solve(file(Path), _), error(Formal, _), true),
		Formal = clingo_solver_error(_, _, Diagnostics, []),
		assertion(Diagnostics \== []),
		workspace_names(After),
		assertion(Before == After).

	test(clingo_solve_generated_utf8, deterministic(Codes == [99,97,102,233]), [condition(resolve_command_path(clingo, _))]) :-
		clingo::solve(terms([fact(function(p, [string([99,97,102,233])], positive))]), Result),
		findall(Model, clingo::model(Result, Model), Models),
		Models = [model([function(p, [string(Codes)], positive)], none, [_])].

	test(clingo_solve_invalid_option, error(domain_error(option, models(-1)))) :-
		clingo::solve(codes([]), _, [models(-1)]).

	test(clingo_solve_missing_executable, error(existence_error(command, logtalk_clingo_missing_executable))) :-
		clingo::solve(codes([]), _, [executable(logtalk_clingo_missing_executable)]).

	test(clingo_transport_failed_child_cleanup, deterministic) :-
		workspace_names(Before),
		transport_fixture(failure, Executable, Arguments, Path),
		clingo_process::run(Executable, Arguments, file(Path), _, Diagnostics, _, Status),
		assertion(Status \== exit(0)),
		assertion(Diagnostics \== []),
		workspace_names(After),
		assertion(Before == After).

	% auxiliary predicates

	assert_printed_result(Result, Expected) :-
		^^set_text_output(''),
		clingo::print_result(Result),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == Expected).

	check_team_result(Result) :-
		clingo::result_property(Result, optimality(Optimality)),
		clingo::result_property(Result, costs(Costs)),
		!,
		assertion(Optimality == proven),
		assertion(Costs == [4,-7,0]),
		findall(
			Sorted,
			(	clingo::model(Result, model(Symbols, [4,-7,0], _)),
				sort(Symbols, Sorted)
			),
			OptimalSolutions
		),
		sort([
			function(selected, [function(alice, [], positive)], positive),
			function(selected, [function(bob, [], positive)], positive),
			function(selected, [function(clara, [], positive)], negative),
			function(onsite, [], positive),
			function(summary, [number(4),number(4),number(2),number(2),number(3),number(1),number(1),number(-1),number(8),number(3),number(1),number(14)], positive),
			function(label, [string([84,101,97,109,32,34,65,34,92,66,10,82,101,97,100,121])], positive),
			function(limits, [infimum,supremum], positive),
			tuple([]),
			tuple([function(alice, [], positive)]),
			tuple([function(bob, [], positive)]),
			tuple([function(alice, [], positive), number(1)]),
			tuple([function(alice, [], positive), number(2)]),
			tuple([function(bob, [], positive), number(1)]),
			tuple([function(bob, [], positive), number(2)]),
			function(report, [function(team, [string([65,108,112,104,97])], positive), tuple([])], positive)
		], Expected),
		assertion(OptimalSolutions == [Expected]).

	workspace_names(Names) :-
		temporary_directory(Temporary),
		directory_files(Temporary, Files),
		findall(
			Name,
			(	member(Name, Files),
				sub_atom(Name, 0, _, _, 'logtalk-clingo')
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

	cli_fixture_result(Program, Result) :-
		resolve_command_path(clingo, Executable),
		clingo_process::run(Executable, ['--outf=2', '--quiet=0', '0'], codes(Program), Codes, Diagnostics, RawStatus, NormalizedStatus),
		clingo_output::decode(Codes, RawStatus, NormalizedStatus, Diagnostics, [], Result).

	fixture_codes(Name, Codes) :-
		atom_concat('test_files/', Name, Relative),
		^^file_path(Relative, Path),
		file_to_codes(Path, Codes).

:- end_object.
