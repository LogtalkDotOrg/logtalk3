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
		date is 2026-11-10,
		comment is 'Unit tests for the asp library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, member/2, msort/2
	]).

	:- uses(os, [
		directory_files/2, operating_system_type/1, resolve_command_path/2, temporary_directory/1
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	cover(asp_program).
	cover(asp_symbols).
	cover(asp_output).
	cover(asp_expansion).
	cover(asp).
	cover(asp_process).

	test(asp_program_empty, deterministic(Codes == [])) :-
		asp_program::serialize([], Codes).

	test(asp_program_fact, deterministic(Atom == 'p(1,-q).\n')) :-
		asp_program::serialize([fact(function(p, [number(1), function(q, [], negative)], positive))], Codes),
		atom_codes(Atom, Codes).

	test(asp_program_rule, deterministic(Atom == 'p(X) :- q(X), not r(X), X != 2.\n')) :-
		asp_program::serialize([
			rule(atom(function(p, [variable('X')], positive)), [
				function(q, [variable('X')], positive),
				not(function(r, [variable('X')], positive)),
				compare('!=', variable('X'), number(2))
			])
		], Codes),
		atom_codes(Atom, Codes).

	test(asp_program_body_conditional, deterministic(Atom == 'q :- p(X) : r(X); s.\n')) :-
		asp_program::serialize([
			rule(atom(function(q, [], positive)), [
				conditional(function(p, [variable('X')], positive), [function(r, [variable('X')], positive)]),
				function(s, [], positive)
			])
		], Codes),
		atom_codes(Atom, Codes).

	test(asp_program_empty_aggregate_tuple, deterministic(Atom == ':- #count{:} > 1.\n')) :-
		asp_program::serialize([
			constraint([compare('>', aggregate(count, [aggregate_element([], [])]), number(1))])
		], Codes),
		atom_codes(Atom, Codes).

	test(asp_program_empty_conditional, deterministic(Atom == 'q :- p : ; r.\n')) :-
		asp_program::serialize([
			rule(atom(function(q, [], positive)), [conditional(function(p, [], positive), []), function(r, [], positive)])
		], Codes),
		atom_codes(Atom, Codes).

	test(asp_program_choice, deterministic(Atom == '1 {p(X) : q(X); r} 2.\n')) :-
		asp_program::serialize([
			rule(choice(number(1), [
				conditional(function(p, [variable('X')], positive), [function(q, [variable('X')], positive)]),
				conditional(function(r, [], positive), [])
			], number(2)), [])
		], Codes),
		atom_codes(Atom, Codes).

	test(asp_program_aggregate, deterministic(Atom == ':- #sum{W,X : p(X,W)} > 3.\n')) :-
		asp_program::serialize([
			constraint([compare('>', aggregate(sum, [
				aggregate_element([variable('W'), variable('X')], [function(p, [variable('X'), variable('W')], positive)])
			]), number(3))])
		], Codes),
		atom_codes(Atom, Codes).

	test(asp_program_objectives, deterministic(Atom == '#minimize {-2@1,X : p(X)}.\n#maximize {}.\n:~ p. [1@0]\n')) :-
		asp_program::serialize([
			minimize([objective(number(-2), number(1), [variable('X')], [function(p, [variable('X')], positive)])]),
			maximize([]),
			weak_constraint([function(p, [], positive)], number(1), number(0), [])
		], Codes),
		atom_codes(Atom, Codes).

	test(asp_program_show_constant, deterministic(Atom == '#const n = (2**3).\n#show -p/1.\n#show.\n#show X : p(X).\n')) :-
		asp_program::serialize([
			constant(n, arithmetic('**', [number(2), number(3)])),
			show(signature(p, 1, negative)),
			show(none),
			show_term(variable('X'), [function(p, [variable('X')], positive)])
		], Codes),
		atom_codes(Atom, Codes).

	test(asp_program_tuple, deterministic(Atom == 'p((),(1,),(1,2),#inf,#sup,_).\n')) :-
		asp_program::serialize([fact(function(p, [
			tuple([]), tuple([number(1)]), tuple([number(1), number(2)]), infimum, supremum, anonymous
		], positive))], Codes),
		atom_codes(Atom, Codes).

	test(asp_program_string, deterministic(Codes == [112,40,34,97,92,110,92,34,92,92,34,41,46,10])) :-
		asp_program::serialize([fact(function(p, [string([97,10,34,92])], positive))], Codes).

	test(asp_program_source_map, deterministic) :-
		asp_program::serialize([
			raw([112,46,10,113,46]),
			fact(function(r, [], positive))
		], [source('fixture.lgt', 3, 4), unknown], Codes, Map),
		assertion(Codes == [112,46,10,113,46,10,114,46,10]),
		assertion(Map == [line(1, 2, source('fixture.lgt', 3, 4)), line(3, 3, unknown)]).

	test(asp_program_unbound, error(instantiation_error)) :-
		asp_program::serialize(_, _).

	test(asp_program_non_ground, error(instantiation_error)) :-
		asp_program::serialize([fact(function(p, [_], positive))], _).

	test(asp_program_non_list, error(type_error(list, invalid))) :-
		asp_program::serialize(invalid, _).

	test(asp_program_invalid_statement, error(domain_error(asp_statement, call(halt)))) :-
		asp_program::serialize([call(halt)], _).

	test(asp_program_invalid_identifier, error(domain_error(asp_statement, fact(function('P', [], positive))))) :-
		asp_program::serialize([fact(function('P', [], positive))], _).

	test(asp_program_variable_constant, error(domain_error(asp_statement, constant(n, variable('X'))))) :-
		asp_program::serialize([constant(n, variable('X'))], _).

	test(asp_program_invalid_origins, error(domain_error(asp_origins, []))) :-
		asp_program::serialize([fact(function(p, [], positive))], [], _, _).

	test(asp_symbols_nested, deterministic(Symbol == function(p, [number(-12), function(q, [], negative), tuple([number(1)]), infimum, supremum], positive))) :-
		atom_codes('p(-12,-q,(1,),#inf,#sup)', Codes),
		asp_symbols::parse(Codes, Symbol).

	test(asp_symbols_empty_tuple, deterministic(Symbol == tuple([]))) :-
		asp_symbols::parse([0'(, 0')], Symbol).

	test(asp_symbols_tuple, deterministic(Symbol == tuple([number(1), function(a, [], positive)]))) :-
		atom_codes('(1,a)', Codes),
		asp_symbols::parse(Codes, Symbol).

	test(asp_symbols_string, deterministic(Symbol == string([97,10,34,92]))) :-
		asp_symbols::parse([34,97,92,110,92,34,92,92,34], Symbol).

	test(asp_symbols_trailing_input, error(domain_error(asp_symbol, [97,32,98]))) :-
		asp_symbols::parse([97,32,98], _).

	test(asp_symbols_unclosed_string, error(domain_error(asp_symbol, [34,97]))) :-
		asp_symbols::parse([34,97], _).

	test(asp_symbols_not_prolog, error(domain_error(asp_symbol, [39,97,39]))) :-
		asp_symbols::parse([39,97,39], _).

	test(asp_symbols_parenthesized_number, error(domain_error(asp_symbol, [40,49,41]))) :-
		asp_symbols::parse([40,49,41], _).

	test(asp_symbols_invalid_identifier, error(domain_error(asp_symbol, [95]))) :-
		asp_symbols::parse([95], _).

	test(asp_symbols_reserved_identifier, error(domain_error(asp_symbol, [110,111,116]))) :-
		asp_symbols::parse([110,111,116], _).

	test(asp_output_optional_witnesses, deterministic) :-
		atom_codes('{"Result":"SATISFIABLE"}', Codes),
		asp_output::decode(clingo, Codes, exit(10), exit(10), [], [], asp_result(Status, Models, Properties), []),
		assertion(Status == satisfiable),
		assertion(Models == []),
		assertion(member(enumeration_complete(unknown), Properties)).

	test(asp_output_sat, deterministic) :-
		fixture_codes('sat.json', Codes),
		asp_output::decode(clingo, Codes, exit(30), exit(30), [], [], asp_result(Status, Models, Properties), []),
		assertion(Status == satisfiable),
		assertion(Models == [model([], none, []), model([function(p, [], positive)], none, [p])]),
		assertion(member(enumeration_complete(true), Properties)).

	test(asp_output_unsat, deterministic) :-
		fixture_codes('unsat.json', Codes),
		asp_output::decode(clingo, Codes, 5120, exit(20), [], [], asp_result(Status, Models, _), []),
		assertion(Status == unsatisfiable),
		assertion(Models == []).

	test(asp_output_optimum, deterministic) :-
		fixture_codes('optimum.json', Codes),
		asp_output::decode(clingo, Codes, exit(30), exit(30), [119], [], asp_result(Status, Models, Properties), []),
		assertion(Status == satisfiable),
		assertion(Models == [
			model([function(p, [number(2)], positive)], [2], ['p(2)']),
			model([function(p, [number(1)], positive)], [1], ['p(1)'])
		]),
		assertion(member(optimality(proven), Properties)),
		assertion(member(costs([1]), Properties)),
		assertion(member(diagnostics([119]), Properties)).

	test(asp_output_time_limit, deterministic) :-
		fixture_codes('time_limit.json', Codes),
		asp_output::decode(clingo, Codes, exit(1), exit(1), [], [], asp_result(Status, Models, Properties), []),
		assertion(Status == unknown),
		assertion(Models == []),
		assertion(member(termination(time_limit), Properties)),
		assertion(member(enumeration_complete(false), Properties)).

	test(asp_output_fatal, error(domain_error(asp_process_status, exit(65)))) :-
		fixture_codes('sat.json', Codes),
		asp_output::decode(clingo, Codes, exit(65), exit(65), [], [], _, []).

	test(asp_output_inconsistent_status, error(domain_error(asp_process_status, exit(20)))) :-
		fixture_codes('sat.json', Codes),
		asp_output::decode(clingo, Codes, exit(20), exit(20), [], [], _, []).

	test(asp_output_invalid_value, error(domain_error(asp_output, 'Value'-42))) :-
		fixture_codes('invalid_value.json', Codes),
		asp_output::decode(clingo, Codes, exit(10), exit(10), [], [], _, []).

	test(asp_output_truncated, error(domain_error(json, _))) :-
		fixture_codes('truncated.json', Codes),
		asp_output::decode(clingo, Codes, exit(10), exit(10), [], [], _, []).

	test(asp_expansion_reachability, deterministic) :-
		findall(Statement, asp_reachability::asp_statement(Statement, _), Statements),
		asp_program::serialize(Statements, Codes),
		atom_codes(Program, Codes),
		assertion(Program == 'edge(a,b).\nedge(b,c).\nreach(Start,End) :- edge(Start,End).\nreach(Start,End) :- edge(Start,Middle), reach(Middle,End).\n#show reach/2.\n').

	test(asp_expansion_variables, deterministic) :-
		asp_variable_fixture::asp_statement(fact(function(p, Arguments, positive)), _),
		assertion(Arguments == [variable('Value'), variable('Value'), anonymous, anonymous, variable('_Named'), variable('_Named')]).

	test(asp_expansion_unconditional_heads, deterministic) :-
		findall(Head, asp_team_selection::asp_statement(rule(Head, []), _), Heads),
		assertion(Heads == [
			choice(function(size, [], positive), [
				conditional(function(selected, [variable('Person')], positive), [function(person, [variable('Person')], positive)])
			], function(size, [], positive)),
			choice(none, [
				conditional(function(reserve, [variable('Person')], positive), [function(person, [variable('Person')], positive)])
			], none),
			disjunction([function(onsite, [], positive), function(remote, [], positive)])
		]).

	test(asp_expansion_raw_text, deterministic) :-
		findall(Statement, asp_raw_fixture::asp_statement(Statement, _), Statements),
		assertion(Statements == [
			raw('#program base.'),
			raw([35,112,114,111,103,114,97,109,32,98,97,115,101,46]),
			raw(['#',p,r,o,g,r,a,m,' ',b,a,s,e,'.']),
			raw("#program base.")
		]),
		asp::program(object(asp_raw_fixture), ProgramCodes),
		atom_codes(Program, ProgramCodes),
		assertion(Program == '#program base.\n#program base.\n#program base.\n#program base.\n').

	test(asp_expansion_body_is_data) :-
		asp_variable_fixture::asp_statement(rule(atom(function(rule_as_data, [], positive)), Body), _),
		assertion(Body == [not(function(halt, [], positive))]),
		assertion(\+ asp_variable_fixture::current_predicate(rule_as_data/0)).

	test(asp_expansion_unrelated) :-
		asp_unrelated_fixture::value(Value),
		assertion(Value == ordinary),
		assertion(\+ asp_unrelated_fixture::current_predicate(asp_statement/2)).

	test(asp_program_object_source, deterministic(Codes == Expected)) :-
		asp::program(object(asp_reachability), Codes),
		findall(Statement, asp_reachability::asp_statement(Statement, _), Statements),
		asp::program(terms(Statements), Expected).

	test(asp_program_codes_source, deterministic(Codes == [112,46,10])) :-
		asp::program(codes([112,46,10]), Codes).

	test(asp_program_wrong_object, error(domain_error(asp_source, object(asp_unrelated_fixture)))) :-
		asp::program(object(asp_unrelated_fixture), _).

	test(asp_program_print_codes, deterministic) :-
		atom_codes('p.\n\tq("a b").', Codes),
		^^set_text_output(''),
		asp::program(codes(Codes)),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p.\n\tq("a b").').

	test(asp_program_print_terms, deterministic) :-
		^^set_text_output(''),
		asp::program(terms([fact(function(p, [], positive))])),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p.\n').

	test(asp_program_print_object, deterministic) :-
		asp::program(object(asp_queens), Codes),
		atom_codes(Expected, Codes),
		^^set_text_output(''),
		asp::program(object(asp_queens)),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == Expected).

	test(asp_program_print_invalid, error(domain_error(asp_source, invalid))) :-
		asp::program(invalid).

	test(asp_program_print_unbound, error(instantiation_error)) :-
		asp::program(_).

	test(asp_result_models) :-
		fixture_codes('sat.json', Codes),
		asp_output::decode(clingo, Codes, exit(30), exit(30), [], [], Result, []),
		findall(Model, asp::model(Result, Model), Models),
		assertion(Models == [model([], none, []), model([function(p, [], positive)], none, [p])]).

	test(asp_result_property) :-
		fixture_codes('sat.json', Codes),
		asp_output::decode(clingo, Codes, exit(30), exit(30), [], [], Result, []),
		asp::result_property(Result, status(Status)),
		assertion(Status == satisfiable),
		asp::result_property(Result, model_count(Count)),
		assertion(Count == 2).

	test(asp_result_invalid, error(domain_error(asp_result, invalid))) :-
		asp::model(invalid, _).

	test(asp_print_program_empty, deterministic) :-
		^^set_text_output(''),
		asp::print_program([]),
		^^text_output_contents(Chars),
		assertion(Chars == []).

	test(asp_print_program_layout, deterministic) :-
		atom_codes('p.\n\tq("a b").', Codes),
		^^set_text_output(''),
		asp::print_program(Codes),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p.\n\tq("a b").').

	test(asp_print_program_generated, deterministic) :-
		asp::program(terms([fact(function(p, [], positive))]), Codes),
		^^set_text_output(''),
		asp::print_program(Codes),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p.\n').

	test(asp_print_program_invalid, error(type_error(integer, p))) :-
		asp::print_program([p]).

	test(asp_print_program_invalid_code, error(domain_error(character_code, -1))) :-
		asp::print_program([112, -1]).

	test(asp_print_program_unbound, error(instantiation_error)) :-
		asp::print_program(_).

	test(asp_print_program_partial, error(instantiation_error)) :-
		asp::print_program([112| _]).

	test(asp_print_result_answers, deterministic) :-
		Result = asp_result(satisfiable, [model([], none, []), model([function(p, [], positive)], none, [p])], [
			solver(clingo), solver_version('clingo version 5.8.3'), model_count(2), enumeration_complete(true)
		]),
		assert_printed_result(Result, 'clingo version 5.8.3\nAnswer: 1\n\nAnswer: 2\np\nSATISFIABLE\n\nModels       : 2\n').

	test(asp_print_result_optimum, deterministic) :-
		Result = asp_result(satisfiable, [
			model([], [2,-3], ['p(2)']),
			model([], [1,-4], ['p(1)'])
		], [model_count(2), enumeration_complete(true), optimality(proven), costs([1,-4])]),
		assert_printed_result(Result, 'Answer: 1\np(2)\nOptimization: 2 -3\nAnswer: 2\np(1)\nOptimization: 1 -4\nOPTIMUM FOUND\n\nModels       : 2\n  Optimum    : yes\nOptimization : 1 -4\n').

	test(asp_print_result_unproven, deterministic) :-
		Result = asp_result(satisfiable, [model([], [2], [p])], [
			model_count(1), enumeration_complete(false), optimality(unproven), costs([2])
		]),
		assert_printed_result(Result, 'Answer: 1\np\nOptimization: 2\nSATISFIABLE\n\nModels       : 1+\n  Optimum    : unknown\nOptimization : 2\n').

	test(asp_print_result_unsatisfiable, deterministic) :-
		Result = asp_result(unsatisfiable, [], [model_count(0), enumeration_complete(true)]),
		assert_printed_result(Result, 'UNSATISFIABLE\n\nModels       : 0\n').

	test(asp_print_result_time_limit, deterministic) :-
		Result = asp_result(unknown, [], [termination(time_limit), model_count(0), enumeration_complete(false)]),
		assert_printed_result(Result, 'UNKNOWN\n\nTIME LIMIT   : 1\nModels       : 0+\n').

	test(asp_print_result_interrupted, deterministic) :-
		Result = asp_result(unknown, [], [termination(interrupted), model_count(unknown), enumeration_complete(unknown)]),
		assert_printed_result(Result, 'UNKNOWN\n\nINTERRUPTED  : 1\nModels       : unknown?\n').

	test(asp_print_result_raw_values, deterministic) :-
		Result = asp_result(satisfiable, [model([], none, ['p("a b\\n")', '-q'])], [model_count(1), enumeration_complete(true)]),
		assert_printed_result(Result, 'Answer: 1\np("a b\\n") -q\nSATISFIABLE\n\nModels       : 1\n').

	test(asp_print_result_time, deterministic) :-
		Result = asp_result(unsatisfiable, [], [model_count(0), enumeration_complete(true), time(json(['Total'-2, 'CPU'-1]))]),
		assert_printed_result(Result, 'UNSATISFIABLE\n\nModels       : 0\nTime         : 2s\nCPU Time     : 1s\n').

	test(asp_print_result_invalid, error(domain_error(asp_result, invalid))) :-
		asp::print_result(invalid).

	test(asp_print_result_unbound, error(instantiation_error)) :-
		asp::print_result(_).

	test(asp_cli_reachability, true, [condition((current_object(process), resolve_command_path(clingo, _)))]) :-
		asp::program(object(asp_reachability), Program),
		cli_fixture_result(Program, Result),
		findall(Model, asp::model(Result, Model), Models),
		Models = [model(Symbols, none, RawValues)],
		assertion(member(function(reach, [function(a, [], positive), function(c, [], positive)], positive), Symbols)),
		assertion(member('reach(a,c)', RawValues)),
		asp::result_property(Result, enumeration_complete(Complete)),
		assertion(Complete == true),
		asp::result_property(Result, diagnostics(Diagnostics)),
		assertion(Diagnostics == []).

	test(asp_cli_empty_conditional, true, [condition((current_object(process), resolve_command_path(clingo, _)))]) :-
		asp::program(terms([
			fact(function(p, [], positive)),
			fact(function(r, [], positive)),
			rule(atom(function(q, [], positive)), [conditional(function(p, [], positive), []), function(r, [], positive)]),
			show(signature(q, 0, positive))
		]), Program),
		cli_fixture_result(Program, Result),
		findall(Model, asp::model(Result, Model), Models),
		assertion(Models == [model([function(q, [], positive)], none, [q])]).

	test(asp_shell_posix_literal, deterministic(Codes == [39,97,39,34,39,34,39,98,59,36,40,41,39])) :-
		atom_codes(Argument, [97,39,98,59,36,40,41]),
		asp_process::quote(posix, Argument, Quoted),
		atom_codes(Quoted, Codes).

	test(asp_solve_reachability, deterministic(Status == satisfiable), [condition(resolve_command_path(clingo, _))]) :-
		asp::solve(object(asp_reachability), asp_result(Status, [_], Properties)),
		assertion(member(source_map([_| _]), Properties)).

	test(asp_solve_all_models, true, [condition(resolve_command_path(clingo, _))]) :-
		atom_codes('{p}.', Program),
		asp::solve(codes(Program), Result, [models(0), models(1)]),
		findall(Model, asp::model(Result, Model), Models),
		length(Models, Count),
		assertion(Count == 2),
		asp::result_property(Result, enumeration_complete(Complete)),
		assertion(Complete == true).

	test(asp_solve_constants, true, [condition(resolve_command_path(clingo, _))]) :-
		atom_codes('#const n=1. p(n).', Program),
		asp::solve(codes(Program), Result, [constants([n-number(3)])]),
		findall(Model, asp::model(Result, Model), Models),
		assertion(Models == [model([function(p, [number(3)], positive)], none, ['p(3)'])]).

	test(asp_shell_windows_literal, deterministic(Codes == [39,97,39,39,98,59,36,40,41,39])) :-
		atom_codes(Argument, [97,39,98,59,36,40,41]),
		asp_process::quote(windows, Argument, Quoted),
		atom_codes(Quoted, Codes).

	test(asp_transport_large_captures) :-
		workspace_names(Before),
		transport_fixture(capture, Executable, Arguments, Path),
		asp_process::run(Executable, Arguments, file(Path), Output, Diagnostics, _, Status),
		length(Output, OutputLength),
		length(Diagnostics, ErrorLength),
		assertion(OutputLength == 131072),
		assertion(ErrorLength == 131072),
		assertion(Status == exit(0)),
		workspace_names(After),
		assertion(Before == After).

	test(asp_solve_native_include_utf8, true, [condition(resolve_command_path(clingo, _))]) :-
		^^file_path('test_files/shell data ''$();/main.lp', Path),
		file_to_codes(Path, Before),
		asp::solve(file(Path), Result),
		findall(Model, asp::model(Result, Model), Models),
		atom_codes(RawValue, [112,40,34,99,97,102,233,34,41]),
		assertion(Models == [model([function(p, [string([99,97,102,233])], positive)], none, [RawValue])]),
		file_to_codes(Path, After),
		assertion(Before == After).

	test(asp_solve_four_queens, true, [condition(resolve_command_path(clingo, _))]) :-
		asp::solve(object(asp_queens), Result, [models(0), constants([n-number(4)])]),
		Result = asp_result(Status, _, _),
		assertion(Status == satisfiable),
		findall(
			Sorted,
			(	asp::model(Result, model(Symbols, none, _)),
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
		asp::result_property(Result, enumeration_complete(Complete)),
		assertion(Complete == true).

	test(asp_solve_minimum_vertex_cover, true, [condition(resolve_command_path(clingo, _))]) :-
		asp::solve(object(asp_vertex_cover), Result, [models(0)]),
		asp::result_property(Result, optimality(Optimality)),
		asp::result_property(Result, costs(Costs)),
		assertion(Optimality == proven),
		assertion(Costs == [1]),
		findall(Symbols, asp::model(Result, model(Symbols, [1], _)), OptimalSolutions),
		assertion(OptimalSolutions == [[function(cover, [function(b, [], positive)], positive)]]).

	test(asp_solve_team_selection_object, deterministic, [condition(resolve_command_path(clingo, _))]) :-
		asp::solve(object(asp_team_selection), Result, [models(0)]),
		check_team_result(Result).

	test(asp_solve_unsatisfiable, deterministic, [condition(resolve_command_path(clingo, _))]) :-
		atom_codes(':- .', Program),
		asp::solve(codes(Program), asp_result(Status, Models, _)),
		assertion(Status == unsatisfiable),
		assertion(Models == []).

	test(asp_solve_optimization, true, [condition(resolve_command_path(clingo, _))]) :-
		atom_codes('1 {p(1);p(2)} 1. #minimize {X:p(X)}.', Program),
		asp::solve(codes(Program), Result, [models(0), statistics(1)]),
		asp::result_property(Result, optimality(Optimality)),
		asp::result_property(Result, costs(Costs)),
		assertion(Optimality == proven),
		assertion(Costs == [1]).

	test(asp_solve_syntax_diagnostics, deterministic, [condition(resolve_command_path(clingo, _))]) :-
		workspace_names(Before),
		^^file_path('test_files/malformed.lp', Path),
		catch(asp::solve(file(Path), _), error(Formal, _), true),
		Formal = asp_solver_error(_, _, Diagnostics, []),
		assertion(Diagnostics \== []),
		workspace_names(After),
		assertion(Before == After).

	test(asp_solve_generated_utf8, deterministic(Codes == [99,97,102,233]), [condition(resolve_command_path(clingo, _))]) :-
		asp::solve(terms([fact(function(p, [string([99,97,102,233])], positive))]), Result),
		findall(Model, asp::model(Result, Model), Models),
		Models = [model([function(p, [string(Codes)], positive)], none, [_])].

	test(asp_solve_invalid_option, error(domain_error(option, models(-1)))) :-
		asp::solve(codes([]), _, [models(-1)]).

	test(asp_solve_missing_executable, error(existence_error(command, logtalk_clingo_missing_executable))) :-
		asp::solve(codes([]), _, [executable(logtalk_clingo_missing_executable)]).

	test(asp_transport_failed_child_cleanup, deterministic) :-
		workspace_names(Before),
		transport_fixture(failure, Executable, Arguments, Path),
		asp_process::run(Executable, Arguments, file(Path), _, Diagnostics, _, Status),
		assertion(Status \== exit(0)),
		assertion(Diagnostics \== []),
		workspace_names(After),
		assertion(Before == After).

	test(asp_default_solver, deterministic(Solver == clingo)) :-
		asp::default_option(solver(Solver)).

	test(asp_solver_zetesis_option, deterministic) :-
		assertion(asp::valid_option(solver(zetesis))),
		assertion(asp::valid_option(backend(cpu))),
		assertion(asp::valid_option(grounder(lazy))),
		assertion(asp::valid_option(threads(2))),
		assertion(asp::valid_option(memory(1048576))),
		assertion(\+ asp::valid_option(threads(0))),
		assertion(\+ asp::valid_option(memory(0))).

	test(asp_zetesis_reject_constants, error(domain_error(asp_solver_option, zetesis-constants([n-number(4)])))) :-
		asp::solve(codes([]), _, [solver(zetesis), constants([n-number(4)])]).

	test(asp_zetesis_reject_ignore, error(domain_error(asp_solver_option, zetesis-opt_mode(ignore)))) :-
		asp::solve(codes([]), _, [solver(zetesis), opt_mode(ignore)]).

	test(asp_clingo_reject_grounder, error(domain_error(asp_solver_option, clingo-grounder(lazy)))) :-
		asp::solve(codes([]), _, [grounder(lazy)]).

	test(asp_symbol_codes_roundtrip, deterministic) :-
		Symbol = function(p, [string([97,34,92,10]), tuple([number(-2)]), infimum, supremum], negative),
		asp_program::symbol_codes(Symbol, Codes),
		asp_symbols::parse(Codes, Parsed),
		assertion(Parsed == Symbol).

	test(asp_symbol_codes_variable, error(domain_error(asp_symbol, variable('X')))) :-
		asp_program::symbol_codes(variable('X'), _).

	test(asp_zetesis_output_empty, deterministic) :-
		zetesis_fixture(empty, 0, asp_result(Status, Models, _)),
		assertion(Status == satisfiable),
		assertion(Models == [model([], none, [])]).

	test(asp_zetesis_print_empty, deterministic) :-
		zetesis_fixture(empty, 0, Result),
		assert_printed_result(Result, 'zetesis\nAnswer: 1\n\nSATISFIABLE\n\nModels       : 1\n').

	test(asp_zetesis_output_unsat, deterministic) :-
		zetesis_fixture(unsat, 0, asp_result(Status, Models, _)),
		assertion(Status == unsatisfiable),
		assertion(Models == []).

	test(asp_zetesis_output_hidden, deterministic) :-
		zetesis_fixture(hidden, 0, asp_result(Status, Models, Properties)),
		assertion(Status == satisfiable),
		assertion(Models == [model([], none, []), model([], none, [])]),
		assertion(member(enumeration_complete(true), Properties)).

	test(asp_zetesis_output_limited, deterministic) :-
		zetesis_fixture(limited, 0, asp_result(Status, [_], Properties)),
		assertion(Status == satisfiable),
		assertion(member(enumeration_complete(false), Properties)),
		assertion(member(search_complete(false), Properties)).

	test(asp_zetesis_output_optimum, deterministic) :-
		zetesis_fixture(optimum, 0, asp_result(Status, Models, Properties)),
		assertion(Status == satisfiable),
		assertion(Models == [model([function(p, [number(1)], positive)], [1], ['p(1)'])]),
		assertion(member(optimality(proven), Properties)),
		assertion(member(cost_priorities([0]), Properties)),
		assertion(member(verified_model_count(2), Properties)),
		assertion(member(model_count(1), Properties)).

	test(asp_zetesis_output_terms, deterministic) :-
		zetesis_fixture(terms, 0, asp_result(_, Models, _)),
		Models = [model(Symbols, none, Raw)],
		assertion(Symbols == [function(p, [string([97,34,92,10]), tuple([number(1)]), function(team, [number(-2)], positive), infimum, supremum], positive), tuple([function(x, [], positive), number(3)])]),
		assertion(Raw == ['p("a\\"\\\\\\n",(1,),team(-2),#inf,#sup)', '(x,3)']).

	test(asp_zetesis_output_signed, deterministic) :-
		zetesis_fixture(signed, 0, asp_result(_, Models, _)),
		assertion(Models == [model([function(p, [function(a, [], negative), function(f, [tuple([function(a, [], positive)]), tuple([])], negative)], positive)], none, ['p(-a,-f((a,),()))'])]).

	test(asp_zetesis_output_interrupted, deterministic) :-
		zetesis_fixture(interrupted, 3, asp_result(Status, Models, Properties)),
		assertion(Status == unknown),
		assertion(Models == []),
		assertion(member(termination(time_limit), Properties)),
		assertion(member(enumeration_complete(false), Properties)).

	test(asp_zetesis_output_invalid, error(domain_error(asp_output, error-_))) :-
		zetesis_fixture(invalid, 2, _).

	test(asp_zetesis_output_exit_mismatch, error(domain_error(asp_process_status, exit(3)))) :-
		zetesis_fixture(empty, 3, _).

	test(asp_zetesis_output_bad_index, error(domain_error(asp_output, atom_index-7))) :-
		zetesis_fixture(bad_index, 0, _).

	test(asp_zetesis_output_bad_schema, error(domain_error(asp_output, schema-2))) :-
		zetesis_fixture(bad_schema, 0, _).

	test(asp_zetesis_output_bad_count, error(domain_error(asp_output, published_models-1))) :-
		zetesis_fixture(bad_count, 0, _).

	test(asp_zetesis_output_bad_term, error(domain_error(asp_output, term-_))) :-
		zetesis_fixture(bad_term, 0, _).

	test(asp_zetesis_output_bad_cost, error(domain_error(asp_output, priority-1))) :-
		zetesis_fixture(bad_cost, 0, _).

	test(asp_zetesis_solve_empty, deterministic, [condition(resolve_command_path(zetesis, _))]) :-
		asp::solve(codes([]), Result, [solver(zetesis), time_limit(0)]),
		Result = asp_result(Status, Models, Properties),
		assertion(Status == satisfiable),
		assertion(Models == [model([], none, [])]),
		assertion(member(solver(zetesis), Properties)).

	test(asp_zetesis_first_solver_option, deterministic, [condition(resolve_command_path(zetesis, _))]) :-
		asp::solve(codes([]), Result, [solver(zetesis), solver(clingo)]),
		Result = asp_result(_, _, Properties),
		assertion(member(solver(zetesis), Properties)).

	test(asp_zetesis_executable_override, deterministic, [condition(resolve_command_path(zetesis, _))]) :-
		resolve_command_path(zetesis, Executable),
		asp::solve(codes([]), Result, [solver(zetesis), executable(Executable), threads(1), grounder(eager), memory(1073741824), statistics(1), time_limit(2)]),
		Result = asp_result(Status, _, _),
		assertion(Status == satisfiable).

	test(asp_zetesis_solve_reachability, deterministic, [condition((resolve_command_path(clingo, _), resolve_command_path(zetesis, _)))]) :-
		asp::solve(object(asp_reachability), Clingo, [models(0)]),
		asp::solve(object(asp_reachability), Zetesis, [solver(zetesis), models(0)]),
		normalized_models(Clingo, Expected),
		normalized_models(Zetesis, Actual),
		assertion(Actual == Expected).

	test(asp_zetesis_solve_four_queens, deterministic, [condition((resolve_command_path(clingo, _), resolve_command_path(zetesis, _)))]) :-
		findall(Statement, asp_queens::asp_statement(Statement, _), [constant(n, _)| Rules]),
		Source = terms([constant(n, number(4))| Rules]),
		asp::solve(Source, Clingo, [models(0)]),
		asp::solve(Source, Zetesis, [solver(zetesis), models(0)]),
		normalized_models(Clingo, Expected),
		normalized_models(Zetesis, Actual),
		assertion(Actual == Expected),
		assertion(length(Actual, 2)).

	test(asp_zetesis_solve_vertex_cover, deterministic, [condition(resolve_command_path(zetesis, _))]) :-
		asp::solve(object(asp_vertex_cover), Result, [solver(zetesis), models(0)]),
		Result = asp_result(_, _, Properties),
		assertion(member(optimality(proven), Properties)),
		normalized_models(Result, Models),
		assertion(Models == [[1]-[function(cover, [function(b, [], positive)], positive)]]).

	test(asp_zetesis_solve_team_selection, deterministic, [condition(resolve_command_path(zetesis, _))]) :-
		asp::solve(object(asp_team_selection), Result, [solver(zetesis), models(0)]),
		check_team_result(Result).

	test(asp_zetesis_unknown_tie_count, deterministic, [condition(resolve_command_path(zetesis, _))]) :-
		atom_codes('a. {b;c}. #show a/0. #minimize{0@2,k:a;0@-1,j:b}.', Codes),
		asp::solve(codes(Codes), Result, [solver(zetesis)]),
		Result = asp_result(_, _, Properties),
		assertion(member(optimality(proven), Properties)),
		assertion(member(optimal_tie_count(unknown), Properties)),
		assertion(member(enumeration_complete(unknown), Properties)).

	test(asp_zetesis_hidden_optimum_ties, deterministic, [condition(resolve_command_path(zetesis, _))]) :-
		atom_codes('a. {b;c}. #show a/0. #minimize{0@2,k:a;0@-1,j:b}.', Codes),
		asp::solve(codes(Codes), Result, [solver(zetesis), models(0)]),
		normalized_models(Result, Models),
		length(Models, Count),
		assertion(Count == 4),
		Result = asp_result(_, _, Properties),
		assertion(member(enumeration_complete(true), Properties)).

	test(asp_zetesis_native_include_utf8, deterministic, [condition(resolve_command_path(zetesis, _))]) :-
		^^file_path('test_files/shell data ''$();/main.lp', Path),
		asp::solve(file(Path), Result, [solver(zetesis)]),
		normalized_models(Result, Models),
		assertion(Models == [none-[function(p, [string([99,97,102,233])], positive)]]).

	test(asp_zetesis_failure_cleanup, deterministic, [condition(resolve_command_path(zetesis, _))]) :-
		workspace_names(Before),
		^^file_path('test_files/malformed.lp', Path),
		catch(asp::solve(file(Path), _, [solver(zetesis)]), error(Formal, _), true),
		Formal = asp_solver_error(_, Exit, Diagnostics, []),
		assertion(Exit == exit(2)),
		assertion(Diagnostics \== []),
		workspace_names(After),
		assertion(After == Before).

	% auxiliary predicates

	zetesis_fixture(Name, Exit, Result) :-
		atom_concat('zetesis_', Name, Prefix),
		atom_concat(Prefix, '.json', File),
		fixture_codes(File, Codes),
		asp_output::decode(zetesis, Codes, exit(Exit), exit(Exit), [], [], Result, [models(0)]).

	normalized_models(Result, Models) :-
		findall(
			Costs-Sorted,
			(	asp::model(Result, model(Symbols, Costs, _)),
				sort(Symbols, Sorted)
			),
			Unsorted
		),
		msort(Unsorted, Models).

	assert_printed_result(Result, Expected) :-
		^^set_text_output(''),
		asp::print_result(Result),
		^^text_output_contents(Chars),
		list::subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == Expected).

	check_team_result(Result) :-
		asp::result_property(Result, optimality(Optimality)),
		asp::result_property(Result, costs(Costs)),
		!,
		assertion(Optimality == proven),
		assertion(Costs == [4,-7,0]),
		findall(
			Sorted,
			(	asp::model(Result, model(Symbols, [4,-7,0], _)),
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
				sub_atom(Name, 0, _, _, 'logtalk-asp')
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
		asp_process::run(Executable, ['--outf=2', '--quiet=0', '0'], codes(Program), Codes, Diagnostics, RawStatus, NormalizedStatus),
		asp_output::decode(clingo, Codes, RawStatus, NormalizedStatus, Diagnostics, [], Result, []).

	fixture_codes(Name, Codes) :-
		atom_concat('test_files/', Name, Relative),
		^^file_path(Relative, Path),
		file_to_codes(Path, Codes).

:- end_object.
