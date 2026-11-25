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
		date is 2026-11-24,
		comment is 'Unit tests for the "wmc" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		memberchk/2, length/2, subtract/3, append/3
	]).

	:- uses(numberlist, [
		sum/2
	]).

	cover(wmc).
	cover(wmc_process).
	cover(wmc_program).
	cover(wmc_output).
	cover(wmc_reference).

	% wmc_program tests

	test(wmc_program_empty, deterministic(Atom == 'p cnf 0 0\n')) :-
		wmc_program::serialize([], Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_unweighted_clauses, deterministic(Atom == 'p cnf 3 2\n1 -2 0\n3 0\n')) :-
		wmc_program::serialize([(+p ; -q), r], Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_bare_literals, deterministic(Atom == 'p cnf 2 1\n1 2 0\n')) :-
		wmc_program::serialize([(p ; q)], Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_weights, deterministic(Atom == 'p cnf 2 1\nc p weight 1 0.25 0\nc p weight -1 1 0\nc p weight 2 1 0\nc p weight -2 2 0\n1 2 0\n')) :-
		wmc_program::serialize([(p ; q), weight(+p, 0.25), weight(-q, 2)], Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_weight_bare_literal_is_positive, deterministic(Atom == 'p cnf 1 0\nc p weight 1 3 0\nc p weight -1 1 0\n')) :-
		wmc_program::serialize([weight(p, 3)], Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_probability, deterministic(Atom == 'p cnf 1 0\nc p weight 1 0.25 0\nc p weight -1 0.75 0\n')) :-
		wmc_program::serialize([probability(p, 0.25)], Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_probability_signed_term, deterministic) :-
		wmc::solve(terms([probability(-a, 0.25), +(-a)]), Result),
		Result = wmc_result(counted, Value, Properties),
		assertion(close(Value, 0.25)),
		assertion(memberchk(variables([-a-1]), Properties)),
		wmc::solve(terms([probability(-a, 0.25)]), FreeResult),
		wmc::value(FreeResult, FreeValue),
		assertion(close(FreeValue, 1.0)).

	test(wmc_program_probability_integer_bounds, deterministic(Atom == 'p cnf 2 0\nc p weight 1 1 0\nc p weight -1 0 0\nc p weight 2 0 0\nc p weight -2 1 0\n')) :-
		wmc_program::serialize([probability(p, 1), probability(q, 0)], Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_variables_only_in_weights, deterministic(Atom == 'p cnf 2 1\nc p weight 1 1 0\nc p weight -1 1 0\nc p weight 2 0.5 0\nc p weight -2 0.5 0\n1 0\n')) :-
		wmc_program::serialize([p, probability(q, 0.5)], Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_empty_clause, deterministic(Atom == 'p cnf 0 1\n0\n')) :-
		wmc_program::serialize([false], Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_comment_and_raw_not_counted, deterministic(Atom == 'p cnf 1 1\nc hi\nraw line\n1 0\n')) :-
		wmc_program::serialize([comment(hi), raw('raw line'), p], Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_variables, deterministic(Variables == [p-1, q-2, r-3])) :-
		wmc_program::variables([(+p ; -q), weight(r, 2), p], Variables).

	test(wmc_program_variables_structured_terms, deterministic(Variables == [queen(1,2)-1, in(a)-2])) :-
		wmc_program::variables([(queen(1,2) ; -in(a))], Variables).

	test(wmc_program_weighted, deterministic) :-
		wmc_program::weighted([p, weight(q, 2)]),
		wmc_program::weighted([probability(q, 0.5)]),
		\+ wmc_program::weighted([p, (q ; -r)]).

	test(wmc_program_source_map, deterministic(Map == [line(8, 8, unknown), line(9, 9, unknown)])) :-
		% header (1), six weight lines, then the clauses; probability statements generate no lines
		wmc_program::serialize([(p ; q), probability(r, 0.5), q], [unknown, unknown, unknown], _, Map).

	test(wmc_program_source_map_unweighted, deterministic(Map == [line(2, 2, unknown), line(3, 3, source(f, 1, 5))])) :-
		wmc_program::serialize([p, q], [unknown, source(f, 1, 5)], _, Map).

	test(wmc_program_invalid_origins, error(domain_error(wmc_origins, [unknown]))) :-
		wmc_program::serialize([p, q], [unknown], _, _).

	test(wmc_program_duplicate_weight, error(domain_error(wmc_duplicate_weight, +p))) :-
		wmc_program::serialize([weight(+p, 1), weight(+p, 2)], _).

	test(wmc_program_duplicate_weight_bare_and_positive, error(domain_error(wmc_duplicate_weight, +p))) :-
		wmc_program::serialize([weight(p, 1), weight(+p, 2)], _).

	test(wmc_program_duplicate_weight_after_probability, error(domain_error(wmc_duplicate_weight, -p))) :-
		wmc_program::serialize([probability(p, 0.5), weight(-p, 2)], _).

	test(wmc_program_negative_weight, error(domain_error(wmc_statement, weight(p, -1)))) :-
		wmc_program::serialize([weight(p, -1)], _).

	test(wmc_program_atom_weight, error(domain_error(wmc_statement, weight(p, heavy)))) :-
		wmc_program::serialize([weight(p, heavy)], _).

	test(wmc_program_probability_out_of_range, error(domain_error(wmc_statement, probability(p, 1.5)))) :-
		wmc_program::serialize([probability(p, 1.5)], _).

	test(wmc_program_probability_negative, error(domain_error(wmc_statement, probability(p, -0.1)))) :-
		wmc_program::serialize([probability(p, -0.1)], _).

	test(wmc_program_numeric_statement, error(domain_error(wmc_statement, 1))) :-
		wmc_program::serialize([1], _).

	test(wmc_program_not_a_list, error(type_error(list, foo))) :-
		wmc_program::serialize(foo, _).

	test(wmc_program_partial_list, error(instantiation_error)) :-
		wmc_program::serialize([p| _], _).

	test(wmc_program_variable_statement, error(instantiation_error)) :-
		wmc_program::serialize([_], _).

	% fixture objects (expanded using the wmc_expansion hook)

	test(wmc_expansion_statements, deterministic(Statements == [(x1 ; x2), (-x1 ; x3), probability(x1, 0.3), probability(x2, 0.5), probability(x3, 0.2)])) :-
		findall(Statement, wmc_example::wmc_statement(Statement, _), Statements).

	test(wmc_expansion_origins, deterministic) :-
		forall(
			wmc_example::wmc_statement(_, Origin),
			assertion(( Origin = source(File, Start, End), atom(File), integer(Start), integer(End) ))
		).

	test(wmc_expansion_directives, deterministic(Statements == [raw('c trusted raw comment'), comment('a readable comment')])) :-
		findall(Statement, wmc_raw_fixture::wmc_statement(Statement, _), Statements).

	test(wmc_expansion_weights, deterministic(Statements == [weight(+a, 2), weight(-a, 3), probability(b, 0.25)])) :-
		findall(Statement, wmc_free_variables::wmc_statement(Statement, _), Statements).

	% wmc_reference tests

	test(wmc_reference_free_variables, deterministic(Count == 8)) :-
		count_atom('p cnf 3 0\n', Count).

	test(wmc_reference_single_clause, deterministic(Count == 3)) :-
		count_atom('p cnf 2 1\n1 2 0\n', Count).

	test(wmc_reference_unsatisfiable, deterministic(Count == 0)) :-
		count_atom('p cnf 1 2\n1 0\n-1 0\n', Count).

	test(wmc_reference_empty_clause, deterministic(Count == 0)) :-
		count_atom('p cnf 1 1\n0\n', Count).

	test(wmc_reference_independent_components, deterministic(Count == 9)) :-
		count_atom('p cnf 4 2\n1 2 0\n3 4 0\n', Count).

	test(wmc_reference_unit_propagation, deterministic(Count == 2)) :-
		% x1 and x2 are forced, x3 is free
		count_atom('p cnf 3 3\n1 0\n-1 2 0\n2 3 0\n', Count).

	test(wmc_reference_no_header, deterministic(Count == 3)) :-
		count_atom('1 2 0\n', Count).

	test(wmc_reference_clauses_across_lines, deterministic(Count == 3)) :-
		count_atom('p cnf 3 2\n1\nc comment\n2 0 -3 0\n', Count).

	test(wmc_reference_clause_count_mismatch, error(domain_error(wmc_source, header(1, 2)))) :-
		count_atom('p cnf 1 2\n1 0\n', _).

	test(wmc_reference_variable_outside_header, error(domain_error(wmc_source, header(1, 1)))) :-
		count_atom('p cnf 1 1\n2 0\n', _).

	test(wmc_reference_weight_outside_header, error(domain_error(wmc_source, header(1, 0)))) :-
		count_atom('p cnf 1 0\nc p weight 2 0.5 0\n', _).

	test(wmc_reference_malformed_weight, error(domain_error(wmc_source, 'c p weight 1 bad 0'))) :-
		count_atom('p cnf 1 1\nc p weight 1 bad 0\n1 0\n', _).

	test(wmc_reference_negative_weight, error(domain_error(wmc_source, 'c p weight 1 -2 0'))) :-
		count_atom('p cnf 1 1\nc p weight 1 -2 0\n1 0\n', _).

	test(wmc_reference_negative_underflow_weight, error(domain_error(wmc_source, 'c p weight 1 -1e-400 0'))) :-
		count_atom('p cnf 1 1\nc p weight 1 -1e-400 0\n1 0\n', _).

	test(wmc_reference_negative_zero_weight, deterministic) :-
		count_atom('p cnf 1 1\nc p weight 1 -0e-400 0\n1 0\n', Count),
		assertion(Count =:= 0).

	test(wmc_reference_duplicate_weight, error(domain_error(wmc_source, 'c p weight 1 0.9 0'))) :-
		count_atom('p cnf 1 1\nc p weight 1 0.1 0\nc p weight 1 0.9 0\n1 0\n', _).

	test(wmc_reference_bounded_integer_promotion, deterministic, [condition(current_prolog_flag(bounded, true))]) :-
		current_prolog_flag(max_integer, Maximum),
		Half is Maximum // 2,
		wmc::solve(terms([weight(+a, Half), weight(-a, 0)]), ExactResult),
		wmc::value(ExactResult, Exact),
		assertion(integer(Exact)),
		assertion(Exact =:= Half),
		count_atom('p cnf 61 0\n', Approximate),
		assertion(float(Approximate)),
		assertion(close(Approximate, 2.305843009213694e18)),
		wmc::solve(terms([weight(+a, Maximum), weight(-a, Maximum)]), Result),
		wmc::value(Result, Sum),
		assertion(float(Sum)),
		Expected is Maximum * 1.0 * 2,
		assertion(close(Sum, Expected)).

	test(wmc_reference_float_overflow, error(evaluation_error(float_overflow))) :-
		wmc::solve(terms([weight(+a, 1.0e308), weight(-a, 1.0e308)]), _).

	test(wmc_reference_weights, deterministic) :-
		count_atom('p cnf 3 2\nc p weight 1 0.3 0\nc p weight -1 0.7 0\nc p weight 2 0.5 0\nc p weight -2 0.5 0\nc p weight 3 0.2 0\nc p weight -3 0.8 0\n1 2 0\n-1 3 0\n', Count),
		assertion(close(Count, 0.41)).

	test(wmc_reference_integer_weights_exact, deterministic(Count == 11)) :-
		% models of (x1 or x2): (T,T) 2*3 = 6, (T,F) 2*1 = 2, and (F,T) 1*3 = 3
		count_atom('p cnf 2 1\nc p weight 1 2 0\nc p weight -1 1 0\nc p weight 2 3 0\nc p weight -2 1 0\n1 2 0\n', Count).

	test(wmc_reference_scientific_notation_weights, deterministic) :-
		count_atom('p cnf 1 0\nc p weight 1 1e-1 0\nc p weight -1 2.5E+0 0\n', Count),
		assertion(close(Count, 2.6)).

	test(wmc_reference_weights_default_to_one, deterministic(Count == 4)) :-
		count_atom('p cnf 2 0\nc p weight 1 1 0\n', Count).

	test(wmc_reference_malformed_line, error(domain_error(wmc_source, foo))) :-
		count_atom('p cnf 2 1\nfoo\n', _).

	test(wmc_reference_clause_without_terminator, error(domain_error(wmc_source, '1 2'))) :-
		count_atom('p cnf 2 1\n1 2\n', _).

	test(wmc_reference_unbound_codes, error(instantiation_error)) :-
		wmc_reference::count(_, _).

	% wmc_output tests

	test(wmc_output_ganak_weighted, deterministic) :-
		atom_codes('c o banner\ns SATISFIABLE\nc s type wmc\nc s log10-estimate -3.87216143e-01\nc s exact arb frac 41/100\nc s pac guarantees epsilon: 0 delta: 0\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], [x1-1, x2-2, x3-3], Result),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(close(Value, 0.41)),
		assertion(memberchk(representation(arb-frac), Properties)),
		assertion(memberchk(exact_fraction(41/100), Properties)),
		assertion(memberchk(number_type(float), Properties)),
		assertion(memberchk(termination(completed), Properties)),
		assertion(memberchk(variable_count(3), Properties)),
		assertion((memberchk(log10_estimate(Estimate), Properties), close(Estimate, -0.387216143) )).

	test(wmc_output_ganak_unweighted, deterministic) :-
		atom_codes('s SATISFIABLE\nc s type mc\nc s log10-estimate 6.02059991e-01\nc s exact arb int 4\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, Result),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(Value == 4),
		assertion(memberchk(number_type(integer), Properties)),
		assertion(memberchk(representation(arb-int), Properties)),
		assertion(memberchk(value_text('4'), Properties)).

	test(wmc_output_ganak_unsatisfiable, deterministic) :-
		atom_codes('s UNSATISFIABLE\nc s type wmc\nc s log10-estimate -inf\nc s exact arb frac 0\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, Result),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == unsatisfiable),
		assertion(Value =:= 0),
		assertion(\+ memberchk(log10_estimate(_), Properties)).

	test(wmc_output_sharpsat_td_arbitrary_float, deterministic) :-
		atom_codes('s SATISFIABLE\nc s type wmc\nc s log10-estimate -0.38721614328026454643\nc s exact arb float 0.40999999999999997558\n', Codes),
		wmc_output::decode(sharpsat_td, Codes, exit(0), exit(0), [], [], none, Result),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(close(Value, 0.41)),
		assertion(memberchk(representation(arb-float), Properties)).

	test(wmc_output_sharpsat_td_double_with_warning, deterministic) :-
		atom_codes('c o WARNING: Using doubles for weighted model counting\ns SATISFIABLE\nc s type wmc\nc s exact double float 0.41\n', Codes),
		wmc_output::decode(sharpsat_td, Codes, exit(0), exit(0), [], [], none, wmc_result(_, Value, Properties)),
		assertion(close(Value, 0.41)),
		assertion(memberchk(representation(double-float), Properties)).

	test(wmc_output_sharpsat_td_unsatisfiable, deterministic) :-
		atom_codes('s UNSATISFIABLE\nc s type wmc\nc s log10-estimate -inf\nc s exact arb float 0\n', Codes),
		wmc_output::decode(sharpsat_td, Codes, exit(0), exit(0), [], [], none, wmc_result(Status, Value, _)),
		assertion(Status == unsatisfiable),
		assertion(Value =:= 0).

	test(wmc_output_scientific_notation_values, deterministic) :-
		atom_codes('s SATISFIABLE\nc s exact double float 1.5e-07\n', Codes1),
		wmc_output::decode(sharpsat_td, Codes1, exit(0), exit(0), [], [], none, wmc_result(_, Value1, _)),
		assertion(close(Value1, 1.5e-7)),
		atom_codes('s SATISFIABLE\nc s exact double float 1e-5\n', Codes2),
		wmc_output::decode(sharpsat_td, Codes2, exit(0), exit(0), [], [], none, wmc_result(_, Value2, _)),
		assertion(close(Value2, 1.0e-5)),
		atom_codes('s SATISFIABLE\nc s exact quadruple float 4.10000000e-01\n', Codes3),
		wmc_output::decode(ganak, Codes3, exit(0), exit(0), [], [], none, wmc_result(_, Value3, _)),
		assertion(close(Value3, 0.41)).

	test(wmc_output_huge_integer, deterministic, [condition(current_prolog_flag(bounded, false))]) :-
		% backends with bounded integers cannot even read the literal
		atom_codes('123456789012345678901234567890', Digits),
		number_codes(Expected, Digits),
		atom_codes('s SATISFIABLE\nc s exact arb int 123456789012345678901234567890\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, wmc_result(_, Value, _)),
		assertion(Value == Expected).

	test(wmc_output_large_fraction, deterministic) :-
		repeat_code(400, 0'9, Numerator),
		repeat_code(400, 0'0, Zeros),
		atom_codes('s SATISFIABLE\nc s exact arb frac ', Prefix),
		append(Prefix, Numerator, Start),
		append(Start, [0'/, 0'1| Zeros], Body),
		append(Body, [10], Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, wmc_result(counted, Value, Properties)),
		assertion(close(Value, 1.0)),
		(	current_prolog_flag(bounded, false) ->
			assertion(memberchk(exact_fraction(_), Properties))
		;	true
		).

	test(wmc_output_fraction_float_overflow, deterministic) :-
		repeat_code(400, 0'0, Zeros),
		atom_codes('s SATISFIABLE\nc s exact arb frac 1', Prefix),
		append(Prefix, Zeros, Start),
		append(Start, [0'/, 0'1, 10], Codes),
		catch(wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _), error(Error, _), true),
		assertion(conversion_range_error(Error)).

	test(wmc_output_parse_number_float_overflow, deterministic) :-
		atom_codes('1.0e400', Codes),
		catch(number_codes(_, Codes), _, ConversionFailed = true),
		catch(wmc_output::parse_number(Codes, _), error(Error, Context), true),
		(	ConversionFailed == true ->
			assertion(Error == domain_error(number, Codes)),
			assertion(Context = logtalk(_, _))
		;	assertion(Error == evaluation_error(float_overflow))
		).

	test(wmc_output_big_integer_float_overflow, deterministic, [condition(current_prolog_flag(bounded, true))]) :-
		repeat_code(400, 0'0, Zeros),
		atom_codes('s SATISFIABLE\nc s exact arb int 1', Prefix),
		append(Prefix, Zeros, Start),
		append(Start, [10], Codes),
		catch(wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _), error(Error, _), true),
		assertion(conversion_range_error(Error)).

	test(wmc_output_parse_number_large_float, deterministic) :-
		atom_codes('1.0e308', Codes),
		wmc_output::parse_number(Codes, Value),
		assertion(close(Value, 1.0e308)).

	test(wmc_output_zero_count_status, deterministic) :-
		atom_codes('s SATISFIABLE\nc s exact arb frac 0\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, wmc_result(unsatisfiable, Value, _)),
		assertion(Value =:= 0).

	test(wmc_output_inconsistent_unsatisfiable, error(domain_error(wmc_output, 'Result'-inconsistent_count))) :-
		atom_codes('s UNSATISFIABLE\nc s exact arb int 7\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_negative_count, error(domain_error(wmc_output, value-'-7'))) :-
		atom_codes('s SATISFIABLE\nc s exact arb int -7\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_negative_underflow, error(domain_error(wmc_output, value-'-1e-400'))) :-
		atom_codes('s SATISFIABLE\nc s exact arb float -1e-400\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_inconsistent_underflow, error(domain_error(wmc_output, 'Result'-inconsistent_count))) :-
		atom_codes('s UNSATISFIABLE\nc s exact arb float 1e-400\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_unknown_status, deterministic) :-
		atom_codes('s UNKNOWN\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, wmc_result(Status, Value, _)),
		assertion(Status == unknown),
		assertion(Value == none).

	test(wmc_output_time_limit_exit_124, deterministic) :-
		wmc_output::decode(ganak, [], exit(124), exit(124), [], [], none, wmc_result(Status, Value, Properties)),
		assertion(Status == unknown),
		assertion(Value == none),
		assertion(memberchk(termination(resource_limit), Properties)).

	test(wmc_output_time_limit_exit_137, deterministic) :-
		wmc_output::decode(ganak, [], exit(137), exit(137), [], [], none, wmc_result(Status, _, Properties)),
		assertion(Status == unknown),
		assertion(memberchk(termination(resource_limit), Properties)).

	test(wmc_output_time_limit_unnormalized_exit, deterministic) :-
		% backends that cannot normalize nonzero exit codes (e.g., GNU Prolog)
		wmc_output::decode(ganak, [], 124, unknown(124), [], [], none, wmc_result(Status, _, Properties)),
		assertion(Status == unknown),
		assertion(memberchk(termination(resource_limit), Properties)).

	test(wmc_output_interrupted_process, deterministic) :-
		atom_codes('s SATISFIABLE\nc s exact arb int 4\n', Codes),
		wmc_output::decode(ganak, Codes, 15, killed(15), [], [], none, wmc_result(Status, Value, Properties)),
		assertion(Status == counted),
		assertion(Value == 4),
		assertion(memberchk(termination(interrupted), Properties)).

	test(wmc_output_crlf_lines, deterministic(Value == 4)) :-
		atom_codes('s SATISFIABLE\r\nc s exact arb int 4\r\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, wmc_result(_, Value, _)).

	test(wmc_output_missing_status, error(domain_error(wmc_output, 'Result'-missing))) :-
		atom_codes('c just a comment\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_unsupported_status, error(domain_error(wmc_output, 'Result'-'SOMETHING ELSE'))) :-
		atom_codes('s SOMETHING ELSE\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_missing_count, error(domain_error(wmc_output, 'Result'-no_count))) :-
		atom_codes('s SATISFIABLE\nc s type mc\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_unsupported_kind, error(domain_error(wmc_output, kind-weird))) :-
		atom_codes('s SATISFIABLE\nc s exact arb weird 1\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_invalid_integer, error(domain_error(wmc_output, value-'1.5'))) :-
		atom_codes('s SATISFIABLE\nc s exact arb int 1.5\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_invalid_fraction, error(domain_error(wmc_output, value-'4/0'))) :-
		atom_codes('s SATISFIABLE\nc s exact arb frac 4/0\n', Codes),
		wmc_output::decode(ganak, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_invalid_float, error(domain_error(wmc_output, value-nan))) :-
		atom_codes('s SATISFIABLE\nc s exact double float nan\n', Codes),
		wmc_output::decode(sharpsat_td, Codes, exit(0), exit(0), [], [], none, _).

	test(wmc_output_unexpected_exit_code, error(domain_error(wmc_process_status, exit(1)))) :-
		atom_codes('s SATISFIABLE\nc s exact arb int 4\n', Codes),
		wmc_output::decode(ganak, Codes, exit(1), exit(1), [], [], none, _).

	test(wmc_output_fatal_status, error(domain_error(wmc_process_status, error))) :-
		atom_codes('s SATISFIABLE\nc s exact arb int 4\n', Codes),
		wmc_output::decode(ganak, Codes, error, error, [], [], none, _).

	test(wmc_output_unbound_solver, error(instantiation_error)) :-
		wmc_output::decode(_, [], exit(0), exit(0), [], [], none, _).

	test(wmc_output_non_atom_solver, error(type_error(atom, 1))) :-
		wmc_output::decode(1, [], exit(0), exit(0), [], [], none, _).

	test(wmc_output_invalid_variables, error(type_error(list(pair(ground,ground)), foo))) :-
		wmc_output::decode(ganak, [], exit(0), exit(0), [], [], foo, _).

	test(wmc_output_parse_number, deterministic) :-
		atom_codes('-1.25e+2', Codes),
		wmc_output::parse_number(Codes, Number),
		assertion(close(Number, -125.0)).

	test(wmc_output_parse_number_invalid, fail) :-
		atom_codes('1.2.3', Codes),
		wmc_output::parse_number(Codes, _).

	% result inspection tests

	test(wmc_value, deterministic(Value == 5)) :-
		wmc::value(wmc_result(counted, 5, []), Value).

	test(wmc_value_missing, fail) :-
		wmc::value(wmc_result(unknown, none, []), _).

	test(wmc_result_property, deterministic(Properties == [status(counted), number_type(integer), solver(ganak)])) :-
		Result = wmc_result(counted, 5, [number_type(integer), solver(ganak)]),
		findall(Property, wmc::result_property(Result, Property), Properties).

	test(wmc_print_result_integer, deterministic) :-
		assert_printed_result(wmc_result(counted, 5, []), 's SATISFIABLE\nc s exact arb int 5\n').

	test(wmc_print_result_float, deterministic) :-
		assert_printed_result(wmc_result(counted, 0.5, []), 's SATISFIABLE\nc s exact double float 0.5\n').

	test(wmc_print_result_unsatisfiable, deterministic) :-
		assert_printed_result(wmc_result(unsatisfiable, 0, []), 's UNSATISFIABLE\nc s exact arb int 0\n').

	test(wmc_print_result_round_trip, deterministic) :-
		printed_result_codes(wmc_result(counted, 5, []), IntegerCodes),
		wmc_output::decode(ganak, IntegerCodes, exit(0), exit(0), [], [], none, wmc_result(counted, 5, _)),
		printed_result_codes(wmc_result(counted, 0.5, []), FloatCodes),
		wmc_output::decode(ganak, FloatCodes, exit(0), exit(0), [], [], none, wmc_result(counted, Float, _)),
		assertion(close(Float, 0.5)),
		printed_result_codes(wmc_result(unknown, none, []), UnknownCodes),
		wmc_output::decode(ganak, UnknownCodes, exit(0), exit(0), [], [], none, wmc_result(unknown, none, _)).

	test(wmc_print_result_unknown, deterministic) :-
		assert_printed_result(wmc_result(unknown, none, []), 's UNKNOWN\n').

	test(wmc_print_result_invalid, error(domain_error(wmc_result, invalid))) :-
		wmc::print_result(invalid).

	test(wmc_print_result_unbound, error(instantiation_error)) :-
		wmc::print_result(_).

	% program generation tests

	test(wmc_program_object_embeds_variables, deterministic(Atom == 'p cnf 3 2\nc v 1 in(a)\nc v 2 in(b)\nc v 3 in(c)\n-1 -2 0\n-2 -3 0\n')) :-
		wmc::program(object(wmc_independent_sets), Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_object_without_markers, deterministic(Atom == 'p cnf 3 2\n-1 -2 0\n-2 -3 0\n')) :-
		wmc::program(object(wmc_independent_sets), Codes, [embed_variables(false)]),
		atom_codes(Atom, Codes).

	test(wmc_program_terms_source, deterministic(Atom == 'p cnf 2 1\nc p weight 1 0.25 0\nc p weight -1 0.75 0\nc p weight 2 1 0\nc p weight -2 1 0\n1 2 0\n')) :-
		wmc::program(terms([(p ; q), probability(p, 0.25)]), Codes, [embed_variables(false)]),
		atom_codes(Atom, Codes).

	test(wmc_program_terms_source_markers, deterministic(Atom == 'p cnf 2 1\nc v 1 p\nc v 2 q\n1 2 0\n')) :-
		wmc::program(terms([(p ; q)]), Codes),
		atom_codes(Atom, Codes).

	test(wmc_program_codes_source, deterministic(Codes == [112, 32, 99, 10])) :-
		wmc::program(codes([112, 32, 99, 10]), Codes).

	test(wmc_program_file_source, deterministic) :-
		object_property(tests, file(_, Directory)),
		atom_concat(Directory, 'test_files/example.cnf', Path),
		wmc::program(file(Path), Codes),
		assertion(Codes = [0'c, 0' | _]).

	test(wmc_program_wrong_object, error(domain_error(wmc_source, object(wmc_unrelated_fixture)))) :-
		wmc::program(object(wmc_unrelated_fixture), _).

	test(wmc_program_wrong_source, error(domain_error(wmc_source, foo))) :-
		wmc::program(foo, _).

	test(wmc_program_unbound, error(instantiation_error)) :-
		wmc::program(_).

	test(wmc_program_print_codes, deterministic) :-
		atom_codes('p cnf 1 1\n1 0\n', Codes),
		^^set_text_output(''),
		wmc::program(codes(Codes)),
		^^text_output_contents(Chars),
		subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == 'p cnf 1 1\n1 0\n').

	% solve/2-3 error handling tests (no solver required)

	test(wmc_solve_missing_executable, error(existence_error(command, logtalk_wmc_missing_executable))) :-
		wmc::solve(terms([p]), _, [solver(ganak), executable(logtalk_wmc_missing_executable)]).

	test(wmc_solve_unbound_source, error(instantiation_error)) :-
		wmc::solve(_, _).

	test(wmc_solve_invalid_option_solver, error(domain_error(option, solver(kissat)))) :-
		wmc::solve(terms([p]), _, [solver(kissat)]).

	test(wmc_solve_invalid_option_time_limit, error(domain_error(option, time_limit(-1)))) :-
		wmc::solve(terms([p]), _, [time_limit(-1)]).

	test(wmc_solve_invalid_option_decomposition_time, error(domain_error(option, decomposition_time(0)))) :-
		wmc::solve(terms([p]), _, [decomposition_time(0)]).

	test(wmc_solve_unbound_options, error(instantiation_error)) :-
		wmc::solve(terms([p]), _, _).

	test(wmc_solve_wrong_source, error(domain_error(wmc_source, foo))) :-
		wmc::solve(foo, _).

	test(wmc_solve_wrong_object, error(domain_error(wmc_source, object(wmc_unrelated_fixture)))) :-
		wmc::solve(object(wmc_unrelated_fixture), _).

	test(wmc_solve_invalid_statement, error(domain_error(wmc_statement, weight(p, -1)))) :-
		wmc::solve(terms([weight(p, -1)]), _).

	% reference counter source variants through solve/2-3

	test(wmc_solve_reference_is_the_default, deterministic) :-
		wmc::solve(object(wmc_independent_sets), Result),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(Value == 5),
		assertion(memberchk(solver(reference), Properties)),
		assertion(memberchk(termination(completed), Properties)),
		assertion(memberchk(variables([in(a)-1, in(b)-2, in(c)-3]), Properties)).

	test(wmc_solve_time_limit_with_reference_is_ignored, deterministic(Value == 5)) :-
		wmc::solve(object(wmc_independent_sets), Result, [time_limit(1)]),
		Result = wmc_result(_, Value, _).

	% time limit enforcement using a fake counter (requires the timeout command)

	test(wmc_solve_time_limit_enforced, deterministic, [condition(os::resolve_command_path(timeout, _))]) :-
		object_property(tests, file(_, Directory)),
		atom_concat(Directory, 'test_files/slow_counter.sh', Counter),
		wmc::solve(terms([p]), Result, [solver(ganak), executable(Counter), time_limit(1)]),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == unknown),
		assertion(Value == none),
		assertion(memberchk(termination(resource_limit), Properties)).

	test(wmc_solve_time_limit_missing_timeout_command, error(existence_error(command, timeout)), [condition(\+ os::resolve_command_path(timeout, _))]) :-
		wmc::solve(terms([p]), _, [solver(ganak), executable(sh), time_limit(1)]).

	test(wmc_process_existing_arguments, deterministic, [condition(\+ os::operating_system_type(windows))]) :-
		os::resolve_command_path(sh, Shell),
		wmc_process::run(Shell, ['-c', 'printf existing'], codes([]), Output, [], _, exit(0)),
		atom_codes(existing, Output).

	test(wmc_process_unbound_argument, error(instantiation_error)) :-
		wmc_process::run(sh, [_], codes([]), _, _, _, _).

	test(wmc_solve_weighted_source_tabs, deterministic, [condition(\+ os::operating_system_type(windows))]) :-
		fake_counter(Counter),
		atom_codes('p cnf 1 1\nc p weight\t1 0.25 0\n1 0\n', Codes),
		wmc::solve(codes(Codes), Result, [solver(ganak), executable(Counter)]),
		wmc::value(Result, Value),
		assertion(close(Value, 0.25)).

	test(wmc_sharpsat_cleanup_completed, deterministic, [condition(\+ os::operating_system_type(windows))]) :-
		fake_counter(Counter),
		wmc::solve(terms([p]), Result, [solver(sharpsat_td), executable(Counter), decomposition_time(1)]),
		assertion(Result = wmc_result(counted, 1, _)),
		assert_workspace_removed(Result).

	test(wmc_sharpsat_cleanup_file, deterministic, [condition(\+ os::operating_system_type(windows))]) :-
		fake_counter(Counter),
		^^file_path('test_files/example.cnf', Path),
		wmc::solve(file(Path), Result, [solver(sharpsat_td), executable(Counter), decomposition_time(1)]),
		assertion(Result = wmc_result(counted, 1, _)),
		assert_workspace_removed(Result),
		assertion(os::file_exists(Path)).

	test(wmc_sharpsat_cleanup_timeout, deterministic, [condition((\+ os::operating_system_type(windows), os::resolve_command_path(timeout, _)))]) :-
		fake_counter(Counter),
		wmc::solve(terms([p]), Result, [solver(sharpsat_td), executable(Counter), decomposition_time(2), time_limit(1)]),
		assertion(Result = wmc_result(unknown, none, _)),
		assert_workspace_removed(Result).

	test(wmc_sharpsat_cleanup_forced_timeout, deterministic, [condition((\+ os::operating_system_type(windows), os::resolve_command_path(timeout, _)))]) :-
		fake_counter(Counter),
		catch(
			(	wmc::solve(terms([p]), Result, [solver(sharpsat_td), executable(Counter), decomposition_time(3), time_limit(1)]),
				Result = wmc_result(unknown, none, Properties),
				assertion(memberchk(termination(resource_limit), Properties)),
				memberchk(diagnostics(Diagnostics), Properties)
			),
			error(wmc_solver_error(domain_error(wmc_output, 'Result'-missing), ProcessStatus, Diagnostics, _), _),
			memberchk(ProcessStatus, [killed(9), unknown(9)])
		),
		assert_diagnostics_workspace_removed(Diagnostics).

	% external Ganak and SharpSAT-TD CLI solving tests and reference counter tests

	test(wmc_solve_invalid_cnf_ganak, error(wmc_solver_error(_, _, _, _)), [condition(os::resolve_command_path(ganak, _))]) :-
		wmc::solve(codes([0'x, 10]), _, [solver(ganak)]).

	test(wmc_solve_example_object_reference, deterministic) :-
		solver_options(reference, Options),
		wmc::solve(object(wmc_example), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(close(Value, 0.41)),
		assertion(memberchk(solver(reference), Properties)),
		assertion(memberchk(termination(completed), Properties)),
		assertion(memberchk(variable_count(3), Properties)),
		assertion(memberchk(number_type(float), Properties)).

	test(wmc_solve_example_terms_reference, deterministic) :-
		solver_options(reference, Options),
		Statements = [(x1 ; x2), (-x1 ; x3), probability(x1, 0.3), probability(x2, 0.5), probability(x3, 0.2)],
		wmc::solve(terms(Statements), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 0.41)).

	test(wmc_solve_example_file_reference, deterministic) :-
		solver_options(reference, Options),
		object_property(tests, file(_, Directory)),
		atom_concat(Directory, 'test_files/example.cnf', Path),
		wmc::solve(file(Path), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(close(Value, 0.41)),
		assertion(memberchk(variables([x1-1, x2-2, x3-3]), Properties)).

	test(wmc_solve_example_codes_reference, deterministic) :-
		solver_options(reference, Options),
		atom_codes('p cnf 3 2\nc p weight 1 0.3 0\nc p weight -1 0.7 0\nc p weight 2 0.5 0\nc p weight -2 0.5 0\nc p weight 3 0.2 0\nc p weight -3 0.8 0\n1 2 0\n-1 3 0\n', Codes),
		wmc::solve(codes(Codes), Result, Options),
		Result = wmc_result(_, Value, Properties),
		assertion(close(Value, 0.41)),
		assertion(memberchk(variables(none), Properties)).

	test(wmc_solve_independent_sets_reference, deterministic) :-
		solver_options(reference, Options),
		wmc::solve(object(wmc_independent_sets), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(Value == 5),
		assertion(memberchk(number_type(integer), Properties)).

	test(wmc_solve_alarm_reference, deterministic) :-
		solver_options(reference, Options),
		wmc::solve(object(wmc_alarm), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 1.0)).

	test(wmc_solve_alarm_evidence_reference, deterministic) :-
		solver_options(reference, Options),
		wmc::solve(object(wmc_alarm_evidence), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 0.28)).

	test(wmc_solve_unsatisfiable_reference, deterministic) :-
		solver_options(reference, Options),
		wmc::solve(object(wmc_unsatisfiable), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == unsatisfiable),
		assertion(Value =:= 0),
		assertion(memberchk(termination(completed), Properties)).

	test(wmc_solve_free_variables_reference, deterministic) :-
		solver_options(reference, Options),
		wmc::solve(object(wmc_free_variables), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 5.0)).

	test(wmc_solve_print_result_reference, deterministic) :-
		solver_options(reference, Options),
		wmc::solve(object(wmc_independent_sets), Result, Options),
		assert_printed_result(Result, 's SATISFIABLE\nc s exact arb int 5\n').

	test(random_generator_seed_sequence, deterministic) :-
		next_seed(0, 12345),
		next_seed(1, 1103527590),
		next_seed(1073741824, 1073754169),
		next_seed(2147483647, 1043980748),
		next_seed(11007, Seed1),
		next_seed(Seed1, Seed2),
		next_seed(Seed2, Seed3),
		next_seed(Seed3, Seed4),
		assertion([Seed1, Seed2, Seed3, Seed4] == [224800972, 1630084885, 1161351722, 2142664731]).

	test(wmc_solve_random_unweighted_instances_reference, deterministic) :-
		solver_options(reference, Options),
		instance_count(40, Count), random_instances(Count, 11007, false, Options).

	test(wmc_solve_random_weighted_instances_reference, deterministic) :-
		solver_options(reference, Options),
		instance_count(40, Count), random_instances(Count, 12013, true, Options).

	test(wmc_solve_example_object_ganak, deterministic, [condition(os::resolve_command_path(ganak, _))]) :-
		solver_options(ganak, Options),
		wmc::solve(object(wmc_example), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(close(Value, 0.41)),
		assertion(memberchk(solver(ganak), Properties)),
		assertion(memberchk(termination(completed), Properties)),
		assertion(memberchk(variable_count(3), Properties)),
		assertion(memberchk(number_type(float), Properties)).

	test(wmc_solve_example_terms_ganak, deterministic, [condition(os::resolve_command_path(ganak, _))]) :-
		solver_options(ganak, Options),
		Statements = [(x1 ; x2), (-x1 ; x3), probability(x1, 0.3), probability(x2, 0.5), probability(x3, 0.2)],
		wmc::solve(terms(Statements), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 0.41)).

	test(wmc_solve_example_file_ganak, deterministic, [condition(os::resolve_command_path(ganak, _))]) :-
		solver_options(ganak, Options),
		object_property(tests, file(_, Directory)),
		atom_concat(Directory, 'test_files/example.cnf', Path),
		wmc::solve(file(Path), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(close(Value, 0.41)),
		assertion(memberchk(variables([x1-1, x2-2, x3-3]), Properties)).

	test(wmc_solve_example_codes_ganak, deterministic, [condition(os::resolve_command_path(ganak, _))]) :-
		solver_options(ganak, Options),
		atom_codes('p cnf 3 2\nc p weight 1 0.3 0\nc p weight -1 0.7 0\nc p weight 2 0.5 0\nc p weight -2 0.5 0\nc p weight 3 0.2 0\nc p weight -3 0.8 0\n1 2 0\n-1 3 0\n', Codes),
		wmc::solve(codes(Codes), Result, Options),
		Result = wmc_result(_, Value, Properties),
		assertion(close(Value, 0.41)),
		assertion(memberchk(variables(none), Properties)).

	test(wmc_solve_independent_sets_ganak, deterministic, [condition(os::resolve_command_path(ganak, _))]) :-
		solver_options(ganak, Options),
		wmc::solve(object(wmc_independent_sets), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(Value == 5),
		assertion(memberchk(number_type(integer), Properties)).

	test(wmc_solve_alarm_ganak, deterministic, [condition(os::resolve_command_path(ganak, _))]) :-
		solver_options(ganak, Options),
		wmc::solve(object(wmc_alarm), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 1.0)).

	test(wmc_solve_alarm_evidence_ganak, deterministic, [condition(os::resolve_command_path(ganak, _))]) :-
		solver_options(ganak, Options),
		wmc::solve(object(wmc_alarm_evidence), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 0.28)).

	test(wmc_solve_unsatisfiable_ganak, deterministic, [condition(os::resolve_command_path(ganak, _))]) :-
		solver_options(ganak, Options),
		wmc::solve(object(wmc_unsatisfiable), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == unsatisfiable),
		assertion(Value =:= 0),
		assertion(memberchk(termination(completed), Properties)).

	test(wmc_solve_free_variables_ganak, deterministic, [condition(os::resolve_command_path(ganak, _))]) :-
		solver_options(ganak, Options),
		wmc::solve(object(wmc_free_variables), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 5.0)).

	test(wmc_solve_print_result_ganak, deterministic, [condition(os::resolve_command_path(ganak, _))]) :-
		solver_options(ganak, Options),
		wmc::solve(object(wmc_independent_sets), Result, Options),
		assert_printed_result(Result, 's SATISFIABLE\nc s exact arb int 5\n').

	test(wmc_solve_random_unweighted_instances_ganak, deterministic, [condition((os::resolve_command_path(ganak, _), \+ current_logtalk_flag(prolog_dialect, gnu)))]) :-
		solver_options(ganak, Options),
		instance_count(25, Count), random_instances(Count, 21007, false, Options).

	test(wmc_solve_random_weighted_instances_ganak, deterministic, [condition((os::resolve_command_path(ganak, _), \+ current_logtalk_flag(prolog_dialect, gnu)))]) :-
		solver_options(ganak, Options),
		instance_count(25, Count), random_instances(Count, 22013, true, Options).

	test(wmc_solve_time_limit_option_ganak, deterministic, [condition((os::resolve_command_path(ganak, _), os::resolve_command_path(timeout, _)))]) :-
		solver_options(ganak, Options0),
		wmc::solve(object(wmc_independent_sets), Result, [time_limit(60)| Options0]),
		Result = wmc_result(Status, Value, _),
		assertion(Status == counted),
		assertion(Value == 5).

	test(wmc_solve_example_object_sharpsat_td, deterministic, [condition(os::resolve_command_path('sharpSAT', _))]) :-
		solver_options(sharpsat_td, Options),
		wmc::solve(object(wmc_example), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(close(Value, 0.41)),
		assertion(memberchk(solver(sharpsat_td), Properties)),
		assertion(memberchk(termination(completed), Properties)),
		assertion(memberchk(variable_count(3), Properties)),
		assertion(memberchk(number_type(float), Properties)).

	test(wmc_solve_example_terms_sharpsat_td, deterministic, [condition(os::resolve_command_path('sharpSAT', _))]) :-
		solver_options(sharpsat_td, Options),
		Statements = [(x1 ; x2), (-x1 ; x3), probability(x1, 0.3), probability(x2, 0.5), probability(x3, 0.2)],
		wmc::solve(terms(Statements), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 0.41)).

	test(wmc_solve_example_file_sharpsat_td, deterministic, [condition(os::resolve_command_path('sharpSAT', _))]) :-
		solver_options(sharpsat_td, Options),
		object_property(tests, file(_, Directory)),
		atom_concat(Directory, 'test_files/example.cnf', Path),
		wmc::solve(file(Path), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(close(Value, 0.41)),
		assertion(memberchk(variables([x1-1, x2-2, x3-3]), Properties)).

	test(wmc_solve_example_codes_sharpsat_td, deterministic, [condition(os::resolve_command_path('sharpSAT', _))]) :-
		solver_options(sharpsat_td, Options),
		atom_codes('p cnf 3 2\nc p weight 1 0.3 0\nc p weight -1 0.7 0\nc p weight 2 0.5 0\nc p weight -2 0.5 0\nc p weight 3 0.2 0\nc p weight -3 0.8 0\n1 2 0\n-1 3 0\n', Codes),
		wmc::solve(codes(Codes), Result, Options),
		Result = wmc_result(_, Value, Properties),
		assertion(close(Value, 0.41)),
		assertion(memberchk(variables(none), Properties)).

	test(wmc_solve_independent_sets_sharpsat_td, deterministic, [condition(os::resolve_command_path('sharpSAT', _))]) :-
		solver_options(sharpsat_td, Options),
		wmc::solve(object(wmc_independent_sets), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == counted),
		assertion(Value == 5),
		assertion(memberchk(number_type(integer), Properties)).

	test(wmc_solve_alarm_sharpsat_td, deterministic, [condition(os::resolve_command_path('sharpSAT', _))]) :-
		solver_options(sharpsat_td, Options),
		wmc::solve(object(wmc_alarm), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 1.0)).

	test(wmc_solve_alarm_evidence_sharpsat_td, deterministic, [condition(os::resolve_command_path('sharpSAT', _))]) :-
		solver_options(sharpsat_td, Options),
		wmc::solve(object(wmc_alarm_evidence), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 0.28)).

	test(wmc_solve_unsatisfiable_sharpsat_td, deterministic, [condition(os::resolve_command_path('sharpSAT', _))]) :-
		solver_options(sharpsat_td, Options),
		wmc::solve(object(wmc_unsatisfiable), Result, Options),
		Result = wmc_result(Status, Value, Properties),
		assertion(Status == unsatisfiable),
		assertion(Value =:= 0),
		assertion(memberchk(termination(completed), Properties)).

	test(wmc_solve_free_variables_sharpsat_td, deterministic, [condition(os::resolve_command_path('sharpSAT', _))]) :-
		solver_options(sharpsat_td, Options),
		wmc::solve(object(wmc_free_variables), Result, Options),
		wmc::value(Result, Value),
		assertion(close(Value, 5.0)).

	test(wmc_solve_print_result_sharpsat_td, deterministic, [condition(os::resolve_command_path('sharpSAT', _))]) :-
		solver_options(sharpsat_td, Options),
		wmc::solve(object(wmc_independent_sets), Result, Options),
		assert_printed_result(Result, 's SATISFIABLE\nc s exact arb int 5\n').

	test(wmc_solve_random_unweighted_instances_sharpsat_td, deterministic, [condition((os::resolve_command_path('sharpSAT', _), \+ current_logtalk_flag(prolog_dialect, gnu)))]) :-
		solver_options(sharpsat_td, Options),
		instance_count(10, Count), random_instances(Count, 31007, false, Options).

	test(wmc_solve_random_weighted_instances_sharpsat_td, deterministic, [condition((os::resolve_command_path('sharpSAT', _), \+ current_logtalk_flag(prolog_dialect, gnu)))]) :-
		solver_options(sharpsat_td, Options),
		instance_count(10, Count), random_instances(Count, 32013, true, Options).

	test(wmc_solve_time_limit_option_sharpsat_td, deterministic, [condition((os::resolve_command_path('sharpSAT', _), os::resolve_command_path(timeout, _)))]) :-
		solver_options(sharpsat_td, Options0),
		wmc::solve(object(wmc_independent_sets), Result, [time_limit(60)| Options0]),
		Result = wmc_result(Status, Value, _),
		assertion(Status == counted),
		assertion(Value == 5).

	% auxiliary predicates

	count_atom(Atom, Count) :-
		atom_codes(Atom, Codes),
		wmc_reference::count(Codes, Count).

	conversion_range_error(Error) :-
		nonvar(Error),
		(	Error = domain_error(number, _)
		;	Error = evaluation_error(float_overflow)
		).

	close(Value, Expected) :-
		abs(Value - Expected) =< 1.0e-9 * max(1, abs(Expected)).

	% fewer random instances on GNU Prolog, whose small default global stack is exhausted by
	% the tests running in debug mode with coverage when using more instances
	:- if(current_logtalk_flag(prolog_dialect, gnu)).
		instance_count(Count0, Count) :-
			Count is max(2, Count0 // 5).
	:- else.
		instance_count(Count, Count).
	:- endif.

	solver_options(reference, [solver(reference)]).
	solver_options(ganak, [solver(ganak)]).
	solver_options(sharpsat_td, [solver(sharpsat_td), decomposition_time(0.1)]).

	assert_printed_result(Result, Expected) :-
		^^set_text_output(''),
		wmc::print_result(Result),
		^^text_output_contents(Chars),
		subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		assertion(Output == Expected).

	printed_result_codes(Result, Codes) :-
		^^set_text_output(''),
		wmc::print_result(Result),
		^^text_output_contents(Chars),
		subtract(Chars, ['\r'], Normalized),
		atom_chars(Output, Normalized),
		atom_codes(Output, Codes).

	repeat_code(0, _, []) :-
		!.
	repeat_code(Count, Code, [Code| Codes]) :-
		Next is Count - 1,
		repeat_code(Next, Code, Codes).

	fake_counter(Counter) :-
		^^file_path('test_files/fake_counter.sh', Counter).

	% The fake counter writes its workspace path to stderr, captured as diagnostics.
	assert_workspace_removed(Result) :-
		Result = wmc_result(_, _, Properties),
		memberchk(diagnostics(Diagnostics), Properties),
		assert_diagnostics_workspace_removed(Diagnostics).

	% The fake counter emits the path as a single newline-terminated line.
	assert_diagnostics_workspace_removed(Diagnostics) :-
		once(append(PathCodes, [10], Diagnostics)),
		atom_codes(Path, PathCodes),
		assertion(\+ os::directory_exists(Path)).

	% deterministic pseudo-random instance generation (linear congruential generator)

	next_seed(Seed0, Seed) :-
		% Split the seed into 16-bit parts to avoid overflow before reduction modulo 2^31.
		Low is Seed0 mod 65536,
		High is ((Seed0 // 65536) * 1103515245) mod 32768,
		Seed is (Low * 1103515245 + High * 65536 + 12345) mod 2147483648.

	random_int(Low, High, Seed0, Seed, Value) :-
		next_seed(Seed0, Seed),
		Value is Low + (Seed // 65536) mod (High - Low + 1).

	random_literal(Variables, Seed0, Seed, Literal) :-
		random_int(1, Variables, Seed0, Seed1, Index),
		random_int(0, 1, Seed1, Seed, Sign),
		(	Sign =:= 0 -> Literal = +x(Index) ; Literal = -x(Index) ).

	random_literals(0, _, Seed, Seed, []) :-
		!.
	random_literals(Count, Variables, Seed0, Seed, [Literal| Literals]) :-
		random_literal(Variables, Seed0, Seed1, Literal),
		Count1 is Count - 1,
		random_literals(Count1, Variables, Seed1, Seed, Literals).

	random_clauses(0, _, Seed, Seed, []) :-
		!.
	random_clauses(Count, Variables, Seed0, Seed, [Literals| Clauses]) :-
		random_int(1, 3, Seed0, Seed1, Length),
		random_literals(Length, Variables, Seed1, Seed2, Literals),
		Count1 is Count - 1,
		random_clauses(Count1, Variables, Seed2, Seed, Clauses).

	% random weights: none, integer weights for both literals, or a probability

	random_weights(0, Seed, Seed, []) :-
		!.
	random_weights(Index, Seed0, Seed, Weights) :-
		random_int(0, 2, Seed0, Seed1, Kind),
		random_int(1, 4, Seed1, Seed2, Positive),
		random_int(1, 4, Seed2, Seed3, Negative),
		random_int(0, 4, Seed3, Seed4, Choice),
		nth_probability(Choice, Probability),
		(	Kind =:= 0 ->
			Weights = Weights1
		;	Kind =:= 1 ->
			Weights = [weight(+x(Index), Positive), weight(-x(Index), Negative)| Weights1]
		;	Weights = [probability(x(Index), Probability)| Weights1]
		),
		Index1 is Index - 1,
		random_weights(Index1, Seed4, Seed, Weights1).

	nth_probability(0, 0.25).
	nth_probability(1, 0.5).
	nth_probability(2, 0.75).
	nth_probability(3, 0.125).
	nth_probability(4, 0.875).

	% a random instance: clauses over the variables x(1) to x(N); variables always occur in the
	% statements (using a tautology for unweighted problems or an explicit weight for weighted ones)

	random_instance(Seed0, Seed, Weighted, Variables, Clauses, Statements) :-
		random_int(2, 6, Seed0, Seed1, Variables),
		random_int(1, 5, Seed1, Seed2, ClauseCount),
		random_clauses(ClauseCount, Variables, Seed2, Seed3, Clauses),
		clause_statements(Clauses, ClauseStatements),
		(	Weighted == true ->
			random_weights(Variables, Seed3, Seed, Weights),
			mention_all(Variables, Weights, Mentions),
			append(ClauseStatements, Mentions, Statements0),
			append(Statements0, Weights, Statements)
		;	Seed = Seed3,
			tautologies(Variables, Tautologies),
			append(Tautologies, ClauseStatements, Statements)
		).

	clause_statements([], []).
	clause_statements([Literals| Clauses], [Disjunction| Statements]) :-
		disjunction(Literals, Disjunction),
		clause_statements(Clauses, Statements).

	disjunction([Literal], Literal) :-
		!.
	disjunction([Literal| Literals], (Literal ; Disjunction)) :-
		disjunction(Literals, Disjunction).

	tautologies(0, []) :-
		!.
	tautologies(Index, [(+x(Index) ; -x(Index))| Tautologies]) :-
		Index1 is Index - 1,
		tautologies(Index1, Tautologies).

	% variables without a weight statement still need to occur in the statements; a tautological clause
	% does that without changing the count
	mention_all(0, _, []) :-
		!.
	mention_all(Index, Weights, Mentions) :-
		Index1 is Index - 1,
		Mentions = [(+x(Index) ; -x(Index))| Mentions1],
		mention_all(Index1, Weights, Mentions1).

	% brute force oracle: sum over all assignments satisfying the clauses of the product of the literal weights

	oracle_count(Variables, Clauses, Statements, Count) :-
		weight_table(Variables, Statements, Table),
		findall(
			Weight,
			(	assignment(Variables, Assignment),
				clauses_satisfied(Clauses, Assignment),
				assignment_weight(Assignment, 1, Table, Weight)
			),
			Weights
		),
		sum(Weights, Count).

	assignment(0, []) :-
		!.
	assignment(Variables, [Value| Values]) :-
		(	Value = true ; Value = false ),
		Variables1 is Variables - 1,
		assignment(Variables1, Values).

	clauses_satisfied([], _).
	clauses_satisfied([Clause| Clauses], Assignment) :-
		clause_true(Clause, Assignment),
		clauses_satisfied(Clauses, Assignment).

	clause_true([Literal| Literals], Assignment) :-
		(	literal_true(Literal, Assignment) ->
			true
		;	clause_true(Literals, Assignment)
		).

	literal_true(+x(Index), Assignment) :-
		nth_value(Index, Assignment, true).
	literal_true(-x(Index), Assignment) :-
		nth_value(Index, Assignment, false).

	% assignments list the values of x(1) to x(N) in that order
	nth_value(Index, Values, Value) :-
		length(Values, N),
		Position is N - Index + 1,
		nth_from_start(Position, Values, Value).

	nth_from_start(1, [Value| _], Value) :-
		!.
	nth_from_start(Position, [_| Values], Value) :-
		Position > 1,
		Position1 is Position - 1,
		nth_from_start(Position1, Values, Value).

	% table of weights(Positive, Negative) per variable index (default 1)
	weight_table(Variables, Statements, Table) :-
		table_entries(Variables, Statements, Table).

	table_entries(0, _, []) :-
		!.
	table_entries(Index, Statements, [Index-weights(Positive, Negative)| Table]) :-
		(	memberchk(probability(x(Index), P), Statements) ->
			Positive = P,
			Negative is 1 - P
		;	(	memberchk(weight(+x(Index), Positive0), Statements) -> Positive = Positive0 ; Positive = 1 ),
			(	memberchk(weight(-x(Index), Negative0), Statements) -> Negative = Negative0 ; Negative = 1 )
		),
		Index1 is Index - 1,
		table_entries(Index1, Statements, Table).

	assignment_weight(Assignment, Weight0, Table, Weight) :-
		length(Assignment, N),
		assignment_weight(Assignment, N, Weight0, Table, Weight).

	assignment_weight([], _, Weight, _, Weight).
	assignment_weight([Value| Values], Index, Weight0, Table, Weight) :-
		memberchk(Index-weights(Positive, Negative), Table),
		(	Value == true ->
			Factor = Positive
		;	Factor = Negative
		),
		Weight1 is Weight0 * Factor,
		Index1 is Index - 1,
		assignment_weight(Values, Index1, Weight1, Table, Weight).

	random_instances(0, _, _, _) :-
		!.
	random_instances(Count, Seed0, Weighted, Options) :-
		random_instance(Seed0, Seed, Weighted, Variables, Clauses, Statements),
		oracle_count(Variables, Clauses, Statements, Expected),
		wmc::solve(terms(Statements), Result, Options),
		Result = wmc_result(Status, Value, _),
		(	Expected =:= 0 ->
			assertion(Status == unsatisfiable),
			assertion(Value =:= 0)
		;	assertion(Status == counted),
			(	Weighted == true ->
				assertion(close(Value, Expected))
			;	assertion(Value == Expected)
			)
		),
		Count1 is Count - 1,
		random_instances(Count1, Seed, Weighted, Options).

:- end_object.
