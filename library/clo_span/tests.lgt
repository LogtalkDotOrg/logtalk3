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
		date is 2026-04-22,
		comment is 'Unit tests for the "clo_span" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(clo_span).

	cleanup :-
		^^clean_file('test_output.pl').

	test(clo_span_mine_2_structure, deterministic(functor(PatternMiner, clo_span_pattern_miner, 3))) :-
		clo_span::mine(closure_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(3)]).

	test(clo_span_mine_3_closed_patterns, deterministic(Patterns == [sequence_pattern([[a], [b]], 5), sequence_pattern([[a], [b], [c]], 2)])) :-
		clo_span::mine(closure_sequences, clo_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]).

	test(clo_span_mine_3_prefix_ladder_closed_pattern, deterministic(memberchk(sequence_pattern([[a], [b], [c]], 2), Patterns))) :-
		clo_span::mine(prefix_ladder_sequences, clo_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]).

	test(clo_span_export_to_clauses_4, deterministic(functor(Clause, mined_patterns, 3))) :-
		clo_span::mine(closure_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(3)]),
		clo_span::export_to_clauses(closure_sequences, PatternMiner, mined_patterns, [Clause]).

	test(clo_span_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		clo_span::mine(closure_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(3)]),
		clo_span::print_pattern_miner(PatternMiner).

	test(clo_span_mine_2_invalid_dataset, error(domain_error(non_empty_itemset, []))) :-
		clo_span::mine(invalid_empty_event_sequences, _PatternMiner).

:- end_object.
