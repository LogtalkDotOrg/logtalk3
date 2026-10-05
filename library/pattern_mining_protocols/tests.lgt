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
		date is 2026-04-29,
		comment is 'Smoke tests for the "pattern_mining_protocols" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cleanup :-
		^^clean_file('test_output.pl').

	test(sample_pattern_miner_mine_2, deterministic(PatternMiner == sample_pattern_miner([frequent_itemset([bread], 5), frequent_itemset([bread, milk], 4)]))) :-
		sample_pattern_miner::mine(sample_dataset, PatternMiner).

	test(sample_export_to_clauses_4, deterministic(Clause == reduced([frequent_itemset([bread], 5), frequent_itemset([bread, milk], 4)]))) :-
		sample_pattern_miner::mine(sample_dataset, PatternMiner),
		sample_pattern_miner::export_to_clauses(sample_dataset, PatternMiner, reduced, [Clause]).

	test(sample_export_to_file_4, deterministic(Patterns == [frequent_itemset([bread], 5), frequent_itemset([bread, milk], 4)])) :-
		^^file_path('test_output.pl', File),
		sample_pattern_miner::mine(sample_dataset, PatternMiner),
		sample_pattern_miner::export_to_file(sample_dataset, PatternMiner, sample_pattern_result, File),
		logtalk_load(File),
		{sample_pattern_result(Patterns)}.

	test(sample_pattern_miner_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		sample_pattern_miner::mine(sample_dataset, PatternMiner),
		sample_pattern_miner::print_pattern_miner(PatternMiner).

	test(sample_pattern_miner_diagnostics_2, deterministic((memberchk(model(sample_pattern_miner), Diagnostics), memberchk(pattern_count(2), Diagnostics), memberchk(search_strategy(sample_projection), Diagnostics)))) :-
		sample_pattern_miner::mine(sample_dataset, PatternMiner),
		sample_pattern_miner::diagnostics(PatternMiner, Diagnostics).

	test(sample_pattern_miner_diagnostic_2, true) :-
		sample_pattern_miner::mine(sample_dataset, PatternMiner),
		sample_pattern_miner::diagnostic(PatternMiner, pattern_length_histogram([1-1, 2-1])).

	test(sample_pattern_miner_options_2, deterministic(Options == [])) :-
		sample_pattern_miner::mine(sample_dataset, PatternMiner),
		sample_pattern_miner::pattern_miner_options(PatternMiner, Options).

	test(sample_pattern_miner_check_pattern_miner_1, deterministic) :-
		sample_pattern_miner::mine(sample_dataset, PatternMiner),
		sample_pattern_miner::check_pattern_miner(PatternMiner).

	test(sample_pattern_miner_check_invalid_pattern_miner_1, error(domain_error(sample_pattern_miner, sample_pattern_miner([frequent_itemset([bread], foo)])))) :-
		PatternMiner = sample_pattern_miner([frequent_itemset([bread], foo)]),
		sample_pattern_miner::check_pattern_miner(PatternMiner).

	test(sample_pattern_miner_valid_pattern_miner_1, deterministic) :-
		sample_pattern_miner::mine(sample_dataset, PatternMiner),
		sample_pattern_miner::valid_pattern_miner(PatternMiner).

	test(sample_pattern_miner_invalid_pattern_miner_1, fail) :-
		PatternMiner = sample_pattern_miner([frequent_itemset([bread], foo)]),
		sample_pattern_miner::valid_pattern_miner(PatternMiner).

:- end_object.
