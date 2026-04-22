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


:- object(sample_dataset).

:- end_object.


:- object(sample_pattern_miner,
	imports(pattern_miner_common)).

	:- public(mine/3).
	:- mode(mine(+object_identifier, -compound, +list(compound)), one).

	mine(_Dataset, sample_pattern_miner([
		frequent_itemset([bread], 5),
		frequent_itemset([bread, milk], 4)
	]), _Options).

	pattern_miner_export_template(_Dataset, sample_pattern_miner(Patterns), Functor, Template) :-
		Template =.. [Functor, Patterns].

	print_pattern_miner(PatternMiner) :-
		writeq(PatternMiner), nl.

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
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

:- end_object.
