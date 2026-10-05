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

	:- uses(list, [
		memberchk/2
	]).

	mine(_Dataset, sample_pattern_miner([
		frequent_itemset([bread], 5),
		frequent_itemset([bread, milk], 4)
	]), _Options).

	pattern_miner_diagnostics_data(sample_pattern_miner(Patterns), Diagnostics) :-
		^^pattern_miner_diagnostics(sample_pattern_miner, [bread, milk], Patterns, [], [search_strategy(sample_projection)], Diagnostics).

	check_pattern_miner(PatternMiner) :-
		(	PatternMiner = sample_pattern_miner(Patterns),
			valid_patterns(Patterns),
			::pattern_miner_diagnostics_data(PatternMiner, Diagnostics),
			^^valid_pattern_miner_metadata(sample_pattern_miner, [bread, milk], Patterns, [], Diagnostics),
			memberchk(search_strategy(sample_projection), Diagnostics) ->
			true
		;	domain_error(sample_pattern_miner, PatternMiner)
		).

	valid_patterns([]).
	valid_patterns([frequent_itemset(Items, Support)| Patterns]) :-
		catch(^^check_item_domain(Items), _Error, fail),
		integer(Support),
		Support > 0,
		valid_patterns(Patterns).

	pattern_miner_export_template(_Dataset, sample_pattern_miner(Patterns), Functor, Template) :-
		Template =.. [Functor, Patterns].

	print_pattern_miner(PatternMiner) :-
		writeq(PatternMiner), nl.

:- end_object.
