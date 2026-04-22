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


:- category(pattern_miner_common,
	implements(pattern_miner_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'Shared predicates for pattern miner defaults, option handling, and export helpers.'
	]).

	:- uses(format, [
		format/3
	]).

	:- uses(type, [
		valid/2
	]).

	:- protected(pattern_miner_export_template/4).
	:- mode(pattern_miner_export_template(+object_identifier, +compound, +atom, -callable), one).
	:- info(pattern_miner_export_template/4, [
		comment is 'Hook predicate that importing pattern miner implementations must define in order to expose the exported pattern miner template for a given functor.',
		argnames is ['Dataset', 'PatternMiner', 'Functor', 'Template']
	]).

	:- protected(check_item_domain/1).
	:- mode(check_item_domain(+list(atom)), one).
	:- info(check_item_domain/1, [
		comment is 'Validates that the declared item domain is canonical and contains only atoms.',
		argnames is ['ItemDomain']
	]).

	:- protected(effective_support_count/3).
	:- mode(effective_support_count(+integer, +list(compound), -integer), one).
	:- info(effective_support_count/3, [
		comment is 'Computes the effective minimum support count from the effective options and the dataset size.',
		argnames is ['DatasetSize', 'Options', 'SupportCount']
	]).

	:- protected(effective_maximum_pattern_length/3).
	:- mode(effective_maximum_pattern_length(+integer, +list(compound), -integer), one).
	:- info(effective_maximum_pattern_length/3, [
		comment is 'Computes the effective maximum pattern length from the effective options and the dataset maximum pattern length.',
		argnames is ['DatasetMaximumPatternLength', 'Options', 'MaximumPatternLength']
	]).

	mine(Dataset, PatternMiner) :-
		::mine(Dataset, PatternMiner, []).

	export_to_clauses(Dataset, PatternMiner, Functor, [Clause]) :-
		::pattern_miner_export_template(Dataset, PatternMiner, Functor, Clause).

	export_to_file(Dataset, PatternMiner, Functor, File) :-
		::export_to_clauses(Dataset, PatternMiner, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Dataset, PatternMiner, Functor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	check_item_domain(ItemDomain) :-
		sort(ItemDomain, SortedItemDomain),
		(	ItemDomain == SortedItemDomain ->
			true
		;	domain_error(canonical_item_domain, ItemDomain)
		),
		check_item_domain_atoms(ItemDomain).

	check_item_domain_atoms([]).
	check_item_domain_atoms([Item| Items]) :-
		(	atom(Item) ->
			true
		;	type_error(atom, Item)
		),
		check_item_domain_atoms(Items).

	effective_support_count(DatasetSize, Options, SupportCount) :-
		(	^^option(minimum_support_count(SupportCount), Options) ->
			true
		;	^^option(minimum_support(MinimumSupport), Options),
			SupportCount is max(1, ceiling(MinimumSupport * DatasetSize))
		).

	effective_maximum_pattern_length(DatasetMaximumPatternLength, Options, MaximumPatternLength) :-
		^^option(maximum_pattern_length(ConfiguredMaximumPatternLength), Options),
		MaximumPatternLength is min(DatasetMaximumPatternLength, ConfiguredMaximumPatternLength).

	write_comment_header(Dataset, PatternMiner, Functor, Stream) :-
		::pattern_miner_export_template(Dataset, PatternMiner, Functor, Template),
		format(Stream, '% ~q~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	default_option(minimum_support(0.5)).
	default_option(maximum_pattern_length(1000)).
	default_option(minimum_pattern_length(1)).

	valid_option(minimum_support(MinimumSupport)) :-
		number(MinimumSupport),
		MinimumSupport > 0.0,
		MinimumSupport =< 1.0.
	valid_option(minimum_support_count(MinimumSupportCount)) :-
		valid(positive_integer, MinimumSupportCount).
	valid_option(maximum_pattern_length(MaximumPatternLength)) :-
		valid(positive_integer, MaximumPatternLength).
	valid_option(minimum_pattern_length(MinimumPatternLength)) :-
		valid(positive_integer, MinimumPatternLength).

:- end_category.
