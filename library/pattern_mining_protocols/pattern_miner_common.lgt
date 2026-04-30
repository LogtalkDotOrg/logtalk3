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
		date is 2026-04-29,
		comment is 'Shared predicates for pattern miner diagnostics, defaults, option handling, and export helpers.'
	]).

	:- protected(pattern_miner_diagnostics_data/2).
	:- mode(pattern_miner_diagnostics_data(+compound, -list(compound)), one).
	:- info(pattern_miner_diagnostics_data/2, [
		comment is 'Hook predicate that importing pattern miner implementations must define in order to expose diagnostics metadata.',
		argnames is ['PatternMiner', 'Diagnostics']
	]).

	:- protected(pattern_miner_diagnostics/6).
	:- mode(pattern_miner_diagnostics(+atom, +list(atom), +list(compound), +list(compound), +list(compound), -list(compound)), one).
	:- info(pattern_miner_diagnostics/6, [
		comment is 'Builds a standard diagnostics list from generic pattern miner metadata and algorithm-specific diagnostics terms.',
		argnames is ['Model', 'ItemDomain', 'Patterns', 'Options', 'SpecificDiagnostics', 'Diagnostics']
	]).

	:- protected(pattern_miner_export_template/4).
	:- mode(pattern_miner_export_template(+object_identifier, +compound, +atom, -callable), one).
	:- info(pattern_miner_export_template/4, [
		comment is 'Hook predicate that importing pattern miner implementations must define in order to expose the exported pattern miner template for a given functor.',
		argnames is ['Dataset', 'PatternMiner', 'Functor', 'Template']
	]).

	:- protected(valid_pattern_miner_metadata/5).
	:- mode(valid_pattern_miner_metadata(+atom, +list(atom), +list(compound), +list(compound), +list(compound)), zero_or_one).
	:- info(valid_pattern_miner_metadata/5, [
		comment is 'True when the diagnostics list matches the generic pattern miner metadata implied by the model, item domain, mined patterns, and effective options.',
		argnames is ['Model', 'ItemDomain', 'Patterns', 'Options', 'Diagnostics']
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

	:- uses(format, [
		format/3
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	mine(Dataset, PatternMiner) :-
		::mine(Dataset, PatternMiner, []).

	diagnostics(PatternMiner, Diagnostics) :-
		::pattern_miner_diagnostics_data(PatternMiner, Diagnostics).

	diagnostic(PatternMiner, Diagnostic) :-
		diagnostics(PatternMiner, Diagnostics),
		member(Diagnostic, Diagnostics).

	pattern_miner_options(PatternMiner, Options) :-
		diagnostics(PatternMiner, Diagnostics),
		memberchk(options(Options), Diagnostics).

	check_pattern_miner(PatternMiner) :-
		(	var(PatternMiner) ->
			instantiation_error
		;   ::pattern_miner_diagnostics_data(PatternMiner, _Diagnostics) ->
			true
		;   domain_error(pattern_miner, PatternMiner)
		).

	valid_pattern_miner(PatternMiner) :-
		catch(::check_pattern_miner(PatternMiner), _Error, fail).

	valid_pattern_miner_metadata(Model, ItemDomain, Patterns, Options, Diagnostics) :-
		catch(::check_item_domain(ItemDomain), _Error, fail),
		valid(list(compound), Patterns),
		valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(options(Options), Diagnostics),
		catch(::check_options(Options), _Error, fail),
		length(ItemDomain, ItemDomainSize),
		memberchk(item_domain_size(ItemDomainSize), Diagnostics),
		length(Patterns, PatternCount),
		memberchk(pattern_count(PatternCount), Diagnostics),
		pattern_length_histogram(Patterns, Histogram),
		memberchk(pattern_length_histogram(Histogram), Diagnostics),
		pattern_support_range(Patterns, MinimumSupport, MaximumSupport),
		memberchk(support_range(MinimumSupport, MaximumSupport), Diagnostics).

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

	pattern_miner_diagnostics(Model, ItemDomain, Patterns, Options, SpecificDiagnostics, Diagnostics) :-
		length(ItemDomain, ItemDomainSize),
		length(Patterns, PatternCount),
		pattern_length_histogram(Patterns, Histogram),
		pattern_support_range(Patterns, MinimumSupport, MaximumSupport),
		Diagnostics = [
			model(Model),
			options(Options),
			item_domain_size(ItemDomainSize),
			pattern_count(PatternCount),
			pattern_length_histogram(Histogram),
			support_range(MinimumSupport, MaximumSupport)
			| SpecificDiagnostics
		].

	pattern_length_histogram(Patterns, Histogram) :-
		pattern_length_histogram(Patterns, [], Histogram0),
		sort(Histogram0, Histogram).

	pattern_length_histogram([], Histogram, Histogram).
	pattern_length_histogram([PatternTerm| PatternTerms], Histogram0, Histogram) :-
		pattern_term_length(PatternTerm, PatternLength),
		increment_pattern_length_count(Histogram0, PatternLength, Histogram1),
		pattern_length_histogram(PatternTerms, Histogram1, Histogram).

	increment_pattern_length_count([], PatternLength, [PatternLength-1]).
	increment_pattern_length_count([PatternLength-Count0| Histogram], PatternLength, [PatternLength-Count| Histogram]) :-
		!,
		Count is Count0 + 1.
	increment_pattern_length_count([Entry| Histogram0], PatternLength, [Entry| Histogram]) :-
		increment_pattern_length_count(Histogram0, PatternLength, Histogram).

	pattern_support_range([], 0, 0).
	pattern_support_range([PatternTerm| PatternTerms], MinimumSupport, MaximumSupport) :-
		arg(2, PatternTerm, Support),
		pattern_support_range(PatternTerms, Support, Support, MinimumSupport, MaximumSupport).

	pattern_support_range([], MinimumSupport, MaximumSupport, MinimumSupport, MaximumSupport).
	pattern_support_range([PatternTerm| PatternTerms], MinimumSupport0, MaximumSupport0, MinimumSupport, MaximumSupport) :-
		arg(2, PatternTerm, Support),
		MinimumSupport1 is min(MinimumSupport0, Support),
		MaximumSupport1 is max(MaximumSupport0, Support),
		pattern_support_range(PatternTerms, MinimumSupport1, MaximumSupport1, MinimumSupport, MaximumSupport).

	pattern_term_length(PatternTerm, PatternLength) :-
		arg(1, PatternTerm, Pattern),
		pattern_items_length(Pattern, 0, PatternLength).

	pattern_items_length([], PatternLength, PatternLength).
	pattern_items_length([Item| Items], PatternLength0, PatternLength) :-
		(   Item = [_| _] ->
			pattern_items_length(Item, 0, ItemLength)
		;   ItemLength = 1
		),
		PatternLength1 is PatternLength0 + ItemLength,
		pattern_items_length(Items, PatternLength1, PatternLength).

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
