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


:- category(ranker_common,
	implements(ranker_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'Shared predicates for ranker score access, diagnostics, and export.'
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(avltree, [
		insert/4 as dictionary_insert/4,
		lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- protected(ranker_scores_data/2).
	:- mode(ranker_scores_data(+compound, -list(pair)), one).
	:- info(ranker_scores_data/2, [
		comment is 'Hook predicate that importing ranker implementations must define in order to expose the learned item-score pairs on the ranker native numeric scale.',
		argnames is ['Ranker', 'Scores']
	]).

	:- protected(ranker_diagnostics_data/2).
	:- mode(ranker_diagnostics_data(+compound, -list(compound)), one).
	:- info(ranker_diagnostics_data/2, [
		comment is 'Hook predicate that importing ranker implementations must define in order to expose diagnostics metadata.',
		argnames is ['Ranker', 'Diagnostics']
	]).

	:- protected(ranker_export_template/4).
	:- mode(ranker_export_template(+object_identifier, +compound, +atom, -callable), one).
	:- info(ranker_export_template/4, [
		comment is 'Hook predicate that importing ranker implementations must define in order to expose the exported ranker template for a given functor.',
		argnames is ['Dataset', 'Ranker', 'Functor', 'Template']
	]).

	:- protected(ranker_term_template/2).
	:- mode(ranker_term_template(+compound, -callable), one).
	:- info(ranker_term_template/2, [
		comment is 'Hook predicate that importing ranker implementations must define in order to expose the learned ranker term template used by pretty-printing helpers.',
		argnames is ['Ranker', 'Template']
	]).

	:- protected(print_ranker_template/1).
	:- mode(print_ranker_template(+compound), one).
	:- info(print_ranker_template/1, [
		comment is 'Pretty-printing helper predicate used by importing ranker implementations to show the learned ranker term template.',
		argnames is ['Ranker']
	]).

	:- protected(valid_ranker_metadata/2).
	:- mode(valid_ranker_metadata(+atom, +list(compound)), zero_or_one).
	:- info(valid_ranker_metadata/2, [
		comment is 'True when the diagnostics list contains the expected model identifier, a valid effective options list, and a structurally valid dataset summary.',
		argnames is ['Model', 'Diagnostics']
	]).

	:- protected(valid_item_value_pairs/2).
	:- mode(valid_item_value_pairs(+list, +list(pair)), zero_or_one).
	:- info(valid_item_value_pairs/2, [
		comment is 'True when the second argument is an ordered list of unique ``Item-Value`` pairs aligned with the first argument item list.',
		argnames is ['Items', 'Pairs']
	]).

	scores(Ranker, Scores) :-
		::ranker_scores_data(Ranker, Scores).

	diagnostics(Ranker, Diagnostics) :-
		::ranker_diagnostics_data(Ranker, Diagnostics).

	diagnostic(Ranker, Diagnostic) :-
		diagnostics(Ranker, Diagnostics),
		member(Diagnostic, Diagnostics).

	ranker_options(Ranker, Options) :-
		diagnostics(Ranker, Diagnostics),
		memberchk(options(Options), Diagnostics).

	check_ranker(Ranker) :-
		(	var(Ranker) ->
			instantiation_error
		;	::ranker_scores_data(Ranker, _Scores),
			::ranker_diagnostics_data(Ranker, _Diagnostics) ->
			true
		;	domain_error(ranker, Ranker)
		).

	valid_ranker(Ranker) :-
		catch(::check_ranker(Ranker), _Error, fail).

	print_ranker_template(Ranker) :-
		::ranker_term_template(Ranker, Template),
		format('Template: ~w~n', [Template]).

	valid_ranker_metadata(Model, Diagnostics) :-
		type::valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(options(Options), Diagnostics),
		catch(::check_options(Options), _Error, fail),
		memberchk(dataset_summary(Summary), Diagnostics),
		type::valid(list(compound), Summary).

	valid_item_value_pairs(Items, Pairs) :-
		dictionary_new(SeenItems),
		valid_item_value_pairs(Items, Pairs, SeenItems).

	valid_item_value_pairs(Items, Pairs, _SeenItems) :-
		(	var(Items); var(Pairs) ),
		!,
		fail.
	valid_item_value_pairs([], [], _SeenItems).
	valid_item_value_pairs([Item| Items], [PairItem-_Value| Pairs], SeenItems0) :-
		nonvar(Item),
		Item == PairItem,
		\+ dictionary_lookup(Item, _Seen, SeenItems0),
		dictionary_insert(SeenItems0, Item, true, SeenItems),
		valid_item_value_pairs(Items, Pairs, SeenItems).

	export_to_file(Dataset, Ranker, Functor, File) :-
		::export_to_clauses(Dataset, Ranker, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Dataset, Functor, Ranker, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_comment_header(Dataset, Functor, Ranker, Stream) :-
		::ranker_export_template(Dataset, Ranker, Functor, Template),
		functor(Template, _, Arity),
		format(Stream, '% exported ranker predicate: ~q/~d~n', [Functor, Arity]),
		format(Stream, '% training dataset: ~q~n', [Dataset]),
		(	::diagnostics(Ranker, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;	true
		),
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

:- end_category.
