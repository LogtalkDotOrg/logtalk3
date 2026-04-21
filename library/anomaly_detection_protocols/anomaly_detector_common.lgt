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


:- category(anomaly_detector_common,
	implements(anomaly_detector_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'Shared predicates for anomaly detector learning defaults, threshold-based prediction, dataset helpers, and export.'
	]).

	:- protected(anomaly_detector_options/2).
	:- mode(anomaly_detector_options(+compound, -list(compound)), one).
	:- info(anomaly_detector_options/2, [
		comment is 'Hook predicate that importing anomaly detector implementations may override in order to expose the effective detector options.',
		argnames is ['AnomalyDetector', 'Options']
	]).

	:- protected(anomaly_detector_export_template/2).
	:- mode(anomaly_detector_export_template(+atom, -callable), one).
	:- info(anomaly_detector_export_template/2, [
		comment is 'Hook predicate that importing anomaly detector implementations must define in order to expose the exported detector predicate template for a given functor.',
		argnames is ['Functor', 'Template']
	]).

	:- protected(anomaly_detector_term_template/2).
	:- mode(anomaly_detector_term_template(+compound, -compound), one).
	:- info(anomaly_detector_term_template/2, [
		comment is 'Hook predicate that importing anomaly detector implementations must define in order to expose the learned detector term template used by pretty-printing helpers.',
		argnames is ['AnomalyDetector', 'Template']
	]).

	:- protected(dataset_attributes/2).
	:- mode(dataset_attributes(+object_identifier, -list(pair)), one).
	:- info(dataset_attributes/2, [
		comment is 'Collects the dataset attribute declarations as `Attribute-Values` pairs.',
		argnames is ['Dataset', 'Attributes']
	]).

	:- protected(extract_scores/2).
	:- mode(extract_scores(+list, -list), one).
	:- info(extract_scores/2, [
		comment is 'Transforms `Score-Id-Class` tuples into the public `Id-Class-Score` representation.',
		argnames is ['Pairs', 'Scores']
	]).

	:- protected(print_anomaly_detector_template/1).
	:- mode(print_anomaly_detector_template(+compound), one).
	:- info(print_anomaly_detector_template/1, [
		comment is 'Prints the learned anomaly detector term template in a human-readable form.',
		argnames is ['AnomalyDetector']
	]).

	:- uses(format, [
		format/3
	]).

	:- uses(list, [
		last/2, member/2
	]).

	learn(Dataset, Detector) :-
		::learn(Dataset, Detector, []).

	predict(Detector, Instance, Prediction) :-
		::anomaly_detector_options(Detector, Options),
		::predict(Detector, Instance, Prediction, Options).

	predict(Detector, Instance, Prediction, UserOptions) :-
		^^check_options(UserOptions),
		::anomaly_detector_options(Detector, ModelOptions),
		(	member(anomaly_threshold(_), UserOptions) ->
			UpdateUserOptions = UserOptions
		;	member(anomaly_threshold(ModelThreshold), ModelOptions) ->
			UpdateUserOptions = [anomaly_threshold(ModelThreshold)| UserOptions]
		;	UpdateUserOptions = UserOptions
		),
		^^merge_options(UpdateUserOptions, Options),
		^^option(anomaly_threshold(Threshold), Options),
		::score(Detector, Instance, Score),
		(	Score >= Threshold ->
			Prediction = anomaly
		;	Prediction = normal
		).

	export_to_file(Dataset, Detector, Functor, File) :-
		::export_to_clauses(Dataset, Detector, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Dataset, Functor, Detector, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	anomaly_detector_options(Detector, Options) :-
		Detector =.. [_| Arguments],
		last(Arguments, Options).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	extract_scores([], []).
	extract_scores([Score-Id-Class| Pairs], [Id-Class-Score| Scores]) :-
		extract_scores(Pairs, Scores).

	print_anomaly_detector_template(Detector) :-
		::anomaly_detector_term_template(Detector, Template),
		format('Template:           ~w~n', [Template]).

	write_comment_header(Dataset, Functor, Detector, Stream) :-
		::anomaly_detector_export_template(Functor, Template),
		functor(Template, _, Arity),
		format(Stream, '% exported anomaly detector predicate: ~q/~d~n', [Functor, Arity]),
		format(Stream, '% training dataset: ~q~n', [Dataset]),
		::anomaly_detector_options(Detector, Options),
		format(Stream, '% options: ~q~n', [Options]),
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

:- end_category.
