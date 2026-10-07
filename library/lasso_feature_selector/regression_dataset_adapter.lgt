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


:- object(regression_dataset_adapter(_Dataset_),
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Transient regression view of a feature dataset, preserving missing features and excluding unknown targets.',
		parameters is [
			'Dataset' - 'Feature dataset object.'
		]
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- private(check_declarations/1).
	:- mode(check_declarations(+list(pair)), one_or_error).
	:- info(check_declarations/1, [
		comment is 'Checks distinct atom feature names and continuous or non-empty, distinct atomic categorical domains.',
		argnames is ['Declarations'],
		exceptions is [
			'A feature declaration is unsupported' - domain_error(feature_type, 'Feature-Values'),
			'A feature is declared more than once' - domain_error(duplicate_feature, 'Feature')
		]
	]).

	:- private(distinct_values/1).
	:- mode(distinct_values(+list(atomic)), zero_or_one).
	:- info(distinct_values/1, [
		comment is 'True when categorical values are pairwise distinct.',
		argnames is ['Values']
	]).

	attribute_values(Feature, Values) :-
		parameter(1, Dataset),
		findall(Name-Domain, Dataset::attribute_values(Name, Domain), Declarations),
		check_declarations(Declarations),
		member(Feature-Values, Declarations).

	target(target).

	example(Id, Target, Pairs) :-
		parameter(1, Dataset),
		Dataset::example(Id, Pairs, Target),
		nonvar(Target).

	check_declarations([]).
	check_declarations([Feature-Values| Declarations]) :-
		(	atom(Feature),
			(	Values == continuous
			;	valid(list(atomic), Values),
				Values \== [],
				distinct_values(Values)
			) ->
			true
		; domain_error(feature_type, Feature-Values)
		),
		(	member(Feature-_, Declarations) ->
			domain_error(duplicate_feature, Feature)
		;	true
		),
		check_declarations(Declarations).

	distinct_values([]).
	distinct_values([Value| Values]) :-
		\+ member(Value, Values),
		distinct_values(Values).

:- end_object.
