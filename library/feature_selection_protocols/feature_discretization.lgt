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


:- category(feature_discretization,
	extends(feature_scoring_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Typed preparation of categorical columns with per-feature or joint complete-case discretization.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, append/3
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- protected(valid_feature_discretization/1).
	:- mode(valid_feature_discretization(@term), zero_or_one).
	:- info(valid_feature_discretization/1, [
		comment is 'True for a ground categorical, equal-width, or equal-frequency specification with a positive integer bin count.',
		argnames is ['Specification']
	]).

	:- protected(prepare_feature_columns/7).
	:- mode(prepare_feature_columns(+object_identifier, +list(atomic), +list(compound), +list(compound), +atom, -list(compound), -list(compound)), one_or_error).
	:- info(prepare_feature_columns/7, [
		comment is 'Prepares ``column(Feature,Specification,Pairs,CategoryCount)`` terms in declaration order. Mode is ``per_feature`` or ``joint``; fitting uses only complete observations.',
		argnames is ['Dataset', 'Features', 'Examples', 'Options', 'Mode', 'Columns', 'Diagnostics'],
		exceptions is [
			'A feature declaration is unsupported' - domain_error(feature_type, 'Feature-Declaration'),
			'A categorical feature value is outside its declared domain' - domain_error(feature_value, 'Feature-Value'),
			'An override names an undeclared feature' - domain_error(unknown_feature, 'Feature'),
			'The preparation mode is unsupported' - domain_error(preparation_mode, 'Mode'),
			'A continuous feature value is not numeric' - type_error(number, 'Value'),
			'A categorical value or target is not atomic' - type_error(atomic, 'Value'),
			'A declaration or configuration is unbound' - instantiation_error,
			'A declaration domain is not a list' - type_error(list, 'Domain'),
			'A binning specification is invalid' - domain_error(discretization, 'Specification')
		]
	]).

	valid_feature_discretization(Specification) :-
		ground(Specification),
		(	Specification == categorical ->
			true
		;	(Specification = equal_width(Count); Specification = equal_frequency(Count)) ->
			integer(Count),
			Count > 0
		;	fail
		).

	prepare_feature_columns(Dataset, Features, Examples, Options, Mode, Columns, Diagnostics) :-
		context(Context),
		findall(Feature-Declaration, Dataset::attribute_values(Feature, Declaration), Declarations),
		preparation_descriptors(Features, Declarations, Context, Descriptors),
		preparation_overrides(Options, Features),
		preparation_rows(Examples, Features, Rows0),
		(	Mode == joint ->
			preparation_joint_rows(Rows0, Rows)
		;	(	Mode == per_feature ->
				Rows = Rows0
			;	domain_error(preparation_mode, Mode)
			)
		),
		preparation_columns(Descriptors, Rows, Options, Context, Columns, Counts, Configurations, Occupied),
		Diagnostics0 = [complete_cases(Counts), discretization(Configurations), occupied_categories(Occupied), preparation_mode(Mode)],
		(	Mode == joint ->
			length(Rows, Used),
			length(Examples, Total),
			Excluded is Total - Used,
			append(Diagnostics0, [usable_example_count(Used), excluded_example_count(Excluded)], Diagnostics)
		;	Diagnostics = Diagnostics0
		).

	preparation_descriptors([], _Declarations, _Context, []).
	preparation_descriptors([Feature| Features], Declarations, Context, [Feature-Declaration| Descriptors]) :-
		memberchk(Feature-Declaration, Declarations),
		(	var(Declaration) ->
			instantiation_error
		;	(	Declaration == continuous ->
				true
			;	(	Declaration = [_| _] ->
					check(list(atomic), Declaration, Context)
				;	domain_error(feature_type, Feature-Declaration)
				)
			)
		),
		preparation_descriptors(Features, Declarations, Context, Descriptors).

	preparation_overrides([], _Features).
	preparation_overrides([Option| Options], Features) :-
		(	Option = feature_discretization(Feature, Specification) ->
			(	member(Feature, Features) ->
				true
			;	domain_error(unknown_feature, Feature)
			),
			preparation_specification(Specification)
		;	(	Option = discretization(Specification) ->
				preparation_specification(Specification)
			;	true
			)
		),
		preparation_overrides(Options, Features).

	preparation_specification(Specification) :-
		(	\+ ground(Specification) ->
			instantiation_error
		;	(	valid_feature_discretization(Specification) ->
				true
			;	domain_error(discretization, Specification)
			)
		).

	preparation_rows([], _Features, []).
	preparation_rows([example(_Id, Pairs, Target)| Examples], Features, [row(Target, Values)| Rows]) :-
		avltree::new(Empty),
		preparation_dictionary(Pairs, Empty, Dictionary),
		preparation_values(Features, Dictionary, Values),
		preparation_rows(Examples, Features, Rows).

	preparation_dictionary([], Dictionary, Dictionary).
	preparation_dictionary([Feature-Value| Pairs], Dictionary0, Dictionary) :-
		avltree::insert(Dictionary0, Feature, Value, Dictionary1),
		preparation_dictionary(Pairs, Dictionary1, Dictionary).

	preparation_values([], _Dictionary, []).
	preparation_values([Feature| Features], Dictionary, [Value| Values]) :-
		(	avltree::lookup(Feature, Found, Dictionary) ->
			Value = Found
		;	true
		),
		preparation_values(Features, Dictionary, Values).

	preparation_joint_rows([], []).
	preparation_joint_rows([row(Target, Values)| Rows], Complete) :-
		(	nonvar(Target),
			preparation_bound_values(Values) ->
			Complete = [row(Target, Values)| Rest]
		;	Complete = Rest
		),
		preparation_joint_rows(Rows, Rest).

	preparation_bound_values([]).
	preparation_bound_values([Value| Values]) :-
		nonvar(Value),
		preparation_bound_values(Values).

	preparation_columns([], _Rows, _Options, _Context, [], [], [], []).
	preparation_columns([Feature-Declaration| Descriptors], Rows, Options, Context,
		[column(Feature, Specification, Pairs, CategoryCount)| Columns],
		[Feature-Count| Counts], [Feature-Specification| Configurations], [Feature-CategoryCount| Occupied]) :-
		preparation_take_column(Rows, Values, Targets, RemainingRows),
		preparation_check_values(Values, Targets, Feature, Declaration, Context),
		preparation_configuration(Feature, Declaration, Options, Specification),
		^^categorical_pairs(Specification, Values, Targets, Pairs),
		length(Pairs, Count),
		^^contingency_counts(Pairs, contingency(_Total, Categories, _Targets, _Cells)),
		avltree::size(Categories, CategoryCount),
		preparation_columns(Descriptors, RemainingRows, Options, Context, Columns, Counts, Configurations, Occupied).

	preparation_take_column([], [], [], []).
	preparation_take_column([row(Target, [Value| Values])| Rows], [Value| Column], [Target| Targets], [row(Target, Values)| Rest]) :-
		preparation_take_column(Rows, Column, Targets, Rest).

	preparation_check_values([], [], _Feature, _Declaration, _Context).
	preparation_check_values([Value| Values], [Target| Targets], Feature, Declaration, Context) :-
		(	nonvar(Value),
			nonvar(Target) ->
			check(atomic, Target, Context),
			(	Declaration == continuous ->
				check(number, Value, Context)
			;	check(atomic, Value, Context),
				(	member(Value, Declaration) ->
					true
				;	domain_error(feature_value, Feature-Value)
				)
			)
		;	true
		),
		preparation_check_values(Values, Targets, Feature, Declaration, Context).

	preparation_configuration(Feature, Declaration, Options, Specification) :-
		(	member(feature_discretization(Feature, Override), Options) ->
			Specification = Override
		;	(	Declaration == continuous ->
				(	member(discretization(Configured), Options) ->
					Specification = Configured
				;	Specification = equal_frequency(10)
				)
			;	Specification = categorical
			)
		).

:- end_category.
