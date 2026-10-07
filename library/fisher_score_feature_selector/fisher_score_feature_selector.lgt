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


:- object(fisher_score_feature_selector,
	imports(filter_feature_selector_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Selects continuous features for categorical targets using Fisher scores, top-k selection, or a score threshold.'
	]).

	:- private(check_declarations/1).
	:- mode(check_declarations(+list(pair)), one_or_error).
	:- info(check_declarations/1, [
		comment is 'Checks that every feature is declared continuous.',
		argnames is ['Declarations'],
		exceptions is [
			'A feature is not declared continuous' - domain_error(feature_type, 'Feature-Declaration')
		]
	]).

	filter_model(fisher_score_feature_selector).

	filter_scoring_metric(_Options, fisher_score).

	filter_validate_dataset(Dataset, _Features, _Examples) :-
		findall(Feature-Declaration, Dataset::attribute_values(Feature, Declaration), Declarations),
		check_declarations(Declarations).

	check_declarations([]).
	check_declarations([Feature-Declaration| Declarations]) :-
		(	Declaration == continuous ->
			true
		;	domain_error(feature_type, Feature-Declaration)
		),
		check_declarations(Declarations).

:- end_object.
