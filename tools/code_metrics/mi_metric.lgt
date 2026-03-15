%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2018-2026 Paulo Moura <pmoura@logtalk.org>
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


:- if(current_object(lines_metric)).

:- object(mi_metric(_Stroud),
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-14,
		comment is 'Computes the maintainability index metric based on the Cyclomatic Complexity, Halstead Volume, and lines of code metrics for an entity.',
		parameters is [
			'Stroud' - 'Halstead metric coefficient for computing the time required to program.'
		],
		remarks is [
			'Formula' - 'The original equation is used: ``MI = 171 - 5.2 * ln(V) - 0.23 * C - 16.2 * ln(L)``. V is the Halstead volume, C is the Cyclomatic Complexity, and L is the number of code lines.',
			'Entity score' - 'Represented as the compound term ``mi(MI)``.'
		]
	]).

	entity_score(Entity, mi(MI)) :-
		parameter(1, Stroud),
		(	var(Stroud) ->
			Stroud = 18
		;	true
		),
		(	var(Entity) ->
			^^current_entity(Entity)
		;	true
		),
		^^entity_kind(Entity, Kind),
		Kind \== protocol,
		halstead_metric(Stroud)::entity_score(Entity, pn_pan_cn_can_ev_el_v_d_e_t_b(_, _, _, _, _, _, V, _, _, _, _)),
		cc_metric::entity_score(Entity, C),
		lines_metric::entity_score(Entity, lines(L, _, _)),
		V > 0,
		L > 0,
		catch(
			MI is 171 - 5.2 * log(V) - 0.23 * C - 16.2 * log(L),
			error(evaluation_error(_), _),
			fail
		).

	format_entity_score(_Entity, mi(MI)) -->
		['Maintainability index: ~f'-[MI], nl].

:- end_object.


:- object(mi_metric,
	extends(mi_metric(18))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-14,
		comment is 'Computes the maintainability index metric based on the Cyclomatic Complexity and the Halstead Volume metrics an entity using a Stroud of 18.'
	]).

:- end_object.

:- endif.
