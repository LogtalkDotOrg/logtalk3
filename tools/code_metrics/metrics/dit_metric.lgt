%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
%                 Paulo Moura         <pmoura@logtalk.org>
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


:- object(dit_metric,
	implements(code_metrics_protocol),
	imports(code_metrics_utilities)).

	:- info([
		version is 0.1,
		author is 'Ebrahim Azarisooreh',
		date is 2017/1/2,
		comment is 'Analyzes the depth of inheritance for objects, protocols, and categories.',
		remarks is [
			'Depth'-'The depth is the maximum length of a node to the root entity. Lower scores are generally better.',
			'Inheritance'-'A level of inheritance defined by either one of specialization, instantiation, extension, importation, or implementation.',
			'Scoring'-'The maximum path length is determined for each entity in question.'
		]
	]).

	:- uses(list, [max/2]).

	item_score(Entity, Score) :-
		^^current_entity(Entity),
		depth(Entity, Score).

	depth(Entity, Depth) :-
		^^entity_kind(Entity, EntityKind),
		setof(D, depth(EntityKind, Entity, 1, D), Depths),
		max(Depths, Depth).

	depth(EntityKind, Entity, Depth0, Depth) :-
		^^ancestor(EntityKind, Entity, AncestorKind, Ancestor),
		Depth1 is Depth0 + 1,
		depth(AncestorKind, Ancestor, Depth1, Depth).

	depth(EntityKind, Entity, Depth, Depth) :-
		\+ ^^ancestor(EntityKind, Entity, _, _).

:- end_object.
