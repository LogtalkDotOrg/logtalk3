%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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


:- object(noc_metric,
	implements(code_metric_protocol),
	imports(code_metrics_utilities)).

	:- info([
		version is 0.3,
		author is 'Ebrahim Azarisooreh',
		date is 2017/12/27,
		comment is 'Number of clauses defined for a predicate in an object or category.'
	]).

	:- uses(list, [memberchk/2]).

	entity_score(Entity, predicate_noc(Predicate, Noc)) :-
		^^current_entity(Entity),
		^^entity_kind(Entity, Kind),
		(  Kind == object
		;  Kind == category
		),
		defined_predicate_noc(Entity, Predicate, Noc).

	defined_predicate_noc(Entity, Predicate, Noc) :-
		^^defines_predicate(Entity, Predicate, Properties),
		memberchk(number_of_clauses(Noc), Properties).

	metric_label('Number of Clauses').

:- end_object.
