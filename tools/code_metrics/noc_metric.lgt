%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
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
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 0.7,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2018/02/01,
		comment is 'Number of clauses defined for a predicate in an object or category.'
	]).

	entity_score(Entity, TotalNocs) :-
		^^current_entity(Entity),
		^^entity_kind(Entity, Kind),
		Kind \== protocol,
		findall(
			Noc,
			defined_predicate_noc(Entity, _, Noc),
			Nocs
		),
		numberlist::sum(Nocs, TotalNocs).

	defined_predicate_noc(Entity, Predicate, Noc) :-
		^^defines_predicate(Entity, Predicate, Properties),
		list::memberchk(number_of_clauses(Noc), Properties).

	entity_score(_Entity, TotalNocs) -->
		['Number of Clauses: ~w'-[TotalNocs], nl].

:- end_object.
