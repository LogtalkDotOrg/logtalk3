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


:- object(code_metrics,
	imports(code_metric)).

	:- info([
		version is 0.1,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2017/12/31,
		comment is 'Helper object to apply all loaded code metrics.'
	]).

 	process_entity(Kind, Entity) :-
 		logtalk::print_message(information, code_metrics, scanning_entity(Kind, Entity)),
		forall(
			process_entity_(Entity, Metric, Score),
			logtalk::print_message(information, code_metrics, entity_score(Entity, Metric, Score))
		).

	process_entity_(Entity, Metric, Score) :-
		imports_category(Metric, code_metric),
		Metric::entity_score(Entity, Score).

:- end_object.
