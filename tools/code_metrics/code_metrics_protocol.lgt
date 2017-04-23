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


:- protocol(code_metrics_protocol).

	:- info([
		version is 0.2,
		author is 'Ebrahim Azarisooreh',
		date is 2017/04/23,
		comment is 'Protocol for code_metrics tool.',
		remarks is [
			'Usage' - 'This protocol should be implemented by any metric added to the system.',
			'Score' - 'Score can be any type of term necessary to explain the nature of the entity and its relationship to the metric in question.'
		],
		see_also is [code_metrics_utilities]
	]).

	:- public(entity_score/2).
	:- mode(entity_score(?term, ?term), zero_or_more).
	:- info(entity_score/2, [
		version is 0.1,
		comment is 'True if Score is a term that represents the metric score associated with Entity.',
		argnames is ['Entity', 'Score']
	]).

:- end_protocol.
