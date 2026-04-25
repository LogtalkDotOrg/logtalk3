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


:- object(large_ranked_ballots,
	implements(ranking_dataset_protocol)).

	group(ballot(Group)) :-
		integer_range(1, 200, Group).

	item(ballot(Group), item(Item)) :-
		integer_range(1, 200, Group),
		integer_range(1, 60, Item).

	relevance(ballot(Group), item(Item), Relevance) :-
		integer_range(1, 200, Group),
		integer_range(1, 60, Item),
		Relevance is 60 - Item.

	integer_range(Low, High, Low) :-
		Low =< High.
	integer_range(Low, High, Integer) :-
		Low < High,
		Next is Low + 1,
		integer_range(Next, High, Integer).

:- end_object.
