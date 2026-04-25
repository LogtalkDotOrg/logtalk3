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


:- object(ranked_ballots,
	implements(ranking_dataset_protocol)).

	group(ballot_one).
	group(ballot_two).

	item(ballot_one, alpha).
	item(ballot_one, beta).
	item(ballot_one, gamma).

	item(ballot_two, alpha).
	item(ballot_two, beta).
	item(ballot_two, gamma).

	relevance(ballot_one, alpha, 2).
	relevance(ballot_one, beta, 1).
	relevance(ballot_one, gamma, 0).

	relevance(ballot_two, alpha, 0).
	relevance(ballot_two, beta, 2).
	relevance(ballot_two, gamma, 1).

:- end_object.