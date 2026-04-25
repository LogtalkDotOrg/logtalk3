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


:- object(regular_head_to_head,
	implements(pairwise_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	item(gamma).
	item(delta).

	preference(alpha, beta, 6).
	preference(beta, alpha, 1).
	preference(alpha, gamma, 6).
	preference(gamma, alpha, 2).
	preference(alpha, delta, 7).
	preference(delta, alpha, 2).
	preference(beta, gamma, 5).
	preference(gamma, beta, 2).
	preference(beta, delta, 6).
	preference(delta, beta, 2).
	preference(gamma, delta, 5).
	preference(delta, gamma, 2).

:- end_object.
