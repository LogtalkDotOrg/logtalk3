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


:- object(search_results,
	implements(ranking_dataset_protocol)).

	group(query_one).
	group(query_two).

	item(query_one, doc_alpha).
	item(query_one, doc_beta).
	item(query_one, doc_gamma).
	item(query_two, doc_delta).
	item(query_two, doc_epsilon).
	item(query_two, doc_zeta).

	relevance(query_one, doc_alpha, 3).
	relevance(query_one, doc_beta, 2).
	relevance(query_one, doc_gamma, 0).
	relevance(query_two, doc_delta, 2).
	relevance(query_two, doc_epsilon, 1).
	relevance(query_two, doc_zeta, 0).

:- end_object.
