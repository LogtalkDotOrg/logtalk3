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


:- object(deep_intersection_baskets,
	implements(transaction_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-19,
		comment is 'Compact transaction dataset with one frequent length-4 itemset and several overlapping length-3 itemsets, suitable for stressing deeper vertical tidset intersections.'
	]).

	items([alpha, beta, delta, epsilon, gamma]).

	transaction(1, [alpha, beta, delta, epsilon, gamma]).
	transaction(2, [alpha, beta, delta, epsilon, gamma]).
	transaction(3, [alpha, beta, delta, gamma]).
	transaction(4, [alpha, beta, delta, gamma]).
	transaction(5, [alpha, beta, epsilon, gamma]).
	transaction(6, [alpha, delta, epsilon, gamma]).
	transaction(7, [beta, delta, epsilon, gamma]).
	transaction(8, [alpha, beta, delta, epsilon]).

:- end_object.
