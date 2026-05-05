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


:- protocol(pairwise_ranking_dataset_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Protocol for pairwise ranking datasets.'
	]).

	:- public(item/1).
	:- mode(item(-atom), zero_or_more).
	:- info(item/1, [
		comment is 'Enumerates by backtracking the items that can be ranked.',
		argnames is ['Item']
	]).

	:- public(preference/3).
	:- mode(preference(-atom, -atom, -number), zero_or_more).
	:- info(preference/3, [
		comment is 'Enumerates by backtracking weighted pairwise preferences where ``Winner`` is preferred over ``Loser`` ``Weight`` times.',
		argnames is ['Winner', 'Loser', 'Weight']
	]).

:- end_protocol.
