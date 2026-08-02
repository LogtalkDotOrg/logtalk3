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


:- protocol(multiplayer_ranking_dataset_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'Protocol for ordered multiplayer ranking datasets with ranked teams and weighted player participation.'
	]).

	:- public(item/1).
	:- mode(item(-atom), zero_or_more).
	:- info(item/1, [
		comment is 'Enumerates by backtracking the items that can be ranked.',
		argnames is ['Item']
	]).

	:- public(match/1).
	:- mode(match(-atom), zero_or_more).
	:- info(match/1, [
		comment is 'Enumerates by backtracking the matches in chronological processing order.',
		argnames is ['Match']
	]).

	:- public(team/3).
	:- mode(team(-atom, -atom, -integer), zero_or_more).
	:- info(team/3, [
		comment is 'Enumerates by backtracking the teams in a match and their non-negative ranks. Lower ranks are better and equal ranks denote a draw.',
		argnames is ['Match', 'Team', 'Rank']
	]).

	:- public(team_member/4).
	:- mode(team_member(-atom, -atom, -atom, -number), zero_or_more).
	:- info(team_member/4, [
		comment is 'Enumerates by backtracking team members and their participation weights, which must be greater than zero and no greater than one.',
		argnames is ['Match', 'Team', 'Item', 'Weight']
	]).

:- end_protocol.
