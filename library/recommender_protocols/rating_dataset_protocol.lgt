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


:- protocol(rating_dataset_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Protocol for datasets used with collaborative filtering recommendation algorithms.'
	]).

	:- public(rating/3).
	:- mode(rating(-atomic, -atomic, -number), zero_or_more).
	:- info(rating/3, [
		comment is 'Enumerates by backtracking the known ratings. ``User`` and ``Item`` must be instantiated atomic identifiers and ``Rating`` is the numeric rating given by ``User`` to ``Item``. At most one rating is declared per ``User``-``Item`` pair.',
		argnames is ['User', 'Item', 'Rating']
	]).

	:- public(rating_count/1).
	:- mode(rating_count(-positive_integer), one).
	:- info(rating_count/1, [
		comment is 'Returns the number of known ratings. The declared count must match the number of ratings enumerated by ``rating/3``.',
		argnames is ['Count']
	]).

	:- public(rating_scale/2).
	:- mode(rating_scale(-number, -number), zero_or_one).
	:- info(rating_scale/2, [
		comment is 'Returns the inclusive lower and upper bounds of the rating scale (e.g. ``1`` and ``5``). Fails when the dataset declares no fixed scale.',
		argnames is ['Min', 'Max']
	]).

:- end_protocol.
