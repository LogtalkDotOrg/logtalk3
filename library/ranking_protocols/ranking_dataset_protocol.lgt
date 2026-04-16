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


:- protocol(ranking_dataset_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-16,
		comment is 'Protocol for grouped ranking datasets.'
	]).

	:- public(group/1).
	:- mode(group(-atom), zero_or_more).
	:- info(group/1, [
		comment is 'Enumerates by backtracking the ranking groups or contexts in the dataset.',
		argnames is ['Group']
	]).

	:- public(item/2).
	:- mode(item(+atom, -atom), zero_or_more).
	:- mode(item(-atom, -atom), zero_or_more).
	:- info(item/2, [
		comment is 'Enumerates by backtracking the items available inside each ranking group or context.',
		argnames is ['Group', 'Item']
	]).

	:- public(relevance/3).
	:- mode(relevance(+atom, +atom, -integer), zero_or_more).
	:- mode(relevance(-atom, -atom, -integer), zero_or_more).
	:- info(relevance/3, [
		comment is 'Enumerates by backtracking the non-negative integer relevance judgments associated with each group and item.',
		argnames is ['Group', 'Item', 'Relevance']
	]).

:- end_protocol.
