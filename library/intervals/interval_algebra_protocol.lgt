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


:- protocol(interval_algebra_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-13,
		comment is 'Allen interval algebra protocol over the 13 base relation atoms.',
		see_also is [interval_protocol, interval, interval_algebra, interval_relation_set_protocol, interval_relation_set]
	]).

	:- public(relation/1).
	:- mode(relation(?atom), zero_or_more).
	:- info(relation/1, [
		comment is 'Enumerates valid Allen base relation atoms in canonical order.',
		argnames is ['Relation']
	]).

	:- public(converse/2).
	:- mode(converse(?atom, ?atom), zero_or_more).
	:- info(converse/2, [
		comment is 'Relates an Allen base relation atom with its converse relation atom.',
		argnames is ['Relation', 'Converse']
	]).

	:- public(compose/3).
	:- mode(compose(+atom, +atom, -list(atom)), zero_or_one).
	:- info(compose/3, [
		comment is 'Returns the canonical ordered list of Allen base relation atoms resulting from composing two Allen base relation atoms.',
		argnames is ['Relation1', 'Relation2', 'Relations']
	]).

:- end_protocol.
