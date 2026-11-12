%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(problog_problem_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-12,
		comment is 'Protocol for ordered, ground ProbLog statement providers.'
	]).

	:- public(problog_statement/2).
	:- mode(problog_statement(-compound, -term), zero_or_more).
	:- info(problog_statement/2, [
		comment is 'Enumerates canonical ProbLog statements in source order together with their source origins.',
		argnames is ['Statement', 'Origin'],
		remarks is [
			'Statements' - 'Statements are fully ground and use the vocabulary documented by the ``problog_program`` object.',
			'Origin' - 'Either ``unknown`` or ``source(File, Start, End)`` where positions are positive character offsets.'
		]
	]).

:- end_protocol.
