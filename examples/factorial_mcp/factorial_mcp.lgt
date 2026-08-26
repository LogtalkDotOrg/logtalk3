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


:- object(factorial_mcp,
    implements(mcp_tool_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-26,
		comment is 'MCP tool provider for computing the factorial of a number.'
	]).

    :- public(factorial/2).
    :- mode(factorial(+integer, -integer), one).
    :- info(factorial/2, [
        comment is 'Computes the factorial of a non-negative integer.',
        argnames is ['N', 'F']
    ]).

    tools([
        tool(factorial, factorial, 2)
    ]).

    factorial(0, 1) :-
		!.
    factorial(N, F) :-
        N > 0,
        N1 is N - 1,
        factorial(N1, F1),
        F is N * F1.

:- end_object.
