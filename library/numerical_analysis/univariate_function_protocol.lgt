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


:- protocol(univariate_function_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Protocol for real-valued functions of one real variable.'
	]).

	:- public(evaluate/2).
	:- mode(evaluate(+number, -number), one).
	:- info(evaluate/2, [
		comment is 'Evaluates the function at the given argument.',
		argnames is ['Argument', 'Value']
	]).

	:- public(derivative/2).
	:- mode(derivative(+number, -number), zero_or_one).
	:- info(derivative/2, [
		comment is 'Optional. Evaluates the first derivative at the given argument.',
		argnames is ['Argument', 'Derivative']
	]).

:- end_protocol.
