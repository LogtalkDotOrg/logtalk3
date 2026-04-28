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


:- protocol(target_supervised_dimension_reduction_dataset_protocol,
	extends(dimension_reduction_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-28,
		comment is 'Protocol for target-valued datasets used with supervised dimension reduction algorithms such as PLS projections.'
	]).

	:- public(target/1).
	:- mode(target(-atom), one).
	:- info(target/1, [
		comment is 'Returns the name of the numeric target attribute.',
		argnames is ['Target']
	]).

	:- public(example/3).
	:- mode(example(-integer, -number, -list(pair)), zero_or_more).
	:- info(example/3, [
		comment is 'Enumerates by backtracking the target-valued examples in the dataset. Each example has an ``Id``, a numeric ``Target`` value, and a list of ``Attribute-Value`` pairs.',
		argnames is ['Id', 'Target', 'AttributeValues']
	]).

:- end_protocol.
