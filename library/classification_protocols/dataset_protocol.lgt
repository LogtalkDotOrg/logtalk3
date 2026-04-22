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


:- protocol(dataset_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Protocol for datasets used with classifier algorithms.'
	]).

	:- public(attribute_values/2).
	:- mode(attribute_values(?atom, -list(atom)), zero_or_more).
	:- mode(attribute_values(?atom, -atom), zero_or_more).
	:- info(attribute_values/2, [
		comment is 'Enumerates by backtracking the attributes and their possible values. For discrete attributes, ``Values`` is a list of possible values. For continuous (numeric) attributes, ``Values`` is the atom ``continuous``.',
		argnames is ['Attribute', 'Values']
	]).

	:- public(class/1).
	:- mode(class(-atom), one).
	:- info(class/1, [
		comment is 'Returns the name of the target class attribute.',
		argnames is ['Class']
	]).

	:- public(class_values/1).
	:- mode(class_values(-list(atom)), one).
	:- info(class_values/1, [
		comment is 'Returns the list of possible values for the target class attribute.',
		argnames is ['Values']
	]).

	:- public(example/3).
	:- mode(example(-integer, -atom, -list(pair)), zero_or_more).
	:- info(example/3, [
		comment is 'Enumerates by backtracking the examples in the dataset. Each example has an ``Id``, a ``Class`` value, and a list of ``Attribute-Value`` pairs.',
		argnames is ['Id', 'Class', 'AttributeValues']
	]).

:- end_protocol.
