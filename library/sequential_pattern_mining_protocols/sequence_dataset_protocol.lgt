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


:- protocol(sequence_dataset_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-29,
		comment is 'Protocol for ordered sequence datasets used with sequential pattern mining algorithms. Sequence identifiers are expected to be unique within a dataset.'
	]).

	:- public(items/1).
	:- mode(items(-list(atom)), one).
	:- info(items/1, [
		comment is 'Returns the declared sequence item domain as a canonical sorted list of unique items.',
		argnames is ['Items']
	]).

	:- public(sequence/2).
	:- mode(sequence(-integer, -list(list(atom))), zero_or_more).
	:- info(sequence/2, [
		comment is 'Enumerates by backtracking the sequences in the dataset. The sequence identifier is expected to be unique within the dataset. Each sequence is represented as an ordered list of canonical sorted itemsets with unique declared items.',
		argnames is ['Id', 'Sequence']
	]).

:- end_protocol.