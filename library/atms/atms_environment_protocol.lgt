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


:- protocol(atms_environment_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-26,
		comment is 'Protocol for the opaque environment representation used by the ATMS library.',
		see_also is [atms, atms_ordered_list_environment, atms_bitset_environment, atms_segmented_bitset_environment]
	]).

	:- public(empty/1).
	:- mode(empty(-atms_environment), one).
	:- info(empty/1, [
		comment is 'Returns the empty environment.',
		argnames is ['Environment']
	]).

	:- public(singleton/2).
	:- mode(singleton(+atms_node, -atms_environment), one).
	:- info(singleton/2, [
		comment is 'Returns the environment containing only the given node.',
		argnames is ['Node', 'Environment']
	]).

	:- public(union/3).
	:- mode(union(+atms_environment, +atms_environment, -atms_environment), one).
	:- info(union/3, [
		comment is 'Returns the union of two environments.',
		argnames is ['Environment1', 'Environment2', 'Union']
	]).

	:- public(subset/2).
	:- mode(subset(+atms_environment, +atms_environment), zero_or_one).
	:- info(subset/2, [
		comment is 'True iff the first environment is a subset of the second environment.',
		argnames is ['Environment1', 'Environment2']
	]).

	:- public(equal/2).
	:- mode(equal(+atms_environment, +atms_environment), zero_or_one).
	:- info(equal/2, [
		comment is 'True iff the two environments are equal.',
		argnames is ['Environment1', 'Environment2']
	]).

	:- public(from_list/2).
	:- mode(from_list(+list(atms_node), -atms_environment), one).
	:- info(from_list/2, [
		comment is 'Converts a list of node identifiers to the canonical internal environment representation.',
		argnames is ['Nodes', 'Environment']
	]).

	:- public(to_list/2).
	:- mode(to_list(+atms_environment, -list(atms_node)), one).
	:- info(to_list/2, [
		comment is 'Converts an internal environment to an ordered list of node identifiers.',
		argnames is ['Environment', 'Nodes']
	]).

:- end_protocol.
