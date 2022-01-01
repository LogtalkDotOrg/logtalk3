%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(uuid_protocol).

	:- info([
		version is 0:3:0,
		author is 'Paulo Moura',
		date is 2021-03-13,
		comment is 'Universally unique identifier (UUID) generator protocol.'
	]).

	:- public(uuid_v1/2).
	:- mode(uuid_v1(+list(byte), --ground), one).
	:- info(uuid_v1/2, [
		comment is 'Returns a version 1 UUID for the given MAC address (a list of six bytes). The MAC address can be replaced by a random 6 bytes node identifier as per RFC 4122 when the MAC address is not available or should not be disclosed.',
		argnames is ['MAC', 'UUID']
	]).

	:- public(uuid_v4/1).
	:- mode(uuid_v4(--ground), one).
	:- info(uuid_v4/1, [
		comment is 'Returns a version 4 UUID.',
		argnames is ['UUID']
	]).

	:- public(uuid_null/1).
	:- mode(uuid_null(--ground), one).
	:- info(uuid_null/1, [
		comment is 'Returns the null UUID.',
		argnames is ['UUID']
	]).

	:- public(random_node/1).
	:- mode(random_node(--list(byte)), one).
	:- info(random_node/1, [
		comment is 'Generates a list with six random bytes that can be used in alternative to a MAC address when generating version 1 UUIDs.',
		argnames is ['Node']
	]).

:- end_protocol.
