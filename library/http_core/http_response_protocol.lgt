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


:- protocol(http_response_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Protocol for Logtalk objects exposing normalized HTTP response data.'
	]).

	:- public(version/1).
	:- mode(version(-compound), one).
	:- info(version/1, [
		comment is 'Returns the normalized HTTP version term ``http(Major, Minor)``.',
		argnames is ['Version']
	]).

	:- public(status/1).
	:- mode(status(-compound), one).
	:- info(status/1, [
		comment is 'Returns the normalized HTTP status term ``status(Code, ReasonPhrase)``.',
		argnames is ['Status']
	]).

	:- public(headers/1).
	:- mode(headers(-list(compound)), one).
	:- info(headers/1, [
		comment is 'Returns the response header list as normalized ``Name-Value`` pairs.',
		argnames is ['Headers']
	]).

	:- public(header/2).
	:- mode(header(?atom, ?term), zero_or_more).
	:- info(header/2, [
		comment is 'Enumerates normalized response headers as ``Name`` and ``Value`` pairs.',
		argnames is ['Name', 'Value']
	]).

	:- public(body/1).
	:- mode(body(-compound), one).
	:- info(body/1, [
		comment is 'Returns the normalized response body term.',
		argnames is ['Body']
	]).

	:- public(property/1).
	:- mode(property(?compound), zero_or_more).
	:- info(property/1, [
		comment is 'Enumerates derived or higher-layer normalized response properties.',
		argnames is ['Property']
	]).

:- end_protocol.
