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


:- object(s3_headers).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Helpers for extracting S3 response metadata from normalized HTTP responses.'
	]).

	:- public(response_properties/2).
	:- mode(response_properties(+compound, -list(compound)), one_or_error).
	:- info(response_properties/2, [
		comment is 'Collects status and response headers into a stable property list.',
		argnames is ['Response', 'Properties'],
		exceptions is [
			'``Response`` is not a normalized HTTP response term' - domain_error(http_response, 'Response')
		]
	]).

	:- public(header_value/3).
	:- mode(header_value(+compound, +atom, -term), zero_or_more).
	:- info(header_value/3, [
		comment is 'Enumerates response header values by normalized header name.',
		argnames is ['Response', 'Name', 'Value'],
		exceptions is [
			'``Response`` is not a normalized HTTP response term' - domain_error(http_response, 'Response')
		]
	]).

	:- uses(http_core, [
		header/3, headers/2, status/2
	]).

	response_properties(Response, [status(Code, Reason)| HeaderProperties]) :-
		http_core::status(Response, status(Code, Reason)),
		http_core::headers(Response, Headers),
		header_properties(Headers, HeaderProperties).

	header_value(Response, Name, Value) :-
		http_core::header(Response, Name, Value).

	header_properties([], []).
	header_properties([Name-Value| Headers], [header(Name, Value)| Properties]) :-
		header_properties(Headers, Properties).

:- end_object.
