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


:- category(http_message_helpers,
	extends(http_text_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Internal shared helpers for HTTP transport persistence rules, connection token normalization, and chunk-size line parsing.'
	]).

	:- protected(connection_persistent/2).
	:- mode(connection_persistent(+compound, +compound), zero_or_one).
	:- info(connection_persistent/2, [
		comment is 'Succeeds when a request-response exchange can continue on the same connection according to HTTP persistence semantics.',
		argnames is ['Request', 'Response']
	]).

	:- protected(request_persistent/1).
	:- mode(request_persistent(+compound), zero_or_one).
	:- info(request_persistent/1, [
		comment is 'Succeeds when a normalized HTTP request keeps the connection persistent.',
		argnames is ['Request']
	]).

	:- protected(response_persistent/1).
	:- mode(response_persistent(+compound), zero_or_one).
	:- info(response_persistent/1, [
		comment is 'Succeeds when a normalized HTTP response keeps the connection persistent.',
		argnames is ['Response']
	]).

	:- protected(version_persistent_by_default/1).
	:- mode(version_persistent_by_default(+compound), zero_or_one).
	:- info(version_persistent_by_default/1, [
		comment is 'Succeeds when the given HTTP version uses persistent connections by default.',
		argnames is ['Version']
	]).

	:- protected(message_has_connection_token/2).
	:- mode(message_has_connection_token(+compound, +atom), zero_or_one).
	:- info(message_has_connection_token/2, [
		comment is 'Succeeds when a normalized HTTP message exposes the given normalized Connection token through either headers or derived properties.',
		argnames is ['Message', 'Token']
	]).

	:- protected(message_connection_tokens/2).
	:- mode(message_connection_tokens(+compound, -list(atom)), one).
	:- info(message_connection_tokens/2, [
		comment is 'Returns the de-duplicated normalized Connection tokens exposed by a normalized HTTP message.',
		argnames is ['Message', 'Tokens']
	]).

	:- protected(message_header_values/3).
	:- mode(message_header_values(+compound, +atom, -list), one).
	:- info(message_header_values/3, [
		comment is 'Returns the stored header values for the given header name in message order.',
		argnames is ['Message', 'Name', 'Values']
	]).

	:- protected(chunk_size_line_size/2).
	:- mode(chunk_size_line_size(+list(integer), -integer), one).
	:- info(chunk_size_line_size/2, [
		comment is 'Parses a chunk-size line into its hexadecimal size, ignoring optional whitespace and chunk extensions.',
		argnames is ['LineBytes', 'Size']
	]).

	:- uses(list, [
		member/2, memberchk/2, reverse/2
	]).

	:- uses(http, [
		headers/2 as http_headers/2,
		version/2 as http_version/2,
		property/2 as http_property/2
	]).

	connection_persistent(Request, Response) :-
		request_persistent(Request),
		response_persistent(Response).

	request_persistent(Request) :-
		http_version(Request, Version),
		\+ message_has_connection_token(Request, close),
		(	version_persistent_by_default(Version) ->
			true
		;	message_has_connection_token(Request, 'keep-alive')
		).

	response_persistent(Response) :-
		http_version(Response, Version),
		\+ http_property(Response, body_framing(close_delimited)),
		\+ message_has_connection_token(Response, close),
		(	version_persistent_by_default(Version) ->
			true
		;	message_has_connection_token(Response, 'keep-alive')
		).

	version_persistent_by_default(http(1, 0)) :-
		!,
		fail.
	version_persistent_by_default(http(1, Minor)) :-
		!,
		Minor >= 1.
	version_persistent_by_default(http(Major, _Minor)) :-
		Major > 1.

	message_header_values(Message, Name, Values) :-
		http_headers(Message, Headers),
		message_header_values_list(Headers, Name, Values).

	message_header_values_list([], _Name, []).
	message_header_values_list([Name-Value| Headers], Name, [Value| Values]) :-
		!,
		message_header_values_list(Headers, Name, Values).
	message_header_values_list([_Header| Headers], Name, Values) :-
		message_header_values_list(Headers, Name, Values).

	message_has_connection_token(Message, Token) :-
		message_connection_tokens(Message, Tokens),
		memberchk(Token, Tokens).

	message_connection_tokens(request(_Method, _Target, _Version, Headers, _Body, Properties), Tokens) :-
		connection_tokens(Headers, Properties, Tokens).
	message_connection_tokens(response(_Version, _Status, Headers, _Body, Properties), Tokens) :-
		connection_tokens(Headers, Properties, Tokens).

	connection_tokens(Headers, Properties, Tokens) :-
		(	member(connection(PropertyTokens), Properties) ->
			true
		;	PropertyTokens = []
		),
		(	member(connection-HeaderTokens, Headers) ->
			true
		;	HeaderTokens = []
		),
		merge_unique_tokens([PropertyTokens, HeaderTokens], Tokens).

	merge_unique_tokens(TokenLists, Tokens) :-
		merge_unique_tokens(TokenLists, [], ReversedTokens),
		reverse(ReversedTokens, Tokens).

	merge_unique_tokens([], Tokens, Tokens).
	merge_unique_tokens([TokenList| TokenLists], Tokens0, Tokens) :-
		merge_unique_token_list(TokenList, Tokens0, Tokens1),
		merge_unique_tokens(TokenLists, Tokens1, Tokens).

	merge_unique_token_list([], Tokens, Tokens).
	merge_unique_token_list([Token| TokenList], Tokens0, Tokens) :-
		(	member(Token, Tokens0) ->
			Tokens1 = Tokens0
		;	Tokens1 = [Token| Tokens0]
		),
		merge_unique_token_list(TokenList, Tokens1, Tokens).

	chunk_size_line_size(LineBytes, Size) :-
		chunk_size_codes(LineBytes, SizeBytes0),
		trim_ows(SizeBytes0, SizeBytes),
		SizeBytes \== [],
		number_codes(Size, [0'0, 0'x| SizeBytes]).

	chunk_size_codes([], []).
	chunk_size_codes([0';| _], []) :-
		!.
	chunk_size_codes([Byte| Bytes], [Byte| SizeBytes]) :-
		chunk_size_codes(Bytes, SizeBytes).

	trim_ows(Bytes0, Bytes) :-
		^^trim_ows_codes(Bytes0, Bytes).

:- end_category.
