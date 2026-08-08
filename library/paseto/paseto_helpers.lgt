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


:- category(paseto_helpers).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-08,
		comment is 'Protected PASETO v4 framing, PAE, and encoding helpers.'
	]).

	:- protected(pae/2).
	:- mode(pae(+list(list(byte)), -list(byte)), one).
	:- info(pae/2, [
		comment is 'Pre-authentication encodes a list of byte-list pieces.',
		argnames is ['Pieces', 'Encoding']
	]).

	:- protected(parse_token/4).
	:- mode(parse_token(+atom, +atom, -list(byte), -list(byte)), one_or_error).
	:- info(parse_token/4, [
		comment is 'Strictly parses a canonical v4 token for Purpose and decodes its body and optional footer.',
		argnames is ['Token', 'Purpose', 'Body', 'Footer'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but does not have the expected PASETO v4 header' - domain_error(paseto_v4_token, 'Token'),
			'``Token`` is a PASETO v4 header but has a malformed compact serialization' - domain_error(paseto_compact_serialization, malformed),
			'``Token`` contains non-canonical base64url data' - representation_error(base64)
		]
	]).

	:- protected(format_token/4).
	:- mode(format_token(+atom, +list(byte), +list(byte), -atom), one).
	:- info(format_token/4, [
		comment is 'Formats a canonical v4 token for Purpose from body and footer bytes.',
		argnames is ['Purpose', 'Body', 'Footer', 'Token']
	]).

	:- uses(list, [
		append/2, append/3, length/2
	]).

	pae(Pieces, Encoding) :-
		length(Pieces, Count),
		le64(Count, CountBytes),
		pae_pieces(Pieces, EncodedPieces),
		append(CountBytes, EncodedPieces, Encoding).

	pae_pieces([], []).
	pae_pieces([Piece| Pieces], Encoding) :-
		length(Piece, Length),
		le64(Length, LengthBytes),
		pae_pieces(Pieces, Rest),
		append([LengthBytes, Piece, Rest], Encoding).

	le64(Integer, Bytes) :-
		le64(Integer, 8, Bytes).

	le64(_, 0, []) :-
		!.
	le64(Integer, Count, [Byte| Bytes]) :-
		Byte is Integer mod 256,
		Next is Integer // 256,
		Remaining is Count - 1,
		le64(Next, Remaining, Bytes).

	parse_token(Token, Purpose, Body, Footer) :-
		(	var(Token) ->
			instantiation_error
		;	atom(Token) ->
			true
		;	type_error(atom, Token)
		),
		header_codes(Purpose, Header),
		atom_codes(Token, Codes),
		(	append(Header, Compact, Codes),
			Compact \== [] ->
			true
		;	domain_error(paseto_v4_token, Token)
		),
		split_compact(Compact, BodyCodes, FooterCodes),
		decode_base64url(BodyCodes, Body),
		(	FooterCodes == none ->
			Footer = []
		;	decode_base64url(FooterCodes, Footer)
		).

	split_compact(Codes, Body, Footer) :-
		split_at_dot(Codes, Body, Tail),
		Body \== [],
		(	Tail == [] ->
			Footer = none
		;	Tail = [0'.| FooterCodes],
			FooterCodes \== [],
			\+ contains_dot(FooterCodes),
			Footer = FooterCodes
		),
		!.
	split_compact(_, _, _) :-
		domain_error(paseto_compact_serialization, malformed).

	split_at_dot([], [], []).
	split_at_dot([0'.| Codes], [], [0'.| Codes]) :-
		!.
	split_at_dot([Code| Codes], [Code| Segment], Tail) :-
		split_at_dot(Codes, Segment, Tail).

	contains_dot([0'.| _]) :-
		!.
	contains_dot([_| Codes]) :-
		contains_dot(Codes).

	decode_base64url(Codes, Bytes) :-
		base64url_no_padding::parse(codes(Codes), Bytes),
		base64url_no_padding::generate(codes(Canonical), Bytes),
		( Codes == Canonical -> true
		; representation_error(base64)
		).

	format_token(Purpose, Body, Footer, Token) :-
		header_codes(Purpose, Header),
		base64url_no_padding::generate(codes(BodyCodes), Body),
		(	Footer == [] ->
			append(Header, BodyCodes, Codes)
		;	base64url_no_padding::generate(codes(FooterCodes), Footer),
			append([Header, BodyCodes, [0'.], FooterCodes], Codes)
		),
		atom_codes(Token, Codes).

	header_codes(local, [0'v,0'4,0'.,0'l,0'o,0'c,0'a,0'l,0'.]).
	header_codes(public, [0'v,0'4,0'.,0'p,0'u,0'b,0'l,0'i,0'c,0'.]).

:- end_category.
