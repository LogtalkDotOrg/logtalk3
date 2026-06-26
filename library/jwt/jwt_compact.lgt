%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(jwt_compact,
	imports(jwt_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'JWT compact serialization parsing and generation helpers.'
	]).

	:- public(decode/3).
	:- mode(decode(+atom, -term, -term), one_or_error).
	:- info(decode/3, [
		comment is 'Decodes a compact JWT into header and claims JSON terms without verifying the signature.',
		argnames is ['Token', 'Header', 'Claims'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(decode/5).
	:- mode(decode(+atom, -term, -term, -list(byte), -atom), one_or_error).
	:- info(decode/5, [
		comment is 'Decodes a compact JWT into header, claims, signature bytes, and signing input.',
		argnames is ['Token', 'Header', 'Claims', 'Signature', 'SigningInput'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(header/2).
	:- mode(header(+atom, -term), one_or_error).
	:- info(header/2, [
		comment is 'Decodes only the JWT header JSON term.',
		argnames is ['Token', 'Header'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(claims/2).
	:- mode(claims(+atom, -term), one_or_error).
	:- info(claims/2, [
		comment is 'Decodes only the JWT claims JSON term.',
		argnames is ['Token', 'Claims'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(segments/5).
	:- mode(segments(+atom, -atom, -atom, -atom, -atom), one_or_error).
	:- info(segments/5, [
		comment is 'Splits a compact JWT into Base64URL segments and signing input.',
		argnames is ['Token', 'HeaderSegment', 'PayloadSegment', 'SignatureSegment', 'SigningInput'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token')
		]
	]).

	:- public(signing_input/4).
	:- mode(signing_input(+term, +term, -atom, -atom), one_or_error).
	:- info(signing_input/4, [
		comment is 'Encodes header and payload JSON terms and returns the compact signing input.',
		argnames is ['Header', 'Payload', 'HeaderSegment', 'SigningInput'],
		exceptions is [
			'``Header`` or ``Payload`` cannot be encoded as JSON' - domain_error(json_sink, 'HeaderOrPayload'),
			'``Header`` or ``Payload`` cannot be encoded as Base64URL bytes' - domain_error(base64url_sink, codes('Codes'))
		]
	]).

	:- public(compact/4).
	:- mode(compact(+atom, +atom, +list(byte), -atom), one_or_error).
	:- info(compact/4, [
		comment is 'Builds a compact JWT from a signing input and signature bytes.',
		argnames is ['HeaderSegment', 'SigningInput', 'Signature', 'Token'],
		exceptions is [
			'``Signature`` cannot be encoded as Base64URL bytes' - domain_error(base64url_sink, codes('Codes'))
		]
	]).

	:- uses(atom, [
		split/3
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	decode(Token, Header, Claims) :-
		segments(Token, HeaderSegment, PayloadSegment, _SignatureSegment, _SigningInput),
		json_segment(HeaderSegment, Header),
		json_segment(PayloadSegment, Claims).

	decode(Token, Header, Claims, Signature, SigningInput) :-
		segments(Token, HeaderSegment, PayloadSegment, SignatureSegment, SigningInput),
		json_segment(HeaderSegment, Header),
		json_segment(PayloadSegment, Claims),
		^^base64url_atom_bytes(SignatureSegment, Signature).

	header(Token, Header) :-
		segments(Token, HeaderSegment, _PayloadSegment, _SignatureSegment, _SigningInput),
		json_segment(HeaderSegment, Header).

	claims(Token, Claims) :-
		segments(Token, _HeaderSegment, PayloadSegment, _SignatureSegment, _SigningInput),
		json_segment(PayloadSegment, Claims).

	segments(Token, HeaderSegment, PayloadSegment, SignatureSegment, SigningInput) :-
		(	var(Token) ->
			instantiation_error
		;	atom(Token) ->
			true
		;	type_error(atom, Token)
		),
		split(Token, '.', Parts),
		(	Parts = [HeaderSegment, PayloadSegment, SignatureSegment] ->
			atomic_list_concat([HeaderSegment, PayloadSegment], '.', SigningInput)
		;	domain_error(jwt_compact_serialization, Token)
		).

	signing_input(Header, Payload, HeaderSegment, SigningInput) :-
		json_segment(HeaderSegment, Header),
		json_segment(PayloadSegment, Payload),
		atomic_list_concat([HeaderSegment, PayloadSegment], '.', SigningInput).

	compact(_HeaderSegment, SigningInput, Signature, Token) :-
		^^base64url_atom_bytes(SignatureSegment, Signature),
		atomic_list_concat([SigningInput, SignatureSegment], '.', Token).

	json_segment(Segment, JSON) :-
		atom(Segment),
		!,
		^^base64url_atom_bytes(Segment, Bytes),
		atom_codes(Atom, Bytes),
		json::parse(atom(Atom), JSON).
	json_segment(Segment, JSON) :-
		json::generate(atom(Atom), JSON),
		atom_codes(Atom, Bytes),
		^^base64url_atom_bytes(Segment, Bytes).

:- end_object.
