%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(jwt_der,
	imports(jwt_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'DER and PEM helpers for JWK public keys and JOSE signatures.'
	]).

	:- public(public_key_pem/2).
	:- mode(public_key_pem(+term, -atom), one_or_error).
	:- info(public_key_pem/2, [
		comment is 'Converts a supported JWK public key into PEM SubjectPublicKeyInfo text.',
		argnames is ['Key', 'PEM'],
		exceptions is [
			'``Key`` is not a supported RSA or P-256 EC public JWK' - domain_error(jwt_jwk_public_key, 'Key'),
			'``Key`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(es256_signature_der/2).
	:- mode(es256_signature_der(+list(byte), -list(byte)), one_or_error).
	:- info(es256_signature_der/2, [
		comment is 'Converts a raw JOSE ES256 signature into DER-encoded ASN.1 form.',
		argnames is ['Signature', 'DER'],
		exceptions is [
			'``Signature`` is not a 64-byte raw ES256 signature' - domain_error(jwt_es256_signature, 'Signature')
		]
	]).

	:- uses(list, [
		append/2, append/3, length/2, take/4
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	public_key_pem(Key, PEM) :-
		^^json_member(kty, Key, 'RSA'),
		!,
		^^json_member(n, Key, NAtom),
		^^json_member(e, Key, EAtom),
		^^base64url_atom_bytes(NAtom, NBytes),
		^^base64url_atom_bytes(EAtom, EBytes),
		asn1_integer(NBytes, NInteger),
		asn1_integer(EBytes, EInteger),
		asn1_sequence([NInteger, EInteger], RSAPublicKey),
		asn1_oid([42,134,72,134,247,13,1,1,1], RSAEncryptionOID),
		asn1_null(ASN1Null),
		asn1_sequence([RSAEncryptionOID, ASN1Null], Algorithm),
		asn1_bit_string(RSAPublicKey, BitString),
		asn1_sequence([Algorithm, BitString], SPKI),
		pem('PUBLIC KEY', SPKI, PEM).
	public_key_pem(Key, PEM) :-
		^^json_member(kty, Key, 'EC'),
		^^json_member(crv, Key, 'P-256'),
		!,
		^^json_member(x, Key, XAtom),
		^^json_member(y, Key, YAtom),
		^^base64url_atom_bytes(XAtom, XBytes),
		^^base64url_atom_bytes(YAtom, YBytes),
		append([0x04| XBytes], YBytes, Point),
		asn1_oid([42,134,72,206,61,2,1], ECPublicKeyOID),
		asn1_oid([42,134,72,206,61,3,1,7], Prime256v1OID),
		asn1_sequence([ECPublicKeyOID, Prime256v1OID], Algorithm),
		asn1_bit_string(Point, BitString),
		asn1_sequence([Algorithm, BitString], SPKI),
		pem('PUBLIC KEY', SPKI, PEM).
	public_key_pem(Key, _) :-
		domain_error(jwt_jwk_public_key, Key).

	es256_signature_der(Signature, DER) :-
		length(Signature, 64),
		!,
		take(32, Signature, RBytes, SBytes),
		asn1_integer(RBytes, RInteger),
		asn1_integer(SBytes, SInteger),
		asn1_sequence([RInteger, SInteger], DER).
	es256_signature_der(Signature, _) :-
		domain_error(jwt_es256_signature, Signature).

	asn1_sequence(Elements, [0x30| DER]) :-
		append(Elements, Content),
		asn1_length(Content, Length),
		append(Length, Content, DER).

	asn1_integer(Bytes0, [0x02| DER]) :-
		strip_leading_zeroes(Bytes0, Bytes1),
		positive_integer_bytes(Bytes1, Bytes),
		asn1_length(Bytes, Length),
		append(Length, Bytes, DER).

	asn1_null([0x05, 0x00]).

	asn1_oid(Identifier, [0x06| DER]) :-
		asn1_length(Identifier, Length),
		append(Length, Identifier, DER).

	asn1_bit_string(Bytes, [0x03| DER]) :-
		asn1_length([0| Bytes], Length),
		append(Length, [0| Bytes], DER).

	asn1_length(Content, [Length]) :-
		length(Content, Length),
		Length < 128,
		!.
	asn1_length(Content, [Prefix| LengthBytes]) :-
		length(Content, Length),
		integer_bytes(Length, LengthBytes),
		length(LengthBytes, LengthLength),
		Prefix is 0x80 + LengthLength.

	integer_bytes(Integer, Bytes) :-
		integer_bytes(Integer, [], Bytes).

	integer_bytes(0, [], [0]) :-
		!.
	integer_bytes(0, Bytes, Bytes) :-
		!.
	integer_bytes(Integer, Acc, Bytes) :-
		Byte is Integer /\ 0xff,
		Next is Integer >> 8,
		integer_bytes(Next, [Byte| Acc], Bytes).

	strip_leading_zeroes([0, Byte| Bytes], Stripped) :-
		Byte < 128,
		!,
		strip_leading_zeroes([Byte| Bytes], Stripped).
	strip_leading_zeroes(Bytes, Bytes).

	positive_integer_bytes([], [0]) :-
		!.
	positive_integer_bytes([Byte| Bytes], [0, Byte| Bytes]) :-
		Byte >= 128,
		!.
	positive_integer_bytes(Bytes, Bytes).

	pem(Label, DER, PEM) :-
		base64::generate(atom(Base64), DER),
		atomic_list_concat(['-----BEGIN ', Label, '-----\n', Base64, '\n-----END ', Label, '-----\n'], PEM).

:- end_object.
