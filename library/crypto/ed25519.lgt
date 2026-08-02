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


:- category(ed25519,
	complements(crypto)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'Ed25519 (RFC 8032) public-key signature implementation. Requires exact, unbounded integer arithmetic for arithmetic modulo the 255-bit field prime and modulo the group order.'
	]).

	:- public(ed25519_keypair/2).
	:- mode(ed25519_keypair(-list(byte), -list(byte)), one).
	:- info(ed25519_keypair/2, [
		comment is 'Generates a fresh random 32-byte Seed and derives the corresponding 32-byte PublicKey. The seed is the value to keep secret and to pass to ed25519_sign/3; it is also sometimes called the "secret key". Available only on backends with unbounded integer arithmetic.',
		argnames is ['Seed', 'PublicKey']
	]).

	:- public(ed25519_public_key/2).
	:- mode(ed25519_public_key(+list(byte), -list(byte)), one_or_error).
	:- info(ed25519_public_key/2, [
		comment is 'Derives the 32-byte PublicKey corresponding to a 32-byte Seed. Available only on backends with unbounded integer arithmetic.',
		argnames is ['Seed', 'PublicKey'],
		exceptions is [
			'``Seed`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Seed`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'Seed'),
			'``Seed`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Seed`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(ed25519_sign/3).
	:- mode(ed25519_sign(+list(byte), +list(byte), -list(byte)), one_or_error).
	:- info(ed25519_sign/3, [
		comment is 'Computes the 64-byte Ed25519 Signature of Message using Seed. Not constant-time; see the module-level note on this section. Available only on backends with unbounded integer arithmetic.',
		argnames is ['Seed', 'Message', 'Signature'],
		exceptions is [
			'``Seed`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Seed`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'Seed'),
			'``Seed`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Seed`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Message`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Message`` is neither a variable nor a list of bytes' - type_error(list(byte), 'Message'),
			'``Message`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Message`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(ed25519_verify/3).
	:- mode(ed25519_verify(+list(byte), +list(byte), +list(byte)), zero_or_one).
	:- info(ed25519_verify/3, [
		comment is 'Succeeds if Signature is a valid Ed25519 signature of Message under PublicKey, and fails otherwise (invalid signature, non-canonical S, non-canonical or small-order PublicKey or R component, or malformed input lengths); see the module-level note on this section for the exact verification policy. Available only on backends with unbounded integer arithmetic.',
		argnames is ['PublicKey', 'Message', 'Signature']
	]).

 	:- uses(list, [
		append/3, length/2, valid/1 as is_list/1
 	]).

	:- uses(sha512, [
		digest/2
	]).

	:- uses(type, [
		check/3
	]).

	% Little-endian byte list <-> non-negative integer, arbitrary length.

	le_bytes_to_int(Bytes, Int) :-
		le_bytes_to_int(Bytes, 0, 0, Int).
	le_bytes_to_int([], _, Accumulator, Accumulator).
	le_bytes_to_int([Byte| Bytes], Shift, Accumulator0, Int) :-
		Accumulator1 is Accumulator0 \/ (Byte << Shift),
		Shift1 is Shift + 8,
		le_bytes_to_int(Bytes, Shift1, Accumulator1, Int).

	int_to_le_bytes_fixed(Int, Count, Bytes) :-
		length(Bytes, Count),
		int_to_le_bytes_fixed_loop(Int, Bytes).
	int_to_le_bytes_fixed_loop(_, []) :-
		!.
	int_to_le_bytes_fixed_loop(Int, [Byte| Bytes]) :-
		Byte is Int /\ 0xff,
		Int1 is Int >> 8,
		int_to_le_bytes_fixed_loop(Int1, Bytes).

	% ---------------------------------------------------------------
	% Ed25519 (RFC 8032).
	%
	% Verification policy: signatures are checked with the cofactored
	% group equation [8][S]B =? [8]R + [8][k]A, as recommended (though
	% not strictly mandated) by RFC 8032, and both the public key and
	% the R component of the signature are additionally required to be
	% canonically encoded and of large order (i.e. not one of the 7
	% points of order dividing 8), matching the hardening most
	% widely-deployed implementations apply for "strongly binding
	% signature" security. S is required to satisfy 0 <= S < L, which
	% RFC 8032 mandates unconditionally. Signing uses simple
	% double-and-add scalar multiplication: it is not constant-time,
	% so timing side channels on the secret scalar are possible on the
	% signing path. Signature verification never touches secret data,
	% so it is not affected by that limitation.
	% ---------------------------------------------------------------

	ed25519_keypair(Seed, PublicKey) :-
		@random_bytes(32, Seed),
		ed25519_public_key(Seed, PublicKey).

	ed25519_public_key(Seed, PublicKey) :-
		context(Context),
		check(list(byte, 32), Seed, Context),
		ed25519_secret_expand(Seed, A, _),
		ed25519_base_point(G),
		ed25519_point_mul(A, G, P),
		ed25519_point_compress(P, PublicKey).

	ed25519_sign(Seed, Message, Signature) :-
		context(Context),
		check(list(byte, 32), Seed, Context),
		check(list(byte), Message, Context),
		ed25519_secret_expand(Seed, A, Prefix),
		ed25519_base_point(G),
		ed25519_point_mul(A, G, APoint),
		ed25519_point_compress(APoint, ABytes),
		append(Prefix, Message, PrefixMessage),
		ed25519_sha512_modq(PrefixMessage, R),
		ed25519_point_mul(R, G, RPoint),
		ed25519_point_compress(RPoint, RBytes),
		append(RBytes, ABytes, RA),
		append(RA, Message, RAM),
		ed25519_sha512_modq(RAM, H),
		ed25519_q(Q),
		S is (R + H*A) mod Q,
		int_to_le_bytes_fixed(S, 32, SBytes),
		append(RBytes, SBytes, Signature).

	ed25519_verify(PublicKey, Message, Signature) :-
		is_list(PublicKey), length(PublicKey, 32),
		is_list(Signature), length(Signature, 64),
		is_list(Message),
		length(RBytes, 32),
		append(RBytes, SBytes, Signature),
		le_bytes_to_int(SBytes, S),
		ed25519_q(Q),
		S < Q,
		ed25519_point_decompress(PublicKey, APoint, true),
		APoint \== none,
		\+ ed25519_point_has_small_order(APoint),
		ed25519_point_decompress(RBytes, RPoint, true),
		RPoint \== none,
		\+ ed25519_point_has_small_order(RPoint),
		append(RBytes, PublicKey, RA),
		append(RA, Message, RAM),
		ed25519_sha512_modq(RAM, H),
		ed25519_base_point(G),
		ed25519_point_mul(S, G, SB),
		ed25519_point_mul(H, APoint, HA),
		ed25519_point_add(RPoint, HA, RHA),
		ed25519_point_mul(8, SB, SB8),
		ed25519_point_mul(8, RHA, RHA8),
		ed25519_point_equal(SB8, RHA8).

	% -- Ed25519 key derivation --

	ed25519_secret_expand(Secret, A, Prefix) :-
		digest(Secret, H),
		length(HA, 32),
		append(HA, Prefix, H),
		le_bytes_to_int(HA, A0),
		A1 is A0 /\ ((1 << 254) - 8),
		A is A1 \/ (1 << 254).

	ed25519_sha512_modq(Bytes, R) :-
		digest(Bytes, Digest),
		le_bytes_to_int(Digest, N),
		ed25519_q(Q),
		R is N mod Q.

	% -- Ed25519 field and curve constants --

	ed25519_p(P) :-
		P is (1 << 255) - 19.

	ed25519_q(Q) :-
		Q is (1 << 252) + 27742317777372353535851937790883648493.

	ed25519_d(D) :-
		ed25519_p(P),
		ed25519_modp_inv(121666, Inv),
		D is (-121665 * Inv) mod P.

	ed25519_modp_inv(X, R) :-
		ed25519_p(P),
		Exponent is P - 2,
		ed25519_modexp(X, Exponent, P, R).

	ed25519_modexp(_, 0, M, R) :-
		!,
		R is 1 mod M.
	ed25519_modexp(B, E, M, R) :-
		E > 0,
		E2 is E // 2,
		ed25519_modexp(B, E2, M, Half),
		Sq is (Half*Half) mod M,
		(	E mod 2 =:= 1 ->
			R is (Sq * (B mod M)) mod M
		;	R = Sq
		).

	ed25519_modp_sqrt_m1(R) :-
		ed25519_p(P),
		Exponent is (P-1)//4,
		ed25519_modexp(2, Exponent, P, R).

	% -- Ed25519 point arithmetic, extended coordinates (X,Y,Z,T) --
	% Complete (exception-free) unified addition formula for twisted Edwards curves,
	% so the same predicate handles both point addition and point doubling.

	ed25519_point_add(pt(X0,Y0,Z0,T0), pt(X1,Y1,Z1,T1), pt(X2,Y2,Z2,T2)) :-
		ed25519_p(P),
		ed25519_d(D),
		A is ((Y0-X0) * (Y1-X1)) mod P,
		B is ((Y0+X0) * (Y1+X1)) mod P,
		C is (2*T0*T1*D) mod P,
		Dd is (2*Z0*Z1) mod P,
		E is (B-A) mod P, F is (Dd-C) mod P, G is (Dd+C) mod P, H is (B+A) mod P,
		X2 is (E*F) mod P, Y2 is (G*H) mod P, Z2 is (F*G) mod P, T2 is (E*H) mod P.

	ed25519_point_mul(0, _, pt(0,1,1,0)) :-
		!.
	ed25519_point_mul(S, P, R) :-
		S > 0,
		S1 is S // 2,
		ed25519_point_mul(S1, P, Half),
		ed25519_point_add(Half, Half, Double),
		(	S mod 2 =:= 1 ->
			ed25519_point_add(Double, P, R)
		;	R = Double
		).

	ed25519_point_equal(pt(X0,Y0,Z0,_), pt(X1,Y1,Z1,_)) :-
		ed25519_p(P),
		(X0*Z1 - X1*Z0) mod P =:= 0,
		(Y0*Z1 - Y1*Z0) mod P =:= 0.

	ed25519_recover_x(Y, _, none) :-
		ed25519_p(P),
		Y >= P,
		!.
	ed25519_recover_x(Y, Sign, X) :-
		ed25519_p(P),
		ed25519_d(D),
		X2num is (Y*Y - 1) mod P,
		DYY1 is (D*Y*Y + 1) mod P,
		ed25519_modp_inv(DYY1, DYY1inv),
		X2 is (X2num * DYY1inv) mod P,
		(	X2 =:= 0 ->
			( Sign =:= 1 -> X = none ; X = 0 )
		;	Exponent is (P+3)//8,
			ed25519_modexp(X2, Exponent, P, X0),
			(	(X0*X0 - X2) mod P =:= 0 ->
				X1 = X0
			;	ed25519_modp_sqrt_m1(SqrtM1),
				X1a is (X0 * SqrtM1) mod P,
				( (X1a*X1a - X2) mod P =:= 0 -> X1 = X1a ; X1 = fail )
			),
			(	X1 == fail ->
				X = none
			;	( (X1 /\ 1) =:= Sign -> X = X1 ; X is P - X1 )
			)
		).

	ed25519_base_point(G) :-
		ed25519_p(P),
		ed25519_modp_inv(5, Inv5),
		GY is (4 * Inv5) mod P,
		ed25519_recover_x(GY, 0, GX),
		GT is (GX*GY) mod P,
		G = pt(GX,GY,1,GT).

	ed25519_point_compress(pt(X,Y,Z,_), Bytes) :-
		ed25519_p(P),
		ed25519_modp_inv(Z, Zinv),
		Xa is (X*Zinv) mod P,
		Ya is (Y*Zinv) mod P,
		SignBit is Xa /\ 1,
		Value is Ya \/ (SignBit << 255),
		int_to_le_bytes_fixed(Value, 32, Bytes).

	% ed25519_point_decompress(+Bytes32, -Point, -Canonical)
	% Point = none if the encoding does not correspond to a point on the curve.
	% Canonical = false if the y-coordinate encoding was not fully reduced mod p
	% (Point is bound to none in that case too, since such input must be rejected
	% by callers regardless of what recover_x would otherwise compute).
	ed25519_point_decompress(Bytes, Point, Canonical) :-
		le_bytes_to_int(Bytes, Yraw),
		Sign is Yraw >> 255,
		Y is Yraw /\ ((1 << 255) - 1),
		ed25519_p(P),
		(	Y >= P ->
			Canonical = false, Point = none
		;	Canonical = true,
			ed25519_recover_x(Y, Sign, X),
			(	X == none ->
				Point = none
			;	T is (X*Y) mod P, Point = pt(X,Y,1,T)
			)
		).

	ed25519_point_has_small_order(Point) :-
		ed25519_point_mul(8, Point, pt(X,Y,Z,_)),
		ed25519_p(P),
		(X mod P) =:= 0,
		(Y mod P) =:= (Z mod P).

:- end_category.
