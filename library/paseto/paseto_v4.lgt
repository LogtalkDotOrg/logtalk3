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


:- object(paseto_v4,
	implements(paseto_protocol),
	imports(paseto_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'PASETO v4.local and v4.public implementation for byte payloads.'
	]).

	:- protected(local_encrypt_with_nonce/6).
	:- mode(local_encrypt_with_nonce(+list(byte), +list(byte), +list(byte), +list(byte), +list(byte), -atom), one_or_error).
	:- info(local_encrypt_with_nonce/6, [
		comment is 'Encrypts a local payload using an explicit 32-byte nonce. Intended for deterministic conformance tests; production callers must use local_encrypt/3 or local_encrypt/5.',
		argnames is ['Key', 'Nonce', 'Payload', 'Footer', 'ImplicitAssertion', 'Token']
	]).

	:- uses(list, [
		append/2, append/3, length/2
	]).

	:- uses(type, [
		check/3
	]).

	local_key(Key) :-
		crypto::random_bytes(32, Key).

	public_keypair(Seed, PublicKey) :-
		crypto::ed25519_keypair(Seed, PublicKey).

	local_encrypt(Key, Payload, Token) :-
		local_encrypt(Key, Payload, [], [], Token).

	local_encrypt(Key, Payload, Footer, ImplicitAssertion, Token) :-
		crypto::random_bytes(32, Nonce),
		local_encrypt_with_nonce(Key, Nonce, Payload, Footer, ImplicitAssertion, Token).

	local_encrypt_with_nonce(Key, Nonce, Payload, Footer, ImplicitAssertion, Token) :-
		context(Context),
		check(list(byte, 32), Key, Context),
		check(list(byte, 32), Nonce, Context),
		check(list(byte), Payload, Context),
		check(list(byte), Footer, Context),
		check(list(byte), ImplicitAssertion, Context),
		local_keys(Key, Nonce, EncryptionKey, CounterNonce, AuthenticationKey),
		crypto::xchacha20(EncryptionKey, CounterNonce, Payload, Ciphertext),
		local_header(Header),
		^^pae([Header, Nonce, Ciphertext, Footer, ImplicitAssertion], PreAuth),
		blake2b(AuthenticationKey, 32)::digest(PreAuth, Tag),
		append([Nonce, Ciphertext, Tag], Body),
		^^format_token(local, Body, Footer, Token).

	local_decrypt(Token, Key, Payload) :-
		local_decrypt(Token, Key, [], Payload, _).

	local_decrypt(Token, Key, ImplicitAssertion, Payload, Footer) :-
		context(Context),
		check(list(byte, 32), Key, Context),
		check(list(byte), ImplicitAssertion, Context),
		^^parse_token(Token, local, Body, Footer),
		split_local_body(Token, Body, Nonce, Ciphertext, Tag),
		local_keys(Key, Nonce, EncryptionKey, CounterNonce, AuthenticationKey),
		local_header(Header),
		^^pae([Header, Nonce, Ciphertext, Footer, ImplicitAssertion], PreAuth),
		blake2b(AuthenticationKey, 32)::digest(PreAuth, ExpectedTag),
		crypto::secure_compare(ExpectedTag, Tag),
		crypto::xchacha20(EncryptionKey, CounterNonce, Ciphertext, Payload).

	public_sign(Seed, Payload, Token) :-
		public_sign(Seed, Payload, [], [], Token).

	public_sign(Seed, Payload, Footer, ImplicitAssertion, Token) :-
		context(Context),
		check(list(byte, 32), Seed, Context),
		check(list(byte), Payload, Context),
		check(list(byte), Footer, Context),
		check(list(byte), ImplicitAssertion, Context),
		public_header(Header),
		^^pae([Header, Payload, Footer, ImplicitAssertion], PreAuth),
		crypto::ed25519_sign(Seed, PreAuth, Signature),
		append(Payload, Signature, Body),
		^^format_token(public, Body, Footer, Token).

	public_verify(Token, PublicKey, Payload) :-
		public_verify(Token, PublicKey, [], Payload, _).

	public_verify(Token, PublicKey, ImplicitAssertion, Payload, Footer) :-
		context(Context),
		check(list(byte, 32), PublicKey, Context),
		check(list(byte), ImplicitAssertion, Context),
		^^parse_token(Token, public, Body, Footer),
		split_public_body(Token, Body, Payload, Signature),
		public_header(Header),
		^^pae([Header, Payload, Footer, ImplicitAssertion], PreAuth),
		crypto::ed25519_verify(PublicKey, PreAuth, Signature).

	footer(Token, Footer) :-
		(	catch(^^parse_token(Token, local, _, Footer), _, fail) ->
			true
		;	^^parse_token(Token, public, _, Footer)
		).

	local_keys(Key, Nonce, EncryptionKey, CounterNonce, AuthenticationKey) :-
		atom_codes('paseto-encryption-key', EncryptionLabel),
		append(EncryptionLabel, Nonce, EncryptionInput),
		blake2b(Key, 56)::digest(EncryptionInput, EncryptionMaterial),
		length(EncryptionKey, 32),
		append(EncryptionKey, CounterNonce, EncryptionMaterial),
		atom_codes('paseto-auth-key-for-aead', AuthenticationLabel),
		append(AuthenticationLabel, Nonce, AuthenticationInput),
		blake2b(Key, 32)::digest(AuthenticationInput, AuthenticationKey).

	split_local_body(Token, Body, Nonce, Ciphertext, Tag) :-
		length(Body, Length),
		(	Length >= 64 ->
			true
		;	domain_error(paseto_v4_local_payload, Token)
		),
		length(Nonce, 32),
		append(Nonce, CiphertextAndTag, Body),
		CiphertextLength is Length - 64,
		length(Ciphertext, CiphertextLength),
		append(Ciphertext, Tag, CiphertextAndTag).

	split_public_body(Token, Body, Payload, Signature) :-
		length(Body, Length),
		(	Length >= 64 ->
			true
		;	domain_error(paseto_v4_public_payload, Token)
		),
		PayloadLength is Length - 64,
		length(Payload, PayloadLength),
		append(Payload, Signature, Body).

	local_header([0'v,0'4,0'.,0'l,0'o,0'c,0'a,0'l,0'.]).

	public_header([0'v,0'4,0'.,0'p,0'u,0'b,0'l,0'i,0'c,0'.]).

:- end_object.
