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


:- category(authenticated_channel,
	complements(crypto)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Identity-authenticated ephemeral X25519 channels with directional XChaCha20-Poly1305 keys, strict counters, and per-message symmetric key ratcheting.'
	]).

	:- public(authenticated_channel_initiate/4).
	:- mode(authenticated_channel_initiate(+list(byte), +list(byte), -compound, -compound), one_or_error).
	:- info(authenticated_channel_initiate/4, [
		comment is 'Creates a signed ephemeral-key Offer and PendingState for an initiator using IdentitySeed and the pinned ResponderIdentityPublicKey.',
		argnames is ['IdentitySeed', 'ResponderIdentityPublicKey', 'Offer', 'PendingState'],
		exceptions is [
			'``IdentitySeed`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``IdentitySeed`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'IdentitySeed'),
			'``IdentitySeed`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``IdentitySeed`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``ResponderIdentityPublicKey`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``ResponderIdentityPublicKey`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'ResponderIdentityPublicKey'),
			'``ResponderIdentityPublicKey`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``ResponderIdentityPublicKey`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(authenticated_channel_accept/5).
	:- mode(authenticated_channel_accept(+list(byte), +list(byte), +compound, -compound, -compound), zero_or_one_or_error).
	:- info(authenticated_channel_accept/5, [
		comment is 'Verifies Offer against the pinned InitiatorIdentityPublicKey, creates a signed Response, and returns the responder Channel state.',
		argnames is ['IdentitySeed', 'InitiatorIdentityPublicKey', 'Offer', 'Response', 'Channel'],
		exceptions is [
			'``IdentitySeed`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``IdentitySeed`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'IdentitySeed'),
			'``IdentitySeed`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``IdentitySeed`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``InitiatorIdentityPublicKey`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``InitiatorIdentityPublicKey`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'InitiatorIdentityPublicKey'),
			'``InitiatorIdentityPublicKey`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``InitiatorIdentityPublicKey`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Offer`` is a variable or contains a partial byte list or a byte list with an element which is a variable' - instantiation_error,
			'``Offer`` is neither a variable nor a compound term' - type_error(compound, 'Offer'),
			'``Offer`` is not a valid authenticated channel offer term' - domain_error(authenticated_channel_offer, 'Offer'),
			'``Offer`` contains a key field which is not a list of 32 bytes' - type_error(list(byte, 32), 'Bytes'),
			'``Offer`` contains a signature field which is not a list of 64 bytes' - type_error(list(byte, 64), 'Signature'),
			'``Offer`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Offer`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(authenticated_channel_finalize/3).
	:- mode(authenticated_channel_finalize(+compound, +compound, -compound), zero_or_one_or_error).
	:- info(authenticated_channel_finalize/3, [
		comment is 'Verifies Response against the identity pinned in PendingState and returns the initiator Channel state.',
		argnames is ['PendingState', 'Response', 'Channel'],
		exceptions is [
			'``PendingState`` is a variable or contains a partial byte list or a byte list with an element which is a variable' - instantiation_error,
			'``PendingState`` is neither a variable nor a compound term' - type_error(compound, 'PendingState'),
			'``PendingState`` is not a valid authenticated channel pending-state term' - domain_error(authenticated_channel_pending_state, 'PendingState'),
			'``PendingState`` contains a key field which is not a list of 32 bytes' - type_error(list(byte, 32), 'Bytes'),
			'``PendingState`` contains a signature field which is not a list of 64 bytes' - type_error(list(byte, 64), 'Signature'),
			'``PendingState`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``PendingState`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Response`` is a variable or contains a partial byte list or a byte list with an element which is a variable' - instantiation_error,
			'``Response`` is neither a variable nor a compound term' - type_error(compound, 'Response'),
			'``Response`` is not a valid authenticated channel response term' - domain_error(authenticated_channel_response, 'Response'),
			'``Response`` contains a key field which is not a list of 32 bytes' - type_error(list(byte, 32), 'Bytes'),
			'``Response`` contains a signature field which is not a list of 64 bytes' - type_error(list(byte, 64), 'Signature'),
			'``Response`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Response`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(authenticated_channel_encrypt/5).
	:- mode(authenticated_channel_encrypt(+compound, +list(byte), +list(byte), -compound, -compound), one_or_error).
	:- info(authenticated_channel_encrypt/5, [
		comment is 'Encrypts and authenticates Plaintext and returns Message and the replacement Channel state. The caller must discard the input state.',
		argnames is ['Channel0', 'AAD', 'Plaintext', 'Message', 'Channel'],
		exceptions is [
			'``Channel0`` is a variable or contains a partial byte list, a byte list with an element which is a variable, or a variable counter' - instantiation_error,
			'``Channel0`` is neither a variable nor a compound term' - type_error(compound, 'Channel0'),
			'``Channel0`` is not a valid authenticated channel state term' - domain_error(authenticated_channel_state, 'Channel0'),
			'``Channel0`` contains a hash or key field which is not a list of 32 bytes' - type_error(list(byte, 32), 'Bytes'),
			'``Channel0`` contains a nonce-prefix field which is not a list of 16 bytes' - type_error(list(byte, 16), 'Bytes'),
			'``Channel0`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Channel0`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Channel0`` contains a counter which is not an integer' - type_error(integer, 'Counter'),
			'``Channel0`` contains a negative counter' - domain_error(non_negative_integer, 'Counter'),
			'``AAD`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``AAD`` is neither a variable nor a list of bytes' - type_error(list(byte), 'AAD'),
			'``AAD`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``AAD`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Plaintext`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Plaintext`` is neither a variable nor a list of bytes' - type_error(list(byte), 'Plaintext'),
			'``Plaintext`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Plaintext`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Channel0`` send counter is exhausted' - resource_error(authenticated_channel_counter)
		]
	]).

	:- public(authenticated_channel_decrypt/5).
	:- mode(authenticated_channel_decrypt(+compound, +list(byte), +compound, -list(byte), -compound), zero_or_one_or_error).
	:- info(authenticated_channel_decrypt/5, [
		comment is 'Authenticates and decrypts the next strictly ordered Message and returns Plaintext and the replacement Channel state. The caller must discard the input state.',
		argnames is ['Channel0', 'AAD', 'Message', 'Plaintext', 'Channel'],
		exceptions is [
			'``Channel0`` is a variable or contains a partial byte list, a byte list with an element which is a variable, or a variable counter' - instantiation_error,
			'``Channel0`` is neither a variable nor a compound term' - type_error(compound, 'Channel0'),
			'``Channel0`` is not a valid authenticated channel state term' - domain_error(authenticated_channel_state, 'Channel0'),
			'``Channel0`` contains a hash or key field which is not a list of 32 bytes' - type_error(list(byte, 32), 'Bytes'),
			'``Channel0`` contains a nonce-prefix field which is not a list of 16 bytes' - type_error(list(byte, 16), 'Bytes'),
			'``Channel0`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Channel0`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Channel0`` contains a counter which is not an integer' - type_error(integer, 'Counter'),
			'``Channel0`` contains a negative counter' - domain_error(non_negative_integer, 'Counter'),
			'``AAD`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``AAD`` is neither a variable nor a list of bytes' - type_error(list(byte), 'AAD'),
			'``AAD`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``AAD`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Message`` is a variable or contains a partial byte list, a byte list with an element which is a variable, or a variable counter' - instantiation_error,
			'``Message`` is neither a variable nor a compound term' - type_error(compound, 'Message'),
			'``Message`` is not a valid authenticated channel message term' - domain_error(authenticated_channel_message, 'Message'),
			'``Message`` counter is not an integer' - type_error(integer, 'Counter'),
			'``Message`` counter is negative' - domain_error(non_negative_integer, 'Counter'),
			'``Message`` ciphertext and tag is not a list of bytes' - type_error(list(byte), 'CiphertextAndTag'),
			'``Message`` ciphertext and tag contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Message`` ciphertext and tag contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Message`` ciphertext and tag is shorter than 16 bytes' - domain_error(minimum_byte_length(16), 'CiphertextAndTag'),
			'``Channel0`` receive counter is exhausted' - resource_error(authenticated_channel_counter)
		]
	]).

	:- uses(list, [
		append/2, append/3, length/2
	]).

	:- uses(sha256, [
		digest/2
	]).

	:- uses(type, [
		check/3
	]).

	authenticated_channel_initiate(IdentitySeed, ResponderIdentityPublicKey, Offer, PendingState) :-
		context(Context),
		check(list(byte, 32), IdentitySeed, Context),
		check(list(byte, 32), ResponderIdentityPublicKey, Context),
		::ed25519_public_key(IdentitySeed, InitiatorIdentityPublicKey),
		::x25519_keypair(InitiatorEphemeralPrivateKey, InitiatorEphemeralPublicKey),
		offer_signed_bytes(InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, SignedBytes),
		::ed25519_sign(IdentitySeed, SignedBytes, Signature),
		Offer = authenticated_channel_offer(1, InitiatorIdentityPublicKey, InitiatorEphemeralPublicKey, Signature),
		PendingState = authenticated_channel_pending(1, InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPrivateKey, InitiatorEphemeralPublicKey, Signature).

	authenticated_channel_accept(IdentitySeed, InitiatorIdentityPublicKey, Offer, Response, Channel) :-
		context(Context),
		check(list(byte, 32), IdentitySeed, Context),
		check(list(byte, 32), InitiatorIdentityPublicKey, Context),
		check_offer(Offer, OfferIdentityPublicKey, InitiatorEphemeralPublicKey, OfferSignature, Context),
		OfferIdentityPublicKey == InitiatorIdentityPublicKey,
		::ed25519_public_key(IdentitySeed, ResponderIdentityPublicKey),
		offer_signed_bytes(InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, OfferSignedBytes),
		::ed25519_verify(InitiatorIdentityPublicKey, OfferSignedBytes, OfferSignature),
		::x25519_keypair(ResponderEphemeralPrivateKey, ResponderEphemeralPublicKey),
		response_signed_bytes(InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, ResponderEphemeralPublicKey, OfferSignature, ResponseSignedBytes),
		::ed25519_sign(IdentitySeed, ResponseSignedBytes, ResponseSignature),
		Response = authenticated_channel_response(1, ResponderIdentityPublicKey, ResponderEphemeralPublicKey, ResponseSignature),
		transcript_hash(InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, ResponderEphemeralPublicKey, OfferSignature, ResponseSignature, TranscriptHash),
		::x25519_shared_secret(ResponderEphemeralPrivateKey, InitiatorEphemeralPublicKey, SharedSecret),
		channel_material(SharedSecret, TranscriptHash, I2RChainKey, R2IChainKey, I2RNoncePrefix, R2INoncePrefix),
		Channel = authenticated_channel_state(responder, TranscriptHash, R2IChainKey, I2RChainKey, R2INoncePrefix, I2RNoncePrefix, 0, 0).

	authenticated_channel_finalize(PendingState, Response, Channel) :-
		context(Context),
		check_pending_state(PendingState, InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPrivateKey, InitiatorEphemeralPublicKey, OfferSignature, Context),
		check_response(Response, ResponseIdentityPublicKey, ResponderEphemeralPublicKey, ResponseSignature, Context),
		ResponseIdentityPublicKey == ResponderIdentityPublicKey,
		response_signed_bytes(InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, ResponderEphemeralPublicKey, OfferSignature, ResponseSignedBytes),
		::ed25519_verify(ResponderIdentityPublicKey, ResponseSignedBytes, ResponseSignature),
		transcript_hash(InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, ResponderEphemeralPublicKey, OfferSignature, ResponseSignature, TranscriptHash),
		::x25519_shared_secret(InitiatorEphemeralPrivateKey, ResponderEphemeralPublicKey, SharedSecret),
		channel_material(SharedSecret, TranscriptHash, I2RChainKey, R2IChainKey, I2RNoncePrefix, R2INoncePrefix),
		Channel = authenticated_channel_state(initiator, TranscriptHash, I2RChainKey, R2IChainKey, I2RNoncePrefix, R2INoncePrefix, 0, 0).

	authenticated_channel_encrypt(Channel0, AAD, Plaintext, Message, Channel) :-
		context(Context),
		check(list(byte), AAD, Context),
		check(list(byte), Plaintext, Context),
		check_channel_state(Channel0, Role, TranscriptHash, SendChainKey, ReceiveChainKey, SendNoncePrefix, ReceiveNoncePrefix, SendCounter, ReceiveCounter, Context),
		check_counter_available(SendCounter, Context),
		channel_direction(Role, send, Direction),
		message_material(SendChainKey, TranscriptHash, Direction, SendCounter, MessageKey, NextSendChainKey),
		counter_bytes(SendCounter, CounterBytes),
		append(SendNoncePrefix, CounterBytes, Nonce),
		message_aad(Direction, TranscriptHash, CounterBytes, AAD, AuthenticatedData),
		::xchacha20_poly1305_encrypt(MessageKey, Nonce, AuthenticatedData, Plaintext, CiphertextAndTag),
		Message = authenticated_channel_message(1, SendCounter, CiphertextAndTag),
		NextSendCounter is SendCounter + 1,
		Channel = authenticated_channel_state(Role, TranscriptHash, NextSendChainKey, ReceiveChainKey, SendNoncePrefix, ReceiveNoncePrefix, NextSendCounter, ReceiveCounter).

	authenticated_channel_decrypt(Channel0, AAD, Message, Plaintext, Channel) :-
		context(Context),
		check(list(byte), AAD, Context),
		check_channel_state(Channel0, Role, TranscriptHash, SendChainKey, ReceiveChainKey, SendNoncePrefix, ReceiveNoncePrefix, SendCounter, ReceiveCounter, Context),
		check_message(Message, Counter, CiphertextAndTag, Context),
		Counter =:= ReceiveCounter,
		check_counter_available(ReceiveCounter, Context),
		channel_direction(Role, receive, Direction),
		message_material(ReceiveChainKey, TranscriptHash, Direction, ReceiveCounter, MessageKey, NextReceiveChainKey),
		counter_bytes(ReceiveCounter, CounterBytes),
		append(ReceiveNoncePrefix, CounterBytes, Nonce),
		message_aad(Direction, TranscriptHash, CounterBytes, AAD, AuthenticatedData),
		::xchacha20_poly1305_decrypt(MessageKey, Nonce, AuthenticatedData, CiphertextAndTag, Plaintext),
		NextReceiveCounter is ReceiveCounter + 1,
		Channel = authenticated_channel_state(Role, TranscriptHash, SendChainKey, NextReceiveChainKey, SendNoncePrefix, ReceiveNoncePrefix, SendCounter, NextReceiveCounter).

	check_offer(Offer, IdentityPublicKey, EphemeralPublicKey, Signature, Context) :-
		check(compound, Offer, Context),
		(	Offer = authenticated_channel_offer(1, IdentityPublicKey, EphemeralPublicKey, Signature) ->
			check(list(byte, 32), IdentityPublicKey, Context),
			check(list(byte, 32), EphemeralPublicKey, Context),
			check(list(byte, 64), Signature, Context)
		;	throw(error(domain_error(authenticated_channel_offer, Offer), Context))
		).

	check_response(Response, IdentityPublicKey, EphemeralPublicKey, Signature, Context) :-
		check(compound, Response, Context),
		(	Response = authenticated_channel_response(1, IdentityPublicKey, EphemeralPublicKey, Signature) ->
			check(list(byte, 32), IdentityPublicKey, Context),
			check(list(byte, 32), EphemeralPublicKey, Context),
			check(list(byte, 64), Signature, Context)
		;	throw(error(domain_error(authenticated_channel_response, Response), Context))
		).

	check_pending_state(PendingState, InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPrivateKey, InitiatorEphemeralPublicKey, OfferSignature, Context) :-
		check(compound, PendingState, Context),
		(	PendingState = authenticated_channel_pending(1, InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPrivateKey, InitiatorEphemeralPublicKey, OfferSignature) ->
			check(list(byte, 32), InitiatorIdentityPublicKey, Context),
			check(list(byte, 32), ResponderIdentityPublicKey, Context),
			check(list(byte, 32), InitiatorEphemeralPrivateKey, Context),
			check(list(byte, 32), InitiatorEphemeralPublicKey, Context),
			check(list(byte, 64), OfferSignature, Context)
		;	throw(error(domain_error(authenticated_channel_pending_state, PendingState), Context))
		).

	check_channel_state(Channel, Role, TranscriptHash, SendChainKey, ReceiveChainKey, SendNoncePrefix, ReceiveNoncePrefix, SendCounter, ReceiveCounter, Context) :-
		check(compound, Channel, Context),
		(	Channel = authenticated_channel_state(Role, TranscriptHash, SendChainKey, ReceiveChainKey, SendNoncePrefix, ReceiveNoncePrefix, SendCounter, ReceiveCounter),
			channel_role(Role) ->
			check(list(byte, 32), TranscriptHash, Context),
			check(list(byte, 32), SendChainKey, Context),
			check(list(byte, 32), ReceiveChainKey, Context),
			check(list(byte, 16), SendNoncePrefix, Context),
			check(list(byte, 16), ReceiveNoncePrefix, Context),
			check(non_negative_integer, SendCounter, Context),
			check(non_negative_integer, ReceiveCounter, Context)
		;	throw(error(domain_error(authenticated_channel_state, Channel), Context))
		).

	check_message(Message, Counter, CiphertextAndTag, Context) :-
		check(compound, Message, Context),
		(	Message = authenticated_channel_message(1, Counter, CiphertextAndTag) ->
			check(non_negative_integer, Counter, Context),
			check(list(byte), CiphertextAndTag, Context),
			length(CiphertextAndTag, Length),
			(	Length >= 16 ->
				true
			;	throw(error(domain_error(minimum_byte_length(16), CiphertextAndTag), Context))
			)
		;	throw(error(domain_error(authenticated_channel_message, Message), Context))
		).

	channel_role(initiator) :-
		!.
	channel_role(responder).

	channel_direction(initiator, send, 0) :-
		!.
	channel_direction(initiator, receive, 1) :-
		!.
	channel_direction(responder, send, 1) :-
		!.
	channel_direction(responder, receive, 0).

	offer_signed_bytes(InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, Bytes) :-
		protocol_label(Label),
		append([Label, [0], InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey], Bytes).

	response_signed_bytes(InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, ResponderEphemeralPublicKey, OfferSignature, Bytes) :-
		protocol_label(Label),
		append([Label, [1], InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, ResponderEphemeralPublicKey, OfferSignature], Bytes).

	transcript_hash(InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, ResponderEphemeralPublicKey, OfferSignature, ResponseSignature, TranscriptHash) :-
		protocol_label(Label),
		append([Label, [2], InitiatorIdentityPublicKey, ResponderIdentityPublicKey, InitiatorEphemeralPublicKey, ResponderEphemeralPublicKey, OfferSignature, ResponseSignature], Transcript),
		digest(Transcript, TranscriptHash).

	channel_material(SharedSecret, TranscriptHash, I2RChainKey, R2IChainKey, I2RNoncePrefix, R2INoncePrefix) :-
		protocol_label(Label),
		append(Label, [3], Info),
		::hkdf(sha256, SharedSecret, 96, Material, [salt(TranscriptHash), info(Info)]),
		length(I2RChainKey, 32),
		append(I2RChainKey, Material1, Material),
		length(R2IChainKey, 32),
		append(R2IChainKey, Material2, Material1),
		length(I2RNoncePrefix, 16),
		append(I2RNoncePrefix, R2INoncePrefix, Material2).

	message_material(ChainKey, TranscriptHash, Direction, Counter, MessageKey, NextChainKey) :-
		protocol_label(Label),
		counter_bytes(Counter, CounterBytes),
		append([Label, [4, Direction], CounterBytes], Info),
		::hkdf(sha256, ChainKey, 64, Material, [salt(TranscriptHash), info(Info)]),
		length(MessageKey, 32),
		append(MessageKey, NextChainKey, Material).

	message_aad(Direction, TranscriptHash, CounterBytes, AAD, AuthenticatedData) :-
		protocol_label(Label),
		append([Label, [5, Direction], TranscriptHash, CounterBytes, AAD], AuthenticatedData).

	check_counter_available(Counter, Context) :-
		(	Counter < 18446744073709551615 ->
			true
		;	throw(error(resource_error(authenticated_channel_counter), Context))
		).

	counter_bytes(Counter, Bytes) :-
		length(Bytes, 8),
		counter_bytes_loop(Counter, Bytes).
	counter_bytes_loop(_Counter, []) :-
		!.
	counter_bytes_loop(Counter, [Byte| Bytes]) :-
		Byte is Counter /\ 0xff,
		NextCounter is Counter // 256,
		counter_bytes_loop(NextCounter, Bytes).

	protocol_label([108,111,103,116,97,108,107,45,99,114,121,112,116,111,45,97,117,116,104,101,110,116,105,99,97,116,101,100,45,99,104,97,110,110,101,108,45,118,49]).

:- end_category.
