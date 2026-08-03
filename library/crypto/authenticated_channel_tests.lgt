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


:- object(authenticated_channel_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Unit tests for the "crypto" library authenticated channel implementation.'
	]).

	:- uses(crypto, [
		hex_bytes/2, ed25519_sign/3,
		authenticated_channel_initiate/4, authenticated_channel_accept/5,
		authenticated_channel_finalize/3, authenticated_channel_encrypt/5,
		authenticated_channel_decrypt/5
	]).

	:- uses(list, [
		append/2, append/3
	]).

	:- private(channel_fixture_/5).
	:- dynamic(channel_fixture_/5).
	:- mode(channel_fixture_(?compound, ?compound, ?compound, ?compound, ?compound), zero_or_one).
	:- info(channel_fixture_/5, [
		comment is 'Stores a complete authenticated handshake and its initial channel states for reuse by the test set.',
		argnames is ['Offer', 'PendingState', 'Response', 'AliceChannel', 'BobChannel']
	]).

	cover(authenticated_channel).

	setup :-
		build_channels(Offer, PendingState, Response, AliceChannel, BobChannel),
		assertz(channel_fixture_(Offer, PendingState, Response, AliceChannel, BobChannel)).

	cleanup :-
		retractall(channel_fixture_(_, _, _, _, _)).

	test(crypto_authenticated_channel_01, deterministic(Plaintext == [72,105])) :-
		setup_channels(AliceChannel0, BobChannel0),
		authenticated_channel_encrypt(AliceChannel0, [], [72,105], Message, _AliceChannel1),
		authenticated_channel_decrypt(BobChannel0, [], Message, Plaintext, _BobChannel1).

	test(crypto_authenticated_channel_02, deterministic(AlicePlaintext-BobPlaintext == [97]-[98])) :-
		setup_channels(AliceChannel0, BobChannel0),
		authenticated_channel_encrypt(AliceChannel0, [1], [98], AliceMessage, AliceChannel1),
		authenticated_channel_decrypt(BobChannel0, [1], AliceMessage, BobPlaintext, BobChannel1),
		authenticated_channel_encrypt(BobChannel1, [2], [97], BobMessage, BobChannel2),
		authenticated_channel_decrypt(AliceChannel1, [2], BobMessage, AlicePlaintext, AliceChannel2),
		authenticated_channel_encrypt(AliceChannel2, [], [99], LastMessage, _AliceChannel3),
		authenticated_channel_decrypt(BobChannel2, [], LastMessage, [99], _BobChannel3).

	test(crypto_authenticated_channel_03, deterministic(Message1 \== Message2)) :-
		setup_channels(AliceChannel0, _BobChannel0),
		authenticated_channel_encrypt(AliceChannel0, [], [1,2,3], Message1, AliceChannel1),
		authenticated_channel_encrypt(AliceChannel1, [], [1,2,3], Message2, _AliceChannel2).

	test(crypto_authenticated_channel_04, fail) :-
		setup_channels(AliceChannel0, BobChannel0),
		authenticated_channel_encrypt(AliceChannel0, [], [1], Message, _AliceChannel1),
		authenticated_channel_decrypt(BobChannel0, [], Message, _Plaintext, BobChannel1),
		authenticated_channel_decrypt(BobChannel1, [], Message, _ReplayPlaintext, _BobChannel2).

	test(crypto_authenticated_channel_05, fail) :-
		setup_channels(AliceChannel0, BobChannel0),
		authenticated_channel_encrypt(AliceChannel0, [], [1], _Message0, AliceChannel1),
		authenticated_channel_encrypt(AliceChannel1, [], [2], Message1, _AliceChannel2),
		authenticated_channel_decrypt(BobChannel0, [], Message1, _Plaintext, _BobChannel1).

	test(crypto_authenticated_channel_06, deterministic(Plaintext == [1])) :-
		setup_channels(AliceChannel0, BobChannel0),
		authenticated_channel_encrypt(AliceChannel0, [1], [1], Message, _AliceChannel1),
		\+ authenticated_channel_decrypt(BobChannel0, [2], Message, _WrongPlaintext, _WrongBobChannel),
		authenticated_channel_decrypt(BobChannel0, [1], Message, Plaintext, _BobChannel1).

	test(crypto_authenticated_channel_07, fail) :-
		identity_keys(_AliceSeed, _AlicePublicKey, BobSeed, BobPublicKey),
		channel_fixture_(Offer, _PendingState, _Response, _AliceChannel, _BobChannel),
		authenticated_channel_accept(BobSeed, BobPublicKey, Offer, _Response, _BobChannel).

	test(crypto_authenticated_channel_08, fail) :-
		third_identity(_CharlieSeed, CharliePublicKey),
		channel_fixture_(_Offer, PendingState, authenticated_channel_response(1, _BobPublicKey, EphemeralPublicKey, Signature), _AliceChannel0, _BobChannel0),
		Response = authenticated_channel_response(1, CharliePublicKey, EphemeralPublicKey, Signature),
		authenticated_channel_finalize(PendingState, Response, _AliceChannel1).

	test(crypto_authenticated_channel_09, fail) :-
		identity_keys(AliceSeed, AlicePublicKey, BobSeed, BobPublicKey),
		channel_fixture_(_Offer1, PendingState1, _Response1, _AliceChannel1, _BobChannel1),
		authenticated_channel_initiate(AliceSeed, BobPublicKey, Offer2, _PendingState2),
		authenticated_channel_accept(BobSeed, AlicePublicKey, Offer2, Response2, _BobChannel2),
		authenticated_channel_finalize(PendingState1, Response2, _AliceChannel).

	test(crypto_authenticated_channel_10, fail) :-
		setup_channels(AliceChannel0, BobChannel0),
		authenticated_channel_encrypt(AliceChannel0, [], [1], authenticated_channel_message(1, Counter, CiphertextAndTag), _AliceChannel1),
		flip_last_byte(CiphertextAndTag, TamperedCiphertextAndTag),
		TamperedMessage = authenticated_channel_message(1, Counter, TamperedCiphertextAndTag),
		authenticated_channel_decrypt(BobChannel0, [], TamperedMessage, _Plaintext, _BobChannel1).

	test(crypto_authenticated_channel_11, error(resource_error(authenticated_channel_counter))) :-
		setup_channels(authenticated_channel_state(Role, TranscriptHash, SendChainKey, ReceiveChainKey, SendNoncePrefix, ReceiveNoncePrefix, _SendCounter, ReceiveCounter), _BobChannel0),
		ExhaustedChannel = authenticated_channel_state(Role, TranscriptHash, SendChainKey, ReceiveChainKey, SendNoncePrefix, ReceiveNoncePrefix, 18446744073709551615, ReceiveCounter),
		authenticated_channel_encrypt(ExhaustedChannel, [], [], _Message, _Channel).

	test(crypto_authenticated_channel_12, fail) :-
		identity_keys(AliceSeed, AlicePublicKey, BobSeed, BobPublicKey),
		low_order_public_key(LowOrderPublicKey),
		protocol_label(Label),
		append([Label, [0], AlicePublicKey, BobPublicKey, LowOrderPublicKey], SignedBytes),
		ed25519_sign(AliceSeed, SignedBytes, Signature),
		Offer = authenticated_channel_offer(1, AlicePublicKey, LowOrderPublicKey, Signature),
		authenticated_channel_accept(BobSeed, AlicePublicKey, Offer, _Response, _BobChannel).

	% auxiliary predicates

	setup_channels(AliceChannel, BobChannel) :-
		channel_fixture_(_Offer, _PendingState, _Response, AliceChannel, BobChannel),
		!.

	build_channels(Offer, PendingState, Response, AliceChannel, BobChannel) :-
		identity_keys(AliceSeed, AlicePublicKey, BobSeed, BobPublicKey),
		authenticated_channel_initiate(AliceSeed, BobPublicKey, Offer, PendingState),
		authenticated_channel_accept(BobSeed, AlicePublicKey, Offer, Response, BobChannel),
		authenticated_channel_finalize(PendingState, Response, AliceChannel).

	identity_keys(AliceSeed, AlicePublicKey, BobSeed, BobPublicKey) :-
		hex_bytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', AliceSeed),
		hex_bytes('d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a', AlicePublicKey),
		hex_bytes('4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb', BobSeed),
		hex_bytes('3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c', BobPublicKey).

	third_identity(Seed, PublicKey) :-
		hex_bytes('c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7', Seed),
		hex_bytes('fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025', PublicKey).

	flip_last_byte(Bytes, TamperedBytes) :-
		append(Prefix, [Last], Bytes),
		TamperedLast is xor(Last, 1),
		append(Prefix, [TamperedLast], TamperedBytes).

	low_order_public_key([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).

	protocol_label([108,111,103,116,97,108,107,45,99,114,121,112,116,111,45,97,117,116,104,101,110,116,105,99,97,116,101,100,45,99,104,97,110,110,101,108,45,118,49]).

:- end_object.
