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


:- protocol(paseto_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'PASETO v4.local and v4.public protocol for byte payloads.'
	]).

	:- public(local_key/1).
	:- mode(local_key(-list(byte)), one).
	:- info(local_key/1, [
		comment is 'Generates a 32-byte v4.local key.',
		argnames is ['Key']
	]).

	:- public(public_keypair/2).
	:- mode(public_keypair(-list(byte), -list(byte)), one).
	:- info(public_keypair/2, [
		comment is 'Generates an Ed25519 seed and public key for v4.public tokens.',
		argnames is ['Seed', 'PublicKey']
	]).

	:- public(local_encrypt/3).
	:- mode(local_encrypt(+list(byte), +list(byte), -atom), one_or_error).
	:- info(local_encrypt/3, [
		comment is 'Encrypts Payload using a 32-byte local Key and empty footer and implicit assertion.',
		argnames is ['Key', 'Payload', 'Token']
	]).

	:- public(local_encrypt/5).
	:- mode(local_encrypt(+list(byte), +list(byte), +list(byte), +list(byte), -atom), one_or_error).
	:- info(local_encrypt/5, [
		comment is 'Encrypts Payload using a 32-byte local Key and authenticates Footer and ImplicitAssertion.',
		argnames is ['Key', 'Payload', 'Footer', 'ImplicitAssertion', 'Token']
	]).

	:- public(local_decrypt/3).
	:- mode(local_decrypt(+atom, +list(byte), -list(byte)), zero_or_one_or_error).
	:- info(local_decrypt/3, [
		comment is 'Authenticates and decrypts a local Token using an empty implicit assertion.',
		argnames is ['Token', 'Key', 'Payload']
	]).

	:- public(local_decrypt/5).
	:- mode(local_decrypt(+atom, +list(byte), +list(byte), -list(byte), -list(byte)), zero_or_one_or_error).
	:- info(local_decrypt/5, [
		comment is 'Authenticates and decrypts a local Token using ImplicitAssertion and returns its authenticated Footer.',
		argnames is ['Token', 'Key', 'ImplicitAssertion', 'Payload', 'Footer']
	]).

	:- public(public_sign/3).
	:- mode(public_sign(+list(byte), +list(byte), -atom), one_or_error).
	:- info(public_sign/3, [
		comment is 'Signs Payload using an Ed25519 Seed and empty footer and implicit assertion.',
		argnames is ['Seed', 'Payload', 'Token']
	]).

	:- public(public_sign/5).
	:- mode(public_sign(+list(byte), +list(byte), +list(byte), +list(byte), -atom), one_or_error).
	:- info(public_sign/5, [
		comment is 'Signs Payload using an Ed25519 Seed and authenticates Footer and ImplicitAssertion.',
		argnames is ['Seed', 'Payload', 'Footer', 'ImplicitAssertion', 'Token']
	]).

	:- public(public_verify/3).
	:- mode(public_verify(+atom, +list(byte), -list(byte)), zero_or_one_or_error).
	:- info(public_verify/3, [
		comment is 'Authenticates a public Token using an Ed25519 public key and empty implicit assertion.',
		argnames is ['Token', 'PublicKey', 'Payload']
	]).

	:- public(public_verify/5).
	:- mode(public_verify(+atom, +list(byte), +list(byte), -list(byte), -list(byte)), zero_or_one_or_error).
	:- info(public_verify/5, [
		comment is 'Authenticates a public Token using an Ed25519 public key and ImplicitAssertion and returns its authenticated Footer.',
		argnames is ['Token', 'PublicKey', 'ImplicitAssertion', 'Payload', 'Footer']
	]).

	:- public(footer/2).
	:- mode(footer(+atom, -list(byte)), one_or_error).
	:- info(footer/2, [
		comment is 'Extracts a token footer without authenticating it. The result must only be used for pre-authentication key selection.',
		argnames is ['Token', 'Footer']
	]).

:- end_protocol.
