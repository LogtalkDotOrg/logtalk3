%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@talk.org>
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


:- protocol(otp_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-14,
		comment is 'Protocol for HOTP and TOTP generation and verification as specified in RFC 4226 and RFC 6238.',
		see_also is [hmac_protocol]
	]).

	:- public(hotp/5).
	:- mode(hotp(+object_identifier, +term, +integer, +integer, --atom), one).
	:- info(hotp/5, [
		comment is 'Computes an HOTP value for the given hash object, secret, moving counter, and number of digits. The secret can be either a raw byte list or a Base32 wrapper term of the form ``base32(atom(Atom))``, ``base32(chars(Chars))``, or ``base32(codes(Codes))``.',
		argnames is ['Hash', 'Secret', 'Counter', 'Digits', 'OTP']
	]).

	:- public(totp/5).
	:- mode(totp(+object_identifier, +term, +integer, +integer, --atom), one).
	:- info(totp/5, [
		comment is 'Computes a TOTP value using the standard 30-second time step and Unix epoch ``T0 = 0`` for the given hash object, secret, Unix time, and number of digits.',
		argnames is ['Hash', 'Secret', 'UnixTime', 'Digits', 'OTP']
	]).

	:- public(hotp_verify/7).
	:- mode(hotp_verify(+object_identifier, +term, +integer, +integer, +integer, +atom, --integer), zero_or_one_or_error).
	:- info(hotp_verify/7, [
		comment is 'Verifies an HOTP value by searching from the given counter through the bounded forward counter window. Returns the matched counter on success.',
		argnames is ['Hash', 'Secret', 'Counter', 'Window', 'Digits', 'OTP', 'MatchedCounter']
	]).

	:- public(totp_verify/7).
	:- mode(totp_verify(+object_identifier, +term, +integer, +integer, +integer, +atom, --integer), zero_or_one_or_error).
	:- info(totp_verify/7, [
		comment is 'Verifies a TOTP value by searching within the bounded symmetric time-step window around the current time step. Returns the matched time step on success.',
		argnames is ['Hash', 'Secret', 'UnixTime', 'Window', 'Digits', 'OTP', 'MatchedTimeStep']
	]).

:- end_protocol.
