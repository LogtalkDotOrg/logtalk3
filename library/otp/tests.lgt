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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-14,
		comment is 'Unit tests for the "otp" library.'
	]).

	cover(otp).

	test(otp_md5_rejected, error(domain_error(otp_hash, md5))) :-
		rfc4226_secret(Secret),
		otp::hotp(md5, Secret, 0, 6, _).

	:- if(current_prolog_flag(bounded, false)).

	test(hotp_rfc4226_counter_0, deterministic(OTP == '755224')) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 0, 6, OTP).

	test(hotp_rfc4226_counter_1, deterministic(OTP == '287082')) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 1, 6, OTP).

	test(hotp_rfc4226_counter_2, deterministic(OTP == '359152')) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 2, 6, OTP).

	test(hotp_rfc4226_counter_3, deterministic(OTP == '969429')) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 3, 6, OTP).

	test(hotp_rfc4226_counter_4, deterministic(OTP == '338314')) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 4, 6, OTP).

	test(hotp_rfc4226_counter_5, deterministic(OTP == '254676')) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 5, 6, OTP).

	test(hotp_rfc4226_counter_6, deterministic(OTP == '287922')) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 6, 6, OTP).

	test(hotp_rfc4226_counter_7, deterministic(OTP == '162583')) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 7, 6, OTP).

	test(hotp_rfc4226_counter_8, deterministic(OTP == '399871')) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 8, 6, OTP).

	test(hotp_rfc4226_counter_9, deterministic(OTP == '520489')) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 9, 6, OTP).

	test(hotp_verify_window_match, deterministic(MatchedCounter == 9)) :-
		rfc4226_secret(Secret),
		otp::hotp_verify(sha1, Secret, 0, 9, 6, '520489', MatchedCounter).

	test(hotp_verify_window_miss, fail) :-
		rfc4226_secret(Secret),
		otp::hotp_verify(sha1, Secret, 0, 8, 6, '520489', _).

	test(hotp_base32_secret, deterministic(OTP == '755224')) :-
		otp::hotp(sha1, base32(atom('GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ')), 0, 6, OTP).

	test(totp_rfc6238_sha1_59, deterministic(OTP == '94287082')) :-
		rfc6238_sha1_secret(Secret),
		otp::totp(sha1, Secret, 59, 8, OTP).

	test(totp_rfc6238_sha1_1111111109, deterministic(OTP == '07081804')) :-
		rfc6238_sha1_secret(Secret),
		otp::totp(sha1, Secret, 1111111109, 8, OTP).

	test(totp_rfc6238_sha1_1111111111, deterministic(OTP == '14050471')) :-
		rfc6238_sha1_secret(Secret),
		otp::totp(sha1, Secret, 1111111111, 8, OTP).

	test(totp_rfc6238_sha1_1234567890, deterministic(OTP == '89005924')) :-
		rfc6238_sha1_secret(Secret),
		otp::totp(sha1, Secret, 1234567890, 8, OTP).

	test(totp_rfc6238_sha1_2000000000, deterministic(OTP == '69279037')) :-
		rfc6238_sha1_secret(Secret),
		otp::totp(sha1, Secret, 2000000000, 8, OTP).

	test(totp_rfc6238_sha1_20000000000, deterministic(OTP == '65353130')) :-
		rfc6238_sha1_secret(Secret),
		otp::totp(sha1, Secret, 20000000000, 8, OTP).

	test(totp_rfc6238_sha256_59, deterministic(OTP == '46119246')) :-
		rfc6238_sha256_secret(Secret),
		otp::totp(sha256, Secret, 59, 8, OTP).

	test(totp_rfc6238_sha256_1111111109, deterministic(OTP == '68084774')) :-
		rfc6238_sha256_secret(Secret),
		otp::totp(sha256, Secret, 1111111109, 8, OTP).

	test(totp_rfc6238_sha256_1111111111, deterministic(OTP == '67062674')) :-
		rfc6238_sha256_secret(Secret),
		otp::totp(sha256, Secret, 1111111111, 8, OTP).

	test(totp_rfc6238_sha256_1234567890, deterministic(OTP == '91819424')) :-
		rfc6238_sha256_secret(Secret),
		otp::totp(sha256, Secret, 1234567890, 8, OTP).

	test(totp_rfc6238_sha256_2000000000, deterministic(OTP == '90698825')) :-
		rfc6238_sha256_secret(Secret),
		otp::totp(sha256, Secret, 2000000000, 8, OTP).

	test(totp_rfc6238_sha256_20000000000, deterministic(OTP == '77737706')) :-
		rfc6238_sha256_secret(Secret),
		otp::totp(sha256, Secret, 20000000000, 8, OTP).

	test(totp_rfc6238_sha512_59, deterministic(OTP == '90693936')) :-
		rfc6238_sha512_secret(Secret),
		otp::totp(sha512, Secret, 59, 8, OTP).

	test(totp_rfc6238_sha512_1111111109, deterministic(OTP == '25091201')) :-
		rfc6238_sha512_secret(Secret),
		otp::totp(sha512, Secret, 1111111109, 8, OTP).

	test(totp_rfc6238_sha512_1111111111, deterministic(OTP == '99943326')) :-
		rfc6238_sha512_secret(Secret),
		otp::totp(sha512, Secret, 1111111111, 8, OTP).

	test(totp_rfc6238_sha512_1234567890, deterministic(OTP == '93441116')) :-
		rfc6238_sha512_secret(Secret),
		otp::totp(sha512, Secret, 1234567890, 8, OTP).

	test(totp_rfc6238_sha512_2000000000, deterministic(OTP == '38618901')) :-
		rfc6238_sha512_secret(Secret),
		otp::totp(sha512, Secret, 2000000000, 8, OTP).

	test(totp_rfc6238_sha512_20000000000, deterministic(OTP == '47863826')) :-
		rfc6238_sha512_secret(Secret),
		otp::totp(sha512, Secret, 20000000000, 8, OTP).

	test(totp_verify_current_step, deterministic(MatchedTimeStep == 1)) :-
		rfc6238_sha1_secret(Secret),
		otp::totp_verify(sha1, Secret, 59, 0, 8, '94287082', MatchedTimeStep).

	test(totp_verify_neighbor_step, deterministic(MatchedTimeStep == 1)) :-
		rfc6238_sha1_secret(Secret),
		otp::totp_verify(sha1, Secret, 29, 1, 8, '94287082', MatchedTimeStep).

	test(totp_verify_window_miss, fail) :-
		rfc6238_sha1_secret(Secret),
		otp::totp_verify(sha1, Secret, 29, 0, 8, '94287082', _).

	test(totp_invalid_otp_atom, error(domain_error(otp_value, abcdefgh))) :-
		rfc6238_sha1_secret(Secret),
		otp::totp_verify(sha1, Secret, 59, 0, 8, abcdefgh, _).

	test(hotp_invalid_window, error(domain_error(non_negative_integer, -1))) :-
		rfc4226_secret(Secret),
		otp::hotp_verify(sha1, Secret, 0, -1, 6, '755224', _).

	:- else.

	test(otp_sha1_unavailable_on_bounded_backend, error(domain_error(otp_hash, sha1))) :-
		rfc4226_secret(Secret),
		otp::hotp(sha1, Secret, 0, 6, _).

	test(otp_sha256_unavailable_on_bounded_backend, error(domain_error(otp_hash, sha256))) :-
		rfc6238_sha256_secret(Secret),
		otp::totp(sha256, Secret, 59, 8, _).

	test(otp_sha512_unavailable_on_bounded_backend, error(domain_error(otp_hash, sha512))) :-
		rfc6238_sha512_secret(Secret),
		otp::totp(sha512, Secret, 59, 8, _).

	:- endif.

	% auxiliary predicates

	rfc4226_secret(Secret) :-
		atom_codes('12345678901234567890', Secret).

	rfc6238_sha1_secret(Secret) :-
		rfc4226_secret(Secret).

	rfc6238_sha256_secret(Secret) :-
		atom_codes('12345678901234567890123456789012', Secret).

	rfc6238_sha512_secret(Secret) :-
		atom_codes('1234567890123456789012345678901234567890123456789012345678901234', Secret).

:- end_object.
