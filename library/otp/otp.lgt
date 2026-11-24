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


:- object(otp,
	implements(otp_protocol)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-11-24,
		comment is 'HOTP and TOTP generation and verification predicates.',
		see_also is [base32, hmac, sha1, sha256, sha512_256]
	]).

	:- uses(base32, [
		parse/2
	]).

	:- uses(hmac, [
		digest/4
	]).

	:- uses(list, [
		append/3, length/2
	]).

	:- uses(type, [
		check/3
	]).

	hotp(Hash, Secret, Counter, Digits, OTP) :-
		context(Context),
		check_hash(Hash, Context),
		normalize_secret(Secret, SecretBytes, Context),
		check(non_negative_integer, Counter, Context),
		check(positive_integer, Digits, Context),
		hotp_checked(Hash, SecretBytes, Counter, Digits, OTP).

	totp(Hash, Secret, UnixTime, Digits, OTP) :-
		context(Context),
		check_hash(Hash, Context),
		normalize_secret(Secret, SecretBytes, Context),
		check(non_negative_integer, UnixTime, Context),
		check(positive_integer, Digits, Context),
		TimeStep is UnixTime // 30,
		hotp_checked(Hash, SecretBytes, TimeStep, Digits, OTP).

	hotp_verify(Hash, Secret, Counter, Window, Digits, OTP, MatchedCounter) :-
		context(Context),
		check_hash(Hash, Context),
		normalize_secret(Secret, SecretBytes, Context),
		check(non_negative_integer, Counter, Context),
		check(non_negative_integer, Window, Context),
		check(positive_integer, Digits, Context),
		check_otp_atom(OTP, Digits, Context),
		hotp_verify_checked(Hash, SecretBytes, Counter, Window, Digits, OTP, MatchedCounter).

	totp_verify(Hash, Secret, UnixTime, Window, Digits, OTP, MatchedTimeStep) :-
		context(Context),
		check_hash(Hash, Context),
		normalize_secret(Secret, SecretBytes, Context),
		check(non_negative_integer, UnixTime, Context),
		check(non_negative_integer, Window, Context),
		check(positive_integer, Digits, Context),
		check_otp_atom(OTP, Digits, Context),
		CurrentTimeStep is UnixTime // 30,
		totp_verify_checked(Hash, SecretBytes, CurrentTimeStep, Window, Digits, OTP, MatchedTimeStep).

	check_hash(Hash, Context) :-
		(	var(Hash) ->
			throw(error(instantiation_error, Context))
		;	\+ callable(Hash) ->
			throw(error(type_error(callable, Hash), Context))
		;	current_object(Hash),
			catch(Hash::digest_size(DigestSize), _, fail),
			catch(Hash::block_size(_BlockSize), _, fail),
			catch(Hash::digest([], _DigestBytes), _, fail) ->
			(	DigestSize >= 20 ->
				true
			;	throw(error(domain_error(otp_hash, Hash), Context))
			)
		;	throw(error(existence_error(otp_hash, Hash), Context))
		).

	normalize_secret(Secret, SecretBytes, Context) :-
		(	var(Secret) ->
			throw(error(instantiation_error, Context))
		;	Secret = base32(atom(Atom)) ->
			parse(atom(Atom), SecretBytes)
		;	Secret = base32(chars(Chars)) ->
			parse(chars(Chars), SecretBytes)
		;	Secret = base32(codes(Codes)) ->
			parse(codes(Codes), SecretBytes)
		;	check(list(byte), Secret, Context),
			SecretBytes = Secret
		).

	check_otp_atom(OTP, Digits, Context) :-
		(	var(OTP) ->
			throw(error(instantiation_error, Context))
		;	atom(OTP) ->
			atom_codes(OTP, Codes),
			(	length(Codes, Digits),
				all_digit_codes(Codes) ->
				true
			;	throw(error(domain_error(otp_value, OTP), Context))
			)
		;	throw(error(type_error(atom, OTP), Context))
		).

	all_digit_codes([]).
	all_digit_codes([Code| Codes]) :-
		Code >= 0'0,
		Code =< 0'9,
		all_digit_codes(Codes).

	hotp_checked(Hash, SecretBytes, Counter, Digits, OTP) :-
		moving_factor_bytes(Counter, CounterBytes),
		digest(Hash, SecretBytes, CounterBytes, DigestBytes),
		dynamic_truncation(DigestBytes, BinaryCode),
		power_of_ten(Digits, Modulus),
		Value is BinaryCode mod Modulus,
		otp_atom(Value, Digits, OTP).

	hotp_verify_checked(Hash, SecretBytes, Counter, Window, Digits, OTP, MatchedCounter) :-
		hotp_checked(Hash, SecretBytes, Counter, Digits, CandidateOTP),
		(	CandidateOTP == OTP ->
			MatchedCounter = Counter
		;	Window > 0,
			NextCounter is Counter + 1,
			NextWindow is Window - 1,
			hotp_verify_checked(Hash, SecretBytes, NextCounter, NextWindow, Digits, OTP, MatchedCounter)
		).

	totp_verify_checked(Hash, SecretBytes, CurrentTimeStep, Window, Digits, OTP, MatchedTimeStep) :-
		totp_verify_checked(0, Hash, SecretBytes, CurrentTimeStep, Window, Digits, OTP, MatchedTimeStep).

	totp_verify_checked(Distance, Hash, SecretBytes, CurrentTimeStep, _Window, Digits, OTP, MatchedTimeStep) :-
		candidate_time_step(Distance, CurrentTimeStep, CandidateTimeStep),
		hotp_checked(Hash, SecretBytes, CandidateTimeStep, Digits, CandidateOTP),
		CandidateOTP == OTP,
		!,
		MatchedTimeStep = CandidateTimeStep.
	totp_verify_checked(Distance, Hash, SecretBytes, CurrentTimeStep, Window, Digits, OTP, MatchedTimeStep) :-
		Distance < Window,
		NextDistance is Distance + 1,
		totp_verify_checked(NextDistance, Hash, SecretBytes, CurrentTimeStep, Window, Digits, OTP, MatchedTimeStep).

	candidate_time_step(0, CurrentTimeStep, CurrentTimeStep).
	candidate_time_step(Distance, CurrentTimeStep, CandidateTimeStep) :-
		Distance > 0,
		PreviousTimeStep is CurrentTimeStep - Distance,
		PreviousTimeStep >= 0,
		CandidateTimeStep = PreviousTimeStep.
	candidate_time_step(Distance, CurrentTimeStep, CandidateTimeStep) :-
		Distance > 0,
		CandidateTimeStep is CurrentTimeStep + Distance.

	moving_factor_bytes(Counter, CounterBytes) :-
		moving_factor_bytes(Counter, 8, CounterBytes, Counter).

	moving_factor_bytes(0, 0, [], _) :-
		!.
	moving_factor_bytes(Value, 0, _, Counter) :-
		Value =\= 0,
		domain_error(otp_moving_factor, Counter).
	moving_factor_bytes(Value, RemainingBytes, Bytes, Counter) :-
		RemainingBytes > 0,
		Quotient is Value // 256,
		Byte is Value mod 256,
		NextRemainingBytes is RemainingBytes - 1,
		moving_factor_bytes(Quotient, NextRemainingBytes, Prefix, Counter),
		append(Prefix, [Byte], Bytes).

	dynamic_truncation(DigestBytes, BinaryCode) :-
		last_byte(DigestBytes, LastByte),
		Offset is LastByte /\ 0x0f,
		four_bytes_at(Offset, DigestBytes, Byte0, Byte1, Byte2, Byte3),
		MaskedByte0 is Byte0 /\ 0x7f,
		BinaryCode is (((MaskedByte0 * 256 + Byte1) * 256 + Byte2) * 256 + Byte3).

	last_byte([Byte], Byte) :-
		!.
	last_byte([_| Bytes], Byte) :-
		last_byte(Bytes, Byte).

	four_bytes_at(0, [Byte0, Byte1, Byte2, Byte3| _], Byte0, Byte1, Byte2, Byte3) :-
		!.
	four_bytes_at(Offset, [_| Bytes], Byte0, Byte1, Byte2, Byte3) :-
		NextOffset is Offset - 1,
		four_bytes_at(NextOffset, Bytes, Byte0, Byte1, Byte2, Byte3).

	power_of_ten(Exponent, Value) :-
		power_of_ten(Exponent, 1, Value).

	power_of_ten(0, Value, Value) :-
		!.
	power_of_ten(Exponent, Value0, Value) :-
		Exponent > 0,
		Exponent1 is Exponent - 1,
		Value1 is Value0 * 10,
		power_of_ten(Exponent1, Value1, Value).

	otp_atom(Value, Digits, OTP) :-
		number_codes(Value, ValueCodes),
		length(ValueCodes, ValueLength),
		ZeroCount is Digits - ValueLength,
		zero_codes(ZeroCount, ZeroCodes),
		append(ZeroCodes, ValueCodes, OTPCodes),
		atom_codes(OTP, OTPCodes).

	zero_codes(0, []) :-
		!.
	zero_codes(Count, [0'0| Codes]) :-
		NextCount is Count - 1,
		zero_codes(NextCount, Codes).

:- end_object.
