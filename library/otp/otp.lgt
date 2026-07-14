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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-14,
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

	hotp(Hash, Secret, Counter, Digits, OTP) :-
		check_hash(Hash),
		normalize_secret(Secret, SecretBytes),
		check_non_negative_integer(Counter),
		check_positive_integer(Digits),
		hotp_checked(Hash, SecretBytes, Counter, Digits, OTP).

	totp(Hash, Secret, UnixTime, Digits, OTP) :-
		check_hash(Hash),
		normalize_secret(Secret, SecretBytes),
		check_non_negative_integer(UnixTime),
		check_positive_integer(Digits),
		TimeStep is UnixTime // 30,
		hotp_checked(Hash, SecretBytes, TimeStep, Digits, OTP).

	hotp_verify(Hash, Secret, Counter, Window, Digits, OTP, MatchedCounter) :-
		check_hash(Hash),
		normalize_secret(Secret, SecretBytes),
		check_non_negative_integer(Counter),
		check_non_negative_integer(Window),
		check_positive_integer(Digits),
		check_otp_atom(OTP, Digits),
		hotp_verify_checked(Hash, SecretBytes, Counter, Window, Digits, OTP, MatchedCounter).

	totp_verify(Hash, Secret, UnixTime, Window, Digits, OTP, MatchedTimeStep) :-
		check_hash(Hash),
		normalize_secret(Secret, SecretBytes),
		check_non_negative_integer(UnixTime),
		check_non_negative_integer(Window),
		check_positive_integer(Digits),
		check_otp_atom(OTP, Digits),
		CurrentTimeStep is UnixTime // 30,
		totp_verify_checked(Hash, SecretBytes, CurrentTimeStep, Window, Digits, OTP, MatchedTimeStep).

	check_hash(Hash) :-
		(	var(Hash) ->
			instantiation_error
		;	current_object(Hash),
			catch(Hash::digest_size(DigestSize), _, fail),
			catch(Hash::block_size(_BlockSize), _, fail),
			catch(Hash::digest([], _DigestBytes), _, fail) ->
			(	DigestSize >= 20 ->
				true
			;	domain_error(otp_hash, Hash)
			)
		;	domain_error(otp_hash, Hash)
		).

	normalize_secret(Secret, SecretBytes) :-
		(	var(Secret) ->
			instantiation_error
		;	Secret = base32(atom(Atom)) ->
			parse(atom(Atom), SecretBytes)
		;	Secret = base32(chars(Chars)) ->
			parse(chars(Chars), SecretBytes)
		;	Secret = base32(codes(Codes)) ->
			parse(codes(Codes), SecretBytes)
		;	check_byte_list(Secret),
			SecretBytes = Secret
		).

	check_byte_list(Bytes) :-
		(	var(Bytes) ->
			instantiation_error
		;	Bytes == [] ->
			true
		;	Bytes = [Byte| Tail] ->
			check_byte(Byte),
			check_byte_list(Tail)
		;	type_error(list, Bytes)
		).

	check_byte(Byte) :-
		(	var(Byte) ->
			instantiation_error
		;	integer(Byte) ->
			(	0 =< Byte, Byte =< 255 ->
				true
			;	domain_error(byte, Byte)
			)
		;	type_error(integer, Byte)
		).

	check_non_negative_integer(Integer) :-
		(	var(Integer) ->
			instantiation_error
		;	integer(Integer) ->
			(	Integer >= 0 ->
				true
			;	domain_error(non_negative_integer, Integer)
			)
		;	type_error(integer, Integer)
		).

	check_positive_integer(Integer) :-
		(	var(Integer) ->
			instantiation_error
		;	integer(Integer) ->
			(	Integer > 0 ->
				true
			;	domain_error(positive_integer, Integer)
			)
		;	type_error(integer, Integer)
		).

	check_otp_atom(OTP, Digits) :-
		(	var(OTP) ->
			instantiation_error
		;	atom(OTP) ->
			atom_codes(OTP, Codes),
			(	length(Codes, Digits),
				all_digit_codes(Codes) ->
				true
			;	domain_error(otp_value, OTP)
			)
		;	type_error(atom, OTP)
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

	power_of_ten(1, 10) :-
		!.
	power_of_ten(Digits, Power) :-
		Digits > 1,
		NextDigits is Digits - 1,
		power_of_ten(NextDigits, NextPower),
		Power is NextPower * 10.

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
