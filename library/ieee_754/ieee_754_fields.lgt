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


:- object(ieee_754_fields(_Precision_, _ByteOrder_),
	implements(ieee_754_fields_protocol),
	imports(ieee_754_common(_Precision_, _ByteOrder_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'IEEE 754 exact bit-field classifier and decomposition object.',
		parameters is [
			'Precision' - 'Floating-point format precision. Supported values are ``half``, ``single``, and ``double``.',
			'ByteOrder' - 'Byte order used when reading byte sequences. Supported values are ``big`` and ``little``.'
		]
	]).

	classify(Source, Class) :-
		^^source_bits(Source, Bits),
		^^bits_fields(Bits, _Sign, _ExponentBits, _MantissaBits, Class).

	fields(Source, Sign, ExponentBits, MantissaBits, Class) :-
		^^source_bits(Source, Bits),
		^^bits_fields(Bits, Sign, ExponentBits, MantissaBits, Class).

	finite_binary_rational(Source, Sign, Significand, Exponent) :-
		^^source_bits(Source, Bits),
		(   ^^bits_finite_binary_rational(Bits, Sign, Significand, Exponent) ->
			true
		;   domain_error(ieee_754_finite_encoding, Source)
		).

	nan_payload(Source, PayloadBits) :-
		^^source_bits(Source, Bits),
		(   ^^bits_nan_payload(Bits, PayloadBits) ->
			true
		;   domain_error(ieee_754_nan_encoding, Source)
		).

	nan_kind(Source, Kind) :-
		^^source_bits(Source, Bits),
		(   ^^bits_nan_kind(Bits, Kind) ->
			true
		;   domain_error(ieee_754_nan_encoding, Source)
		).

	precision(_Precision_).

	order(_ByteOrder_).

	byte_count(Count) :-
		^^precision_spec(_Precision_, _, _, _, Count).

:- end_object.
