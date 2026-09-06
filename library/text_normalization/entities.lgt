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


:- category(text_entities).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Portable character reference decoding rules over lists of Unicode code points.',
		see_also is [text_normalization_profile_protocol, text_normalizer(_, _)]
	]).

	:- protected(decode_entities_codes/4).
	:- mode(decode_entities_codes(+list(integer), +object_identifier, +atom, -list(integer)), one_or_error).
	:- info(decode_entities_codes/4, [
		comment is 'Decodes semicolon-terminated numeric and profile-defined named character references. The unknown-name policy is ``preserve`` or ``error``.',
		argnames is ['Codes', 'Profile', 'Unknown', 'Decoded'],
		exceptions is [
			'A numeric reference denotes a value that is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Value'),
			'A named reference is unknown and the policy is ``error``' - domain_error(character_reference, 'Reference')
		]
	]).

	:- uses(list, [
		append/3
	]).

	decode_entities_codes([], _, _, []) :-
		!.
	decode_entities_codes([38| Codes], Profile, Unknown, Decoded) :-
		!,
		(	entity_reference(Codes, Reference, Rest) ->
			(	decode_reference(Reference, Profile, Replacement) ->
				append(Replacement, DecodedRest, Decoded),
				decode_entities_codes(Rest, Profile, Unknown, DecodedRest)
			;	named_reference(Reference) ->
				handle_unknown_reference(Unknown, Reference, Rest, Profile, Decoded)
			;	preserve_reference(Reference, Rest, Profile, Unknown, Decoded)
			)
		;	Decoded = [38| DecodedRest],
			decode_entities_codes(Codes, Profile, Unknown, DecodedRest)
		).
	decode_entities_codes([Code| Codes], Profile, Unknown, [Code| Decoded]) :-
		Code =\= 38,
		decode_entities_codes(Codes, Profile, Unknown, Decoded).

	entity_reference([59| Rest], [], Rest) :-
		!.
	entity_reference([38| _], _, _) :-
		!,
		fail.
	entity_reference([Code| Codes], [Code| Reference], Rest) :-
		entity_reference(Codes, Reference, Rest).

	decode_reference([35, 120| Digits], _, [Value]) :-
		digits_value(Digits, 16, Value),
		check_unicode_scalar(Value),
		!.
	decode_reference([35, 88| Digits], _, [Value]) :-
		digits_value(Digits, 16, Value),
		check_unicode_scalar(Value),
		!.
	decode_reference([35| Digits], _, [Value]) :-
		digits_value(Digits, 10, Value),
		check_unicode_scalar(Value),
		!.
	decode_reference(Reference, Profile, Replacement) :-
		catch(atom_codes(Name, Reference), _, fail),
		Profile::named_entity(Name, Replacement),
		!.

	named_reference([Code| Codes]) :-
		ascii_letter(Code),
		named_reference_rest(Codes).

	named_reference_rest([]).
	named_reference_rest([Code| Codes]) :-
		( ascii_letter(Code); Code >= 48, Code =< 57 ),
		named_reference_rest(Codes).

	ascii_letter(Code) :-
		Code >= 65,
		Code =< 90,
		!.
	ascii_letter(Code) :-
		Code >= 97,
		Code =< 122.

	digits_value([Digit| Digits], Base, Value) :-
		digit_value(Digit, Base, DigitValue),
		!,
		digits_value(Digits, Base, DigitValue, Value).

	digits_value([], _, Value, Value).
	digits_value([Digit| Digits], Base, Value0, Value) :-
		digit_value(Digit, Base, DigitValue),
		Value1 is Value0 * Base + DigitValue,
		digits_value(Digits, Base, Value1, Value).

	digit_value(Digit, 10, Value) :-
		Digit >= 48,
		Digit =< 57,
		Value is Digit - 48.
	digit_value(Digit, 16, Value) :-
		(	Digit >= 48, Digit =< 57 ->
			Value is Digit - 48
		;	Digit >= 65, Digit =< 70 ->
			Value is Digit - 55
		;	Digit >= 97, Digit =< 102,
			Value is Digit - 87
		).

	check_unicode_scalar(Value) :-
		(	Value >= 0, Value =< 0x10FFFF, (Value < 0xD800; Value > 0xDFFF) ->
			true
		;	domain_error(unicode_scalar_value, Value)
		).

	handle_unknown_reference(preserve, Reference, Rest, Profile, Decoded) :-
		!,
		preserve_reference(Reference, Rest, Profile, preserve, Decoded).
	handle_unknown_reference(error, Reference, _, _, _) :-
		domain_error(character_reference, Reference).

	preserve_reference(Reference, Rest, Profile, Unknown, [38| Decoded]) :-
		append(Reference, [59| DecodedRest], Decoded),
		decode_entities_codes(Rest, Profile, Unknown, DecodedRest).

:- end_category.
