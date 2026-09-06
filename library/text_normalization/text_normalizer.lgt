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


:- object(text_normalizer(_Representation_, _Profile_),
	implements(text_normalizer_protocol),
	imports([options, text_diacritics, text_case_folding, text_entities, text_whitespace])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Text normalizer parameterized by text representation and normalization profile.',
		parameters is [
			'Representation' - 'Text representation. Valid values are ``atom``, ``chars``, and ``codes``.',
			'Profile' - 'Object implementing the ``text_normalization_profile_protocol`` protocol.'
		],
		see_also is [text_normalizer_protocol, text_normalization_profile_protocol, default_text_normalization]
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	normalize_unicode(Form, Text, Normalized) :-
		prepare_text(Text, Codes),
		check_form(Form),
		^^normalize_unicode_codes(Form, Codes, NormalizedCodes),
		codes_text(NormalizedCodes, Normalized).

	remove_diacritics(Text, Normalized) :-
		prepare_text(Text, Codes),
		^^remove_diacritics_codes(Codes, NormalizedCodes),
		codes_text(NormalizedCodes, Normalized).

	fold_diacritics(Text, Normalized) :-
		prepare_text(Text, Codes),
		^^fold_diacritics_codes(Codes, _Profile_, NormalizedCodes),
		codes_text(NormalizedCodes, Normalized).

	case_fold(Text, Folded) :-
		case_conversion(fold, Text, Folded).

	lower_case(Text, Lowercase) :-
		case_conversion(lower, Text, Lowercase).

	upper_case(Text, Uppercase) :-
		case_conversion(upper, Text, Uppercase).

	title_case(Text, Titlecase) :-
		case_conversion(title, Text, Titlecase).

	case_conversion(Mode, Text, Converted) :-
		prepare_text(Text, Codes),
		^^convert_case_codes(Mode, Codes, _Profile_, ConvertedCodes),
		codes_text(ConvertedCodes, Converted).

	decode_entities(Text, Decoded) :-
		decode_entities(Text, Decoded, []).

	decode_entities(Text, Decoded, UserOptions) :-
		prepare_text(Text, Codes),
		check_entity_options(UserOptions),
		^^option(unknown(Unknown), UserOptions, unknown(preserve)),
		^^decode_entities_codes(Codes, _Profile_, Unknown, DecodedCodes),
		codes_text(DecodedCodes, Decoded).

	normalize_whitespace(Text, Normalized) :-
		normalize_whitespace(Text, Normalized, []).

	normalize_whitespace(Text, Normalized, UserOptions) :-
		prepare_text(Text, Codes),
		check_whitespace_options(UserOptions),
		whitespace_options(UserOptions, Trim, Collapse, LineEndings, Controls),
		^^normalize_whitespace_codes(Codes, Trim, Collapse, LineEndings, Controls, NormalizedCodes),
		codes_text(NormalizedCodes, Normalized).

	clean(Text, Cleaned) :-
		clean(Text, Cleaned, []).

	clean(Text, Cleaned, UserOptions) :-
		prepare_text(Text, Codes),
		check_clean_options(UserOptions),
		^^merge_options(UserOptions, Options),
		clean_entities(Options, Codes, EntityCodes),
		clean_case(Options, EntityCodes, CaseCodes),
		clean_diacritics(Options, CaseCodes, DiacriticCodes),
		^^option(unicode(Form), Options),
		^^normalize_unicode_codes(Form, DiacriticCodes, UnicodeCodes),
		clean_whitespace(Options, UnicodeCodes, CleanedCodes),
		codes_text(CleanedCodes, Cleaned).

	default_option(unicode(nfc)).
	default_option(entities(true)).
	default_option(unknown_entities(preserve)).
	default_option(diacritics(none)).
	default_option(case(preserve)).
	default_option(whitespace(true)).
	default_option(trim(true)).
	default_option(collapse(all)).
	default_option(line_endings(lf)).
	default_option(controls(preserve)).
	default_option(unknown(preserve)).

	valid_option(unicode(Form)) :-
		member(Form, [nfc, nfd, nfkc, nfkd]).
	valid_option(entities(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(unknown_entities(Policy)) :-
		member(Policy, [preserve, error]).
	valid_option(diacritics(Mode)) :-
		member(Mode, [none, remove, fold]).
	valid_option(case(Mode)) :-
		member(Mode, [preserve, fold, lower, upper, title]).
	valid_option(whitespace(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(trim(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(collapse(Mode)) :-
		member(Mode, [none, horizontal, all]).
	valid_option(line_endings(Mode)) :-
		member(Mode, [lf, crlf, cr, preserve]).
	valid_option(controls(Mode)) :-
		member(Mode, [preserve, remove]).
	valid_option(unknown(Policy)) :-
		member(Policy, [preserve, error]).

	prepare_text(Text, Codes) :-
		check_representation,
		check_profile,
		check_text(Text),
		text_codes(Text, Codes),
		check_scalar_values(Codes).

	check_representation :-
		(	var(_Representation_) ->
			instantiation_error
		;	member(_Representation_, [atom, chars, codes]) ->
			true
		;	domain_error(text_representation, _Representation_)
		).

	check_profile :-
		(	var(_Profile_) ->
			instantiation_error
		;	conforms_to_protocol(_Profile_, text_normalization_profile_protocol) ->
			true
		;	domain_error(text_normalization_profile, _Profile_)
		).

	check_text(Text) :-
		context(Context),
		check_text(_Representation_, Text, Context).

	check_text(atom, Text, Context) :-
		check(atom, Text, Context).
	check_text(chars, Text, Context) :-
		check(chars, Text, Context).
	check_text(codes, Text, Context) :-
		check(codes, Text, Context).

	text_codes(atom, Atom, Codes) :-
		atom_codes(Atom, Codes).
	text_codes(chars, Chars, Codes) :-
		atom_chars(Atom, Chars),
		atom_codes(Atom, Codes).
	text_codes(codes, Codes, Codes).

	text_codes(Text, Codes) :-
		text_codes(_Representation_, Text, Codes).

	codes_text(Codes, Text) :-
		codes_text(_Representation_, Codes, Text).

	codes_text(atom, Codes, Atom) :-
		atom_codes(Atom, Codes).
	codes_text(chars, Codes, Chars) :-
		atom_codes(Atom, Codes),
		atom_chars(Atom, Chars).
	codes_text(codes, Codes, Codes).

	check_scalar_values([]).
	check_scalar_values([Code| Codes]) :-
		( Code =< 0x10FFFF, (Code < 0xD800; Code > 0xDFFF) ->
			check_scalar_values(Codes)
		; domain_error(unicode_scalar_value, Code)
		).

	check_form(Form) :-
		( var(Form) ->
			instantiation_error
		; \+ atom(Form) ->
			type_error(atom, Form)
		; member(Form, [nfc, nfd, nfkc, nfkd]) ->
			true
		; domain_error(unicode_normalization_form, Form)
		).

	check_entity_options(Options) :-
		^^check_options(Options),
		check_distinct_options(Options),
		check_option_names(Options, [unknown]).

	check_whitespace_options(Options) :-
		^^check_options(Options),
		check_distinct_options(Options),
		check_option_names(Options, [trim, collapse, line_endings, controls]).

	check_clean_options(Options) :-
		^^check_options(Options),
		check_distinct_options(Options),
		check_option_names(Options, [unicode, entities, unknown_entities, diacritics, case, whitespace, trim, collapse, line_endings, controls]).

	check_option_names([], _).
	check_option_names([Option| Options], Names) :-
		functor(Option, Name, 1),
		( member(Name, Names) ->
			check_option_names(Options, Names)
		; domain_error(option, Option)
		).

	check_distinct_options([]).
	check_distinct_options([Option| Options]) :-
		( same_option_name(Option, Options, Duplicate) ->
			domain_error(option, Duplicate)
		; check_distinct_options(Options)
		).

	same_option_name(Option, [Candidate| _], Candidate) :-
		functor(Option, Name, Arity),
		functor(Candidate, Name, Arity),
		!.
	same_option_name(Option, [_| Options], Duplicate) :-
		same_option_name(Option, Options, Duplicate).

	whitespace_options(Options, Trim, Collapse, LineEndings, Controls) :-
		^^option(trim(Trim), Options, trim(true)),
		^^option(collapse(Collapse), Options, collapse(all)),
		^^option(line_endings(LineEndings), Options, line_endings(lf)),
		^^option(controls(Controls), Options, controls(preserve)).

	clean_entities(Options, Codes, Decoded) :-
		^^option(entities(Entities), Options),
		( Entities == true ->
			^^option(unknown_entities(Unknown), Options),
			^^decode_entities_codes(Codes, _Profile_, Unknown, Decoded)
		; Decoded = Codes
		).

	clean_case(Options, Codes, Converted) :-
		^^option(case(Mode), Options),
		( Mode == preserve ->
			Converted = Codes
		; ^^convert_case_codes(Mode, Codes, _Profile_, Converted)
		).

	clean_diacritics(Options, Codes, Converted) :-
		^^option(diacritics(Mode), Options),
		( Mode == none ->
			Converted = Codes
		; Mode == remove ->
			^^remove_diacritics_codes(Codes, Converted)
		; ^^fold_diacritics_codes(Codes, _Profile_, Converted)
		).

	clean_whitespace(Options, Codes, Normalized) :-
		^^option(whitespace(Whitespace), Options),
		( Whitespace == true ->
			^^option(trim(Trim), Options),
			^^option(collapse(Collapse), Options),
			^^option(line_endings(LineEndings), Options),
			^^option(controls(Controls), Options),
			^^normalize_whitespace_codes(Codes, Trim, Collapse, LineEndings, Controls, Normalized)
		; Normalized = Codes
		).

:- end_object.
