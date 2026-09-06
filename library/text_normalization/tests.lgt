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

	cover(default_text_normalization).
	cover(text_normalizer(_, _)).
	cover(test_text_normalization_profile).
	cover(test_text_case).
	cover(test_text_unicode).
	cover(test_text_entities).
	cover(test_text_whitespace).

	test(default_profile_xml_entity, deterministic(Codes == [38])) :-
		default_text_normalization::named_entity(amp, Codes).

	test(default_profile_html_entity, deterministic(Codes == [160])) :-
		default_text_normalization::named_entity(nbsp, Codes).

	test(default_profile_mixed_case_html_entity, deterministic(Codes == [198])) :-
		default_text_normalization::named_entity('AElig', Codes).

	test(default_profile_multi_code_point_html_entity, deterministic(Codes == [8770, 824])) :-
		default_text_normalization::named_entity('NotEqualTilde', Codes).

	test(default_profile_unknown_entity, false) :-
		default_text_normalization::named_entity(unknown, _).

	test(default_profile_diacritic_fold, deterministic(Codes == [0x006F])) :-
		default_text_normalization::diacritic_fold(0x00F8, Codes).

	test(default_profile_case_fallback, false) :-
		default_text_normalization::case_conversion(lower, [65], _).

	test(case_fold_full_mapping, deterministic(Folded == [115, 115])) :-
		test_text_case::convert(fold, [223], default_text_normalization, Folded).

	test(case_fold_dotted_i, deterministic(Folded == [105, 775])) :-
		test_text_case::convert(fold, [304], default_text_normalization, Folded).

	test(lower_case_final_sigma, deterministic(Lower == [959, 962])) :-
		test_text_case::convert(lower, [927, 931], default_text_normalization, Lower).

	test(lower_case_nonfinal_sigma, deterministic(Lower == [963, 959])) :-
		test_text_case::convert(lower, [931, 927], default_text_normalization, Lower).

	test(upper_case_expansion, deterministic(Upper == [83, 83])) :-
		test_text_case::convert(upper, [223], default_text_normalization, Upper).

	test(title_case_simple_words, deterministic(Title == [201, 108, 97, 110, 32, 86, 105, 116, 97, 108])) :-
		test_text_case::convert(title, [233, 76, 65, 78, 32, 118, 73, 84, 65, 76], default_text_normalization, Title).

	test(title_case_final_sigma, deterministic(Title == [927, 962])) :-
		test_text_case::convert(title, [927, 931], default_text_normalization, Title).

	test(case_profile_override, deterministic(Lower == [305])) :-
		test_text_case::convert(lower, [73], test_text_normalization_profile, Lower).

	test(facade_codes_normalize, deterministic(Normalized == [197]), [condition(\+ current_logtalk_flag(unicode, unsupported))]) :-
		text_normalizer(codes, default_text_normalization)::normalize_unicode(nfc, [65, 778], Normalized).

	test(facade_empty_text, deterministic(Cleaned == [])) :-
		text_normalizer(codes, default_text_normalization)::clean([], Cleaned).

	test(facade_atom_normalize, deterministic(Normalized == 'Cafe')) :-
		text_normalizer(atom, default_text_normalization)::normalize_unicode(nfc, 'Cafe', Normalized).

	test(facade_chars_case_fold, deterministic(Folded == ['s', 's'])) :-
		char_code(SharpS, 223),
		text_normalizer(chars, default_text_normalization)::case_fold([SharpS], Folded).

	test(facade_remove_diacritics, deterministic(Removed == [99, 97, 102, 101])) :-
		text_normalizer(codes, default_text_normalization)::remove_diacritics([99, 97, 102, 233], Removed).

	test(facade_fold_diacritics, deterministic(Folded == [79, 114, 101])) :-
		text_normalizer(codes, default_text_normalization)::fold_diacritics([216, 114, 233], Folded).

	test(facade_decode_entities_default, deterministic(Decoded == [60, 160, 62])) :-
		text_normalizer(codes, default_text_normalization)::decode_entities([38,108,116,59,38,110,98,115,112,59,38,103,116,59], Decoded).

	test(facade_whitespace_options, deterministic(Normalized == [97, 32, 98, 10, 99])) :-
		text_normalizer(codes, default_text_normalization)::normalize_whitespace([97,9,32,98,13,10,99], Normalized, [trim(false), collapse(horizontal), line_endings(lf)]).

	test(facade_clean_defaults, deterministic(Cleaned == [193]), [condition(\+ current_logtalk_flag(unicode, unsupported))]) :-
		text_normalizer(codes, default_text_normalization)::clean([32,38,110,98,115,112,59,65,769,32,32], Cleaned).

	test(facade_clean_pipeline, deterministic(Cleaned == [67, 65, 70, 69])) :-
		text_normalizer(codes, default_text_normalization)::clean([32,99,97,102,233,32], Cleaned, [case(upper), diacritics(remove)]).

	test(facade_clean_disabled_stages, deterministic(Cleaned == [38,97,109,112,59,32,32])) :-
		text_normalizer(codes, default_text_normalization)::clean([38,97,109,112,59,32,32], Cleaned, [entities(false), whitespace(false)]).

	test(facade_clean_idempotent, deterministic(Twice == Once), [condition(\+ current_logtalk_flag(unicode, unsupported))]) :-
		text_normalizer(codes, default_text_normalization)::clean([32,38,110,98,115,112,59,65,769,32,32], Once),
		text_normalizer(codes, default_text_normalization)::clean(Once, Twice).

	test(facade_custom_profile_case, deterministic(Lower == [305])) :-
		text_normalizer(codes, test_text_normalization_profile)::lower_case([73], Lower).

	test(facade_custom_profile_entity, deterministic(Decoded == [88])) :-
		text_normalizer(codes, test_text_normalization_profile)::decode_entities([38,101,120,97,109,112,108,101,59], Decoded).

	test(facade_pipeline_decodes_before_case, deterministic(Cleaned == [305])) :-
		text_normalizer(codes, test_text_normalization_profile)::clean([38,99,97,112,105,116,97,108,95,105,59], Cleaned, [case(lower), whitespace(false)]).

	test(facade_default_entity_option, deterministic) :-
		text_normalizer(codes, default_text_normalization)::default_option(unknown(preserve)).

	test(facade_unknown_entity_error, error(domain_error(character_reference, [110,111,112,101]))) :-
		text_normalizer(codes, default_text_normalization)::decode_entities([38,110,111,112,101,59], _, [unknown(error)]).

	test(facade_invalid_representation, error(domain_error(text_representation, string))) :-
		text_normalizer(string, default_text_normalization)::clean([], _).

	test(facade_invalid_profile, error(domain_error(text_normalization_profile, list))) :-
		text_normalizer(codes, list)::clean([], _).

	test(facade_variable_representation, error(instantiation_error)) :-
		text_normalizer(_, default_text_normalization)::clean([], _).

	test(facade_variable_profile, error(instantiation_error)) :-
		text_normalizer(codes, _)::clean([], _).

	test(facade_invalid_form, error(domain_error(unicode_normalization_form, nfce))) :-
		text_normalizer(codes, default_text_normalization)::normalize_unicode(nfce, [], _).

	test(facade_variable_form, error(instantiation_error)) :-
		text_normalizer(codes, default_text_normalization)::normalize_unicode(_, [], _).

	test(facade_non_atom_form, error(type_error(atom, 42))) :-
		text_normalizer(codes, default_text_normalization)::normalize_unicode(42, [], _).

	test(facade_invalid_text, error(type_error(codes, atom))) :-
		text_normalizer(codes, default_text_normalization)::clean(atom, _).

	test(facade_nonground_text, error(instantiation_error)) :-
		text_normalizer(codes, default_text_normalization)::clean([65, _], _).

	test(facade_invalid_scalar, error(domain_error(unicode_scalar_value, 55296)), [condition(\+ current_logtalk_flag(unicode, unsupported))]) :-
		text_normalizer(codes, default_text_normalization)::clean([55296], _).

	test(facade_non_bmp_codes, deterministic(Normalized == [128512]), [condition(\+ current_logtalk_flag(unicode, unsupported))]) :-
		text_normalizer(codes, default_text_normalization)::normalize_unicode(nfc, [128512], Normalized).

	test(facade_duplicate_option, error(domain_error(option, case(upper)))) :-
		text_normalizer(codes, default_text_normalization)::clean([], _, [case(lower), case(upper)]).

	test(facade_variable_options, error(instantiation_error)) :-
		text_normalizer(codes, default_text_normalization)::clean([], _, _).

	test(facade_non_list_options, error(type_error(list, options))) :-
		text_normalizer(codes, default_text_normalization)::clean([], _, options).

	test(facade_variable_option, error(instantiation_error)) :-
		text_normalizer(codes, default_text_normalization)::clean([], _, [_]).

	test(facade_non_compound_option, error(type_error(compound, invalid))) :-
		text_normalizer(codes, default_text_normalization)::clean([], _, [invalid]).

	test(facade_invalid_option_value, error(domain_error(option, case(invalid)))) :-
		text_normalizer(codes, default_text_normalization)::clean([], _, [case(invalid)]).

	test(facade_irrelevant_entity_option, error(domain_error(option, trim(true)))) :-
		text_normalizer(codes, default_text_normalization)::decode_entities([], _, [trim(true)]).

	test(facade_irrelevant_whitespace_option, error(domain_error(option, case(fold)))) :-
		text_normalizer(codes, default_text_normalization)::normalize_whitespace([], _, [case(fold)]).

	test(extended_profile_custom_entity, deterministic(Codes == [88])) :-
		test_text_normalization_profile::named_entity(example, Codes).

	test(extended_profile_inherited_entity, deterministic(Codes == [60])) :-
		test_text_normalization_profile::named_entity(lt, Codes).

	test(unicode_nfd_canonical_decomposition, deterministic(Normalized == [65, 778])) :-
		test_text_unicode::normalize(nfd, [197], Normalized).

	test(unicode_nfc_canonical_composition, deterministic(Normalized == [197])) :-
		test_text_unicode::normalize(nfc, [65, 778], Normalized).

	test(unicode_nfkd_compatibility_decomposition, deterministic(Normalized == [102, 102])) :-
		test_text_unicode::normalize(nfkd, [64256], Normalized).

	test(unicode_nfkc_compatibility_composition, deterministic(Normalized == [197])) :-
		test_text_unicode::normalize(nfkc, [8491], Normalized).

	test(unicode_canonical_ordering, deterministic(Normalized == [97, 768, 789])) :-
		test_text_unicode::normalize(nfd, [97, 789, 768], Normalized).

	test(unicode_composition_exclusion, deterministic(Normalized == [776, 769])) :-
		test_text_unicode::normalize(nfc, [836], Normalized).

	test(unicode_hangul_decomposition, deterministic(Normalized == [4352, 4449, 4520])) :-
		test_text_unicode::normalize(nfd, [44033], Normalized).

	test(unicode_hangul_composition, deterministic(Normalized == [44033])) :-
		test_text_unicode::normalize(nfc, [4352, 4449, 4520], Normalized).

	test(unicode_normalization_idempotent, deterministic(Twice == Once)) :-
		test_text_unicode::normalize(nfkc, [8491, 64256, 197], Once),
		test_text_unicode::normalize(nfkc, Once, Twice).

	test(unicode_invalid_scalar, error(domain_error(unicode_scalar_value, 55296))) :-
		test_text_unicode::normalize(nfc, [55296], _).

	test(decode_named_entities, deterministic(Decoded == [60, 88, 62])) :-
		test_text_entities::decode([38,108,116,59,38,101,120,97,109,112,108,101,59,38,103,116,59], preserve, Decoded).

	test(decode_multi_code_point_entity, deterministic(Decoded == [65, 66])) :-
		test_text_entities::decode([38,112,97,105,114,59], preserve, Decoded).

	test(decode_decimal_reference, deterministic(Decoded == [123])) :-
		test_text_entities::decode([38,35,49,50,51,59], preserve, Decoded).

	test(decode_hexadecimal_reference, deterministic(Decoded == [0x1F600])) :-
		test_text_entities::decode([38,35,120,49,70,54,48,48,59], preserve, Decoded).

	test(decode_unknown_reference_preserve, deterministic(Decoded == [38,110,111,112,101,59])) :-
		test_text_entities::decode([38,110,111,112,101,59], preserve, Decoded).

	test(decode_unknown_reference_error, error(domain_error(character_reference, [110,111,112,101]))) :-
		test_text_entities::decode([38,110,111,112,101,59], error, _).

	test(decode_incomplete_reference_preserve, deterministic(Decoded == [38,97,109,112])) :-
		test_text_entities::decode([38,97,109,112], preserve, Decoded).

	test(decode_malformed_numeric_reference_preserve, deterministic(Decoded == [38,35,59])) :-
		test_text_entities::decode([38,35,59], error, Decoded).

	test(decode_malformed_named_reference_preserve, deterministic(Decoded == [38,97,32,98,59])) :-
		test_text_entities::decode([38,97,32,98,59], error, Decoded).

	test(decode_invalid_scalar_reference, error(domain_error(unicode_scalar_value, 0xD800))) :-
		test_text_entities::decode([38,35,120,68,56,48,48,59], preserve, _).

	test(whitespace_default_shape, deterministic(Normalized == [72,101,108,108,111,32,119,111,114,108,100])) :-
		test_text_whitespace::normalize([32,32,72,101,108,108,111,9,32,119,111,114,108,100,13,10], true, all, lf, preserve, Normalized).

	test(whitespace_horizontal_preserves_lines, deterministic(Normalized == [97,32,98,10,99])) :-
		test_text_whitespace::normalize([97,9,32,98,13,10,99], false, horizontal, lf, preserve, Normalized).

	test(whitespace_crlf_output, deterministic(Normalized == [97,13,10,98,13,10,99])) :-
		test_text_whitespace::normalize([97,13,98,8232,99], false, none, crlf, preserve, Normalized).

	test(whitespace_unicode_collapse, deterministic(Normalized == [97,32,98])) :-
		test_text_whitespace::normalize([97,160,8195,98], false, all, preserve, preserve, Normalized).

	test(whitespace_remove_controls, deterministic(Normalized == [97,9,98,10,99])) :-
		test_text_whitespace::normalize([97,1,9,98,127,10,99], false, none, preserve, remove, Normalized).

:- end_object.
