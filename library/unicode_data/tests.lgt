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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Unicode resources.'
	]).

	test(unicode_version_17, true(Version == 17-0-0)) :-
		{unicode_version(Major, Minor, Patch)},
		Version = Major-Minor-Patch.

	test(category_specific_and_aggregate, true(Categories == ['Lu', 'Lc', 'L'])) :-
		{findall(Category, unicode_category(65, Category), Categories)}.

	test(category_unassigned_default, true(Category == 'Cn')) :-
		{unicode_category(1114111, Category)}.

	test(combining_class_default, deterministic(Class == 0)) :-
		{unicode_combining_class(65, Class)}.

	test(binary_property_views, true) :-
		{unicode_alphabetic(65), unicode_core_property(65, 'Alphabetic'), unicode_white_space(32)}.

	test(range_property_views, deterministic) :-
		{unicode_block(65, 'Basic Latin'), unicode_age(65, '1.1'), unicode_bidi_class(65, 'L'),
		 unicode_decomposition_type(192, 'Canonical'), unicode_east_asian_width(65, 'Na'),
		 unicode_joining_group(65, 'No_Joining_Group'), unicode_joining_type(65, 'U'),
		 unicode_line_break(65, 'AL'), unicode_numeric_type(48, 'Decimal'),
		 unicode_hangul_syllable_type(65, 'NA'),
		 unicode_indic_matra_category(65, 'Not_Applicable'),
		 unicode_indic_syllabic_category(65, 'Other'), unicode_script(65, 'Latin')}.

	test(case_folding, deterministic(Mapping == [115, 115])) :-
		{unicode_case_folding(223, 'F', Mapping)}.

	test(special_casing, deterministic(Mapping == [83, 83])) :-
		{unicode_special_casing(223, _, _, Mapping, [])}.

	test(sparse_relations, true) :-
		{unicode_composition_exclusion(2392), unicode_bidi_mirroring(40, 41),
		 unicode_cjk_radical('1', 12032, 19968), unicode_jamo(4352, "G"),
		 unicode_name_alias(0, 'NULL', control), unicode_numerical_value(48, 0.0, 0)}.

	test(arabic_shaping, deterministic) :-
		{unicode_arabic_shaping(1568, _, _, _)}.

	test(script_extension_explicit, deterministic) :-
		{unicode_script_extension(183, ['Avst','Cari','Copt','Dupl','Elba','Geor','Glag','Gong','Goth','Grek','Hani','Latn','Lydi','Mahj','Perm','Shaw'])}.

	test(script_extension_fallback, deterministic(Extension == ['Latin'])) :-
		{unicode_script_extension(65, Extension)}.

	test(script_record_description, deterministic(Description == 'LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z')) :-
		{unicode_script(65, 90, 'Latin', 'Lu', 26, Description)}.

	test(unihan_variants_preserve_solutions, true(Count > 1)) :-
		{findall(Variant, unicode_unihan_variant(20010, _, Variant), Variants)},
		length(Variants, Count).

	test(explicit_name, deterministic(Name == 'LATIN CAPITAL LETTER A')) :-
		{unicode_name(65, Name)}.

	test(normalization_properties, deterministic) :-
		{unicode_fc_nfkc(890, [32, 953]), unicode_full_composition_exclusion(832),
		 unicode_nfc_qc_maybe(768), unicode_nfkc_cf(65, [97]),
		 unicode_changes_when_nfkc_casefolded(65)}.

	test(decomposition_subviews, deterministic) :-
		{unicode_canonical(192), unicode_font(8450), unicode_fraction(188)}.

:- end_object.
