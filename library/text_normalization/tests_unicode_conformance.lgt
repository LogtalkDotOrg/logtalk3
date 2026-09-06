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


:- object(unicode_conformance_case,
	imports(text_case_folding)).

	:- public(case_fold/1).

	:- uses(user, [
		unicode_data_case_fold_mapping/2,
		unicode_data_case_fold_mapping_count/1
	]).

	case_fold(Count) :-
		unicode_data_case_fold_mapping_count(Count),
		forall(
			unicode_data_case_fold_mapping(Code, Expected),
			^^convert_case_codes(fold, [Code], default_text_normalization, Expected)
		).

:- end_object.


:- object(unicode_conformance_normalization,
	imports([text_unicode, unicode_normalization_test_data])).

	:- public(normalization/1).

	normalization(Count) :-
		^^normalization_test_count(Count),
		forall(
			^^normalization_test(Source, NFC, NFD, NFKC, NFKD),
			normalization_invariants(Source, NFC, NFD, NFKC, NFKD)
		).

	normalization_invariants(Source, NFC, NFD, NFKC, NFKD) :-
		^^normalize_unicode_codes(nfc, Source, NFC),
		^^normalize_unicode_codes(nfc, NFC, NFC),
		^^normalize_unicode_codes(nfc, NFD, NFC),
		^^normalize_unicode_codes(nfc, NFKC, NFKC),
		^^normalize_unicode_codes(nfc, NFKD, NFKC),
		^^normalize_unicode_codes(nfd, Source, NFD),
		^^normalize_unicode_codes(nfd, NFC, NFD),
		^^normalize_unicode_codes(nfd, NFD, NFD),
		^^normalize_unicode_codes(nfd, NFKC, NFKD),
		^^normalize_unicode_codes(nfd, NFKD, NFKD),
		^^normalize_unicode_codes(nfkc, Source, NFKC),
		^^normalize_unicode_codes(nfkc, NFC, NFKC),
		^^normalize_unicode_codes(nfkc, NFD, NFKC),
		^^normalize_unicode_codes(nfkc, NFKC, NFKC),
		^^normalize_unicode_codes(nfkc, NFKD, NFKC),
		^^normalize_unicode_codes(nfkd, Source, NFKD),
		^^normalize_unicode_codes(nfkd, NFC, NFKD),
		^^normalize_unicode_codes(nfkd, NFD, NFKD),
		^^normalize_unicode_codes(nfkd, NFKC, NFKD),
		^^normalize_unicode_codes(nfkd, NFKD, NFKD).

:- end_object.


:- object(tests_unicode_conformance,
	extends(lgtunit)).

	test(case_fold_conformance, deterministic(Count == 1585)) :-
		unicode_conformance_case::case_fold(Count).

	test(unicode_normalization_conformance, deterministic(Count == 20034)) :-
		unicode_conformance_normalization::normalization(Count).

:- end_object.
