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


:- if(\+ current_logtalk_flag(unicode, unsupported)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(types(loader)),
		logtalk_load(options(loader)),
		logtalk_load(stop_words(loader)),
		logtalk_load([
			stop_words('languages/stopwords_de'),
			stop_words('languages/stopwords_es'),
			stop_words('languages/stopwords_fr'),
			stop_words('languages/stopwords_it'),
			stop_words('languages/stopwords_pt')
		], [
			optimize(on)
		]),
		logtalk_load(tokenization(loader)),
		logtalk_load(text_normalization(loader)),
		logtalk_load(n_grams(loader)),
		logtalk_load([
			language_detector_protocol,
			language_detection_strategy_protocol,
			language_profile_protocol,
			language_profile_common,
			language_detection_scoring,
			language_profiles,
			'profiles/en_language_profile',
			'profiles/pt_language_profile',
			'profiles/es_language_profile',
			'profiles/fr_language_profile',
			'profiles/de_language_profile',
			'profiles/it_language_profile',
			'strategies/language_detection_tokenizer',
			'strategies/stopword_language_detector',
			'strategies/ngram_language_detector',
			'strategies/hybrid_language_detector',
			language_detector,
			'test_files/test_objects'
		], [
			debug(on),
			source_data(on)
		]),
		logtalk_load(lgtunit(loader)),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- else.

	:- initialization((write('(not applicable)'), nl)).

:- endif.
