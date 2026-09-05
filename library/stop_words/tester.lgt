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


:- if(\+ current_logtalk_flag(encoding_directive, unsupported)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(strings(loader)),
		logtalk_load([
			stop_words_language_protocol,
			stop_words_protocol,
			stop_words,
			'languages/stopwords_de',
			'languages/stopwords_en',
			'languages/stopwords_es',
			'languages/stopwords_fr',
			'languages/stopwords_it',
			'languages/stopwords_pt',
			'test_files/test_objects'
		], [debug(on), source_data(on)]),
		logtalk_load(lgtunit(loader)),
		logtalk_load([
			'test_files/tests_stop_words_atom',
			'test_files/tests_stop_words_chars',
			'test_files/tests_stop_words_codes',
			'test_files/tests_languages'
		], [hook(lgtunit)]),
		lgtunit::run_test_sets([
			tests_stop_words_atom,
			tests_stop_words_chars,
			tests_stop_words_codes,
			tests_languages
		])
	)).

:- else.

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(strings(loader)),
		logtalk_load([
			stop_words_language_protocol,
			stop_words_protocol,
			stop_words,
			'languages/stopwords_en',
			'test_files/test_objects'
		], [debug(on), source_data(on)]),
		logtalk_load(lgtunit(loader)),
		logtalk_load([
			'test_files/tests_stop_words_atom',
			'test_files/tests_stop_words_chars',
			'test_files/tests_stop_words_codes'
		], [hook(lgtunit)]),
		lgtunit::run_test_sets([
			tests_stop_words_atom,
			tests_stop_words_chars,
			tests_stop_words_codes
		])
	)).

:- endif.
