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


:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(grammars(loader)),
	logtalk_load(strings(loader)),
	logtalk_load(options(loader)),
	logtalk_load(url(loader)),
	logtalk_load([
		tokenizer_protocol,
		sentence_splitter_protocol,
		tokenizer_language_protocol,
		tokenizer,
		tokenizer_rules,
		'languages/english_tokenizer',
		'test_files/test_objects'
	], [
		debug(on),
		source_data(on)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load([
		'test_files/tests_tokenizer_atom',
		'test_files/tests_tokenizer_chars',
		'test_files/tests_tokenizer_codes'
	], [
		hook(lgtunit)
	]),
	lgtunit::run_test_sets([
		tests_tokenizer_atom,
		tests_tokenizer_chars,
		tests_tokenizer_codes
	])
)).
