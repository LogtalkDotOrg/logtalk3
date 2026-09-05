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


:- protocol(tokenizer_language_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'Language provider protocol for tokenization and sentence splitting using canonical character-code lists.',
		see_also is [tokenizer_protocol, sentence_splitter_protocol, tokenizer(_, _)]
	]).

	:- public(tokenize_codes/3).
	:- mode(tokenize_codes(+list(character_code), -list(list(character_code)), +list(compound)), one).
	:- info(tokenize_codes/3, [
		comment is 'Tokenizes a character-code list using validated, merged facade options.',
		argnames is ['Codes', 'Tokens', 'Options']
	]).

	:- public(split_sentence_codes/3).
	:- mode(split_sentence_codes(+list(character_code), -list(list(character_code)), +list(compound)), one).
	:- info(split_sentence_codes/3, [
		comment is 'Splits a character-code list into sentence character-code lists using validated, merged facade options.',
		argnames is ['Codes', 'Sentences', 'Options']
	]).

:- end_protocol.