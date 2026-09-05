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


:- protocol(stop_words_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'Stop-word enumeration and filtering protocol.',
		see_also is [stop_words_language_protocol, stop_words(_, _)]
	]).

	:- public(stop_word/1).
	:- mode(stop_word(?text), zero_or_more).
	:- info(stop_word/1, [
		comment is 'Enumerates canonical stop words using the configured text representation.',
		argnames is ['Word']
	]).

	:- public(is_stop_word/1).
	:- mode(is_stop_word(+text), zero_or_one).
	:- info(is_stop_word/1, [
		comment is 'True if the word, after ASCII lowercasing, is a stop word.',
		argnames is ['Word']
	]).

	:- public(exclude/2).
	:- mode(exclude(+list(text), -list(text)), one).
	:- info(exclude/2, [
		comment is 'Removes stop words, preserving the representation, value, and order of the remaining words.',
		argnames is ['Words', 'Filtered']
	]).

:- end_protocol.