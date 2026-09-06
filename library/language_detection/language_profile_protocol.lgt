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


:- protocol(language_profile_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Language detection profile protocol.'
	]).

	:- public(language/1).
	:- mode(language(-atom), one).
	:- info(language/1, [
		comment is 'Returns the profile ISO 639-1 language code.',
		argnames is ['Language']
	]).

	:- public(trigram_counts/1).
	:- mode(trigram_counts(-list(pair(text,positive_integer))), one).
	:- info(trigram_counts/1, [
		comment is 'Returns the lexically sorted character-trigram count vector.',
		argnames is ['Counts']
	]).

	:- public(stop_word/1).
	:- mode(stop_word(?atom), zero_or_more).
	:- info(stop_word/1, [
		comment is 'Enumerates canonical lowercase stop words.',
		argnames is ['Word']
	]).

:- end_protocol.
