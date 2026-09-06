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


:- protocol(text_normalization_profile_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Profile provider protocol for extending text normalization policy without replacing Unicode normalization algorithms.',
		see_also is [text_normalizer_protocol, text_normalizer(_, _)]
	]).

	:- public(case_conversion/3).
	:- mode(case_conversion(+atom, +codes, -codes), zero_or_one).
	:- info(case_conversion/3, [
		comment is 'Converts a list of Unicode code points for a profile-specific case operation. Valid operations are ``fold``, ``lower``, ``upper``, and ``title``. Fails to select the library default Unicode behavior.',
		argnames is ['Operation', 'Codes', 'Converted']
	]).

	:- public(diacritic_fold/2).
	:- mode(diacritic_fold(+code, -codes), zero_or_one).
	:- info(diacritic_fold/2, [
		comment is 'Maps a Unicode code point without a canonical base decomposition to a profile-specific replacement. Fails when no mapping is defined.',
		argnames is ['Code', 'Replacement']
	]).

	:- public(named_entity/2).
	:- mode(named_entity(+atom, -codes), zero_or_one).
	:- info(named_entity/2, [
		comment is 'Maps a case-sensitive named character reference, without the leading ampersand or trailing semicolon, to Unicode code points. Fails for an unknown name.',
		argnames is ['Name', 'Codes']
	]).

:- end_protocol.
