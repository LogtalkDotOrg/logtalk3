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


:- protocol(language_detector_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Language detection protocol.',
		see_also is [language_detection_strategy_protocol]
	]).

	:- public(detect/2).
	:- mode(detect(+text, -atom), zero_or_one).
	:- info(detect/2, [
		comment is 'Detects the most likely language and returns its ISO 639-1 code.',
		argnames is ['Text', 'Language']
	]).

	:- public(detect/3).
	:- mode(detect(+text, -atom, -float), zero_or_one).
	:- info(detect/3, [
		comment is 'Detects the most likely language and returns its ISO 639-1 code and relative detection score.',
		argnames is ['Text', 'Language', 'Score']
	]).

	:- public(detect/4).
	:- mode(detect(+text, -atom, -float, +list(compound)), zero_or_one).
	:- info(detect/4, [
		comment is 'Detects the most likely language using the given options and returns its ISO 639-1 code and relative detection score.',
		argnames is ['Text', 'Language', 'Score', 'Options']
	]).

	:- public(detect_all/2).
	:- mode(detect_all(+text, -list(pair(atom,float))), one_or_error).
	:- info(detect_all/2, [
		comment is 'Returns candidate languages ranked by descending relative detection score or an empty list when there is insufficient evidence.',
		argnames is ['Text', 'ScoredLanguages']
	]).

	:- public(detect_all/3).
	:- mode(detect_all(+text, -list(pair(atom,float)), +list(compound)), one_or_error).
	:- info(detect_all/3, [
		comment is 'Returns candidate languages ranked by descending relative detection score using the given options or an empty list when there is insufficient evidence.',
		argnames is ['Text', 'ScoredLanguages', 'Options']
	]).

:- end_protocol.
