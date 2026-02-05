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


:- protocol(stemmer_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-05,
		comment is 'Stemmer protocol for reducing words to their stems.',
		see_also is [porter_stemmer(_), lovins_stemmer(_)]
	]).

	:- public(stem/2).
	:- mode(stem(+text, -text), one).
	:- info(stem/2, [
		comment is 'Stems a single word, returning its root form.',
		argnames is ['Word', 'Stem']
	]).

	:- public(stems/2).
	:- mode(stems(+list(text), -list(text)), one).
	:- info(stems/2, [
		comment is 'Stems a list of words, returning a list of their root forms.',
		argnames is ['Words', 'Stems']
	]).

:- end_protocol.
