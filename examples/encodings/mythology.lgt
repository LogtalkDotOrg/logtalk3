:- encoding('UTF-32').		% this directive, when present, must be the first
							% term, in the first line, of a source file

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(mythology).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2008-01-16,
		comment is 'Simple test of the encoding/1 directive.'
	]).

	:- public(divinity/2).
	:- mode(divinity(?atom, ?atom), zero_or_more).
	:- info(divinity/2, [
		comment is 'Table of english and greek names for mythology divinities.',
		argnames is ['English', 'Greek']
	]).

	divinity(hera, 'Ηρα').
	divinity(kalypso, 'Καλυψω').
	divinity(morpheus, 'Μορφευς').
	divinity(poseidon, 'Ποσειδων').
	divinity(zeus, 'Ζευς').

:- end_object.
