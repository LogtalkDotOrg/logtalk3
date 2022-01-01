%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org> and
%  Jacinto Dávila <jdavila@optimusprime.ai>
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


:- protocol(json_protocol).

	:- info([
		version is 0:10:0,
		author is 'Paulo Moura and Jacinto Dávila',
		date is 2021-03-22,
		comment is 'JSON parser and generator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --term), one_or_error).
	:- info(parse/2, [
		comment is 'Parses the JSON contents read from the given source (``codes(List)``, ``stream(Stream)``, ``file(Path)``, ``chars(List)``, or ``atom(Atom)``) into a term. Fails if the JSON contents cannot be parsed.',
		argnames is ['Source', 'Term']
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++term), one_or_error).
	:- info(generate/2, [
		comment is 'Generates the content using the representation specified in the first argument (``codes(List)``, ``stream(Stream)``, ``file(Path)``, ``chars(List)``, or ``atom(Atom)``) for the term in the second argument. Fails if this term cannot be processed.',
		argnames is ['Sink', 'Term']
	]).

:- end_protocol.
