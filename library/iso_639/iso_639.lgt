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


:- object(iso_639,
	implements(iso_639_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Facade object for ISO 639 language and language-group queries.'
	]).

	language(Alpha2, Alpha3, Name) :-
		iso_639_1::language(Alpha2, Alpha3, Name).

	language_code(Bibliographic, Terminologic, Alpha2, Name, Class) :-
		iso_639_2::language_code(Bibliographic, Terminologic, Alpha2, Name, Class).

	language(Alpha3, Alpha2, Scope, Type, Name) :-
		iso_639_3::language(Alpha3, Alpha2, Scope, Type, Name).

	language_group(Code, Name) :-
		iso_639_5::language_group(Code, Name).

:- end_object.
