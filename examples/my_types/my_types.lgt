%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- category(my_types).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-01-03,
		comment is 'A simple example of extending the "type" library object with a new type definition.'
	]).

	% register a new parametric temperature type
	:- multifile(type::type/1).
	type::type(temperature(_Unit)).

	% add the actual checking code for the new type
	:- multifile(type::check/2).
	type::check(temperature(Unit), Term) :-
		check_temperature(Unit, Term).

	% given that temperature has only a lower bound, we make use of the library
	% property/2 type to define the necessary test expression for each unit
	check_temperature(celsius, Term) :-
		type::check(property(float, [Temperature]>>(Temperature >= -273.15)), Term).
	check_temperature(fahrenheit, Term) :-
		type::check(property(float, [Temperature]>>(Temperature >= -459.67)), Term).
	check_temperature(kelvin, Term) :-
		type::check(property(float, [Temperature]>>(Temperature >= 0.0)), Term).

:- end_category.
