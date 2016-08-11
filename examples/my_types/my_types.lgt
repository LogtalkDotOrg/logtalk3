%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/08/10,
		comment is 'A simple example of extending the "type" library object with a new type definition.'
	]).

	% register the new type
	:- multifile(type::type/1).
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::type/1).
	:- endif.
	type::type(temperature(_Unit)).

	% add the actual checking code for the new type
	:- multifile(type::check/2).
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::check/2).
	:- endif.
	type::check(temperature(Unit), Term) :-
		check_temperature(Unit, Term).

	check_temperature(celsius, Term) :-
		type::check(property(float, [Temperature]>>(Temperature >= -273.15)), Term).
	check_temperature(fahrenheit, Term) :-
		type::check(property(float, [Temperature]>>(Temperature >= -459.67)), Term).
	check_temperature(kelvin, Term) :-
		type::check(property(float, [Temperature]>>(Temperature >= 0.0)), Term).

:- end_category.
