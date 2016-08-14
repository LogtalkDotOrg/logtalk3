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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/08/13,
		comment is 'Unit tests for the "my_types" example.'
	]).

	succeeds(types_1) :-
		type::check(temperature(celsius), 38.7).

	succeeds(types_2) :-
		type::check(temperature(fahrenheit), 101.2).

	succeeds(types_3) :-
		type::check(temperature(kelvin), 307.4).

	fails(types_4) :-
		type::valid(temperature(kelvin), -12.1).

	throws(types_5, type_error(float,38)) :-
		type::check(temperature(celsius), 38).

	throws(types_6, error(domain_error(property(float,[Parameter]>>(Parameter>=0.0)),-12.1), my_error_context)) :-
		type::check(temperature(kelvin), -12.1, my_error_context).

:- end_object.
