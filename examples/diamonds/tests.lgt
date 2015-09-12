%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "diamonds" example.'
	]).

	test(d1) :-
		d1::predicate_property(m, defined_in(Object)),
		Object == b1.

	test(d2_1) :-
		d2::predicate_property(m, defined_in(Object)),
		Object == d2.

	test(d2_2) :-
		d2::predicate_property(c2_m, defined_in(Object)),
		Object == c2.

	test(d3_1) :-
		d3::predicate_property(b3_m, defined_in(Object)),
		Object == b3.

	test(d3_2) :-
		d3::predicate_property(c3_m, defined_in(Object)),
		Object == c3.

	test(d3_3) :-
		d3::predicate_property(m, defined_in(Object)),
		Object == b3.

:- end_object.
