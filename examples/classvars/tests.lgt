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
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "classvars" example.'
	]).

	cover(root).
	cover(instance1).
	cover(instance2).
	cover(instance3).

	test(classvars_1) :-
		instance1::cv(Value1),
		instance2::cv(Value2),
		instance3::cv(Value3),
		Value1 == 0, Value2 == 0, Value3 == 0.

	% test 2.  Note: Depends on previous test.
	test(classvars_2) :-
		instance1::set_cv(1).

	% test 3.   Note: Depends on previous test.
	test(classvars_3) :-
		instance2::cv(Value2),
		instance3::cv(Value3),
		Value2 == 1, Value3 == 1.

:- end_object.
