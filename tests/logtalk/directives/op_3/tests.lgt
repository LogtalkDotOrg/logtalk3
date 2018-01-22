%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		date is 2012/12/26,
		comment is 'Unit tests for the op/3 built-in directive.'
	]).

	:- private(op(501, xfx, foo)).
	:- private(op(601, xfy, [bar, baz])).

	test(op_3_1) :-
		current_op(Priority, Specifier, foo),
		Priority == 501,
		Specifier == xfx.

	test(op_3_2) :-
		current_op(Priority, Specifier, bar),
		Priority == 601,
		Specifier == xfy.

	test(op_3_3) :-
		current_op(Priority, Specifier, baz),
		Priority == 601,
		Specifier == xfy.

:- end_object.
