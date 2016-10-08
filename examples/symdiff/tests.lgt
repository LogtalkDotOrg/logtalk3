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
		comment is 'Unit tests for the "symdiff" example.'
	]).

	test(symdiff_1) :-
		(x**1 + x*0 - x*1)::simplify(S),
		S == 0.

	test(symdiff_2) :-
		(2*x**3 + x**2 - 4*x)::diff(D), D::simplify(S),
		D == 2 * (3*x**2*1) + 2*x**1*1-4*1, S == 2 * (3*x**2) + 2*x-4.

	test(symdiff_3) :-
		(log(x**2 + 2*x - 7) + 4*x)::diff(D), D::simplify(S),
		D == (2*x**1*1+2*1) * (x**2+2*x-7) ** -1 + 4*1, S == (2*x+2) * (x**2+2*x-7) ** -1 + 4.

:- end_object.
