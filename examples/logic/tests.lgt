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
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2016/02/15,
		comment is 'Unit tests for the "logic" example.'
	]).

	:- uses(translator, [
		op(10, fy, '~' ), op(20, yfx, '&' ), op(30, yfx, 'v' ),
		op(40, xfx, '=>'), op(40, xfx, '<=>')
	]).

	test(logic_1) :-
		translator::translate((p v ~q) => (r & k), Cs),
		Cs = [cl([r],[p]),cl([k],[p]),cl([q,r],[]),cl([q,k],[])].

	test(logic_2) :-
		translator::step_by_step(all(X, exists(Y, p(X) v ~q(X) => r(X, Y))), Cs),
		Y = f1(X), Cs = [cl([r(X, f1(X))], [p(X)]), cl([q(X), r(X, f1(X))], [])].

	test(logic_3) :-
		translator::step_by_step(all(X, men(X) => mortal(X)), Cs),
		Cs = [cl([mortal(X)], [men(X)])].

:- end_object.
