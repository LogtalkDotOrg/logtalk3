%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2012-07-03,
		comment is 'Unit tests for the "prototypes" example.'
	]).

	cover(alf).
	cover(skip).
	cover(rhonda).

	test(prototypes_1) :-
		findall(P, (alf::current_predicate(F/A), functor(P,F,A), alf::P), Solutions),
		Solutions == [
			chases('Lucky'), 
			favorite_food(cats), 
			motto('Are you going to finish that sandwich?'), 
			name('Gordon Shumway'), 
			planet('Melmac'), 
			stomachs(8)].

	test(prototypes_2) :-
		findall(Melmacian, rhonda::boyfriend(Melmacian), Solutions),
		Solutions == [alf].

:- end_object.
