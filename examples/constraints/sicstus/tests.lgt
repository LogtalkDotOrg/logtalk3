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
		date is 2012/11/26,
		comment is 'Unit tests for the "constraints/sicstus" example.'
	]).

	test(sicstus_clpfd_1) :-
		findall(X, cars_ix::cars_ix([ff], X), Xs),
		Xs == [
			[1,2,6,3,5,4,4,5,3,6],
			[1,3,6,2,5,4,3,5,4,6],
			[1,3,6,2,6,4,5,3,4,5],
			[5,4,3,5,4,6,2,6,3,1],
			[6,3,5,4,4,5,3,6,2,1],
			[6,4,5,3,4,5,2,6,3,1]
		].

	test(sicstus_clpfd_2) :-
		findall(X, cars_ix::cars_ix2([ff], X), Xs),
		Xs == [
			[1,2,6,3,5,4,4,5,3,6],
			[1,3,6,2,5,4,3,5,4,6],
			[1,3,6,2,6,4,5,3,4,5],
			[5,4,3,5,4,6,2,6,3,1],
			[6,3,5,4,4,5,3,6,2,1],
			[6,4,5,3,4,5,2,6,3,1]
		].

	test(sicstus_clpfd_3) :-
		findall(X, cars_ix::cars_ix3([ff], X), Xs),
		Xs == [
			[1,2,6,3,5,4,4,5,3,6],
			[1,3,6,2,5,4,3,5,4,6],
			[1,3,6,2,6,4,5,3,4,5],
			[5,4,3,5,4,6,2,6,3,1],
			[6,3,5,4,4,5,3,6,2,1],
			[6,4,5,3,4,5,2,6,3,1]
		].

:- end_object.
