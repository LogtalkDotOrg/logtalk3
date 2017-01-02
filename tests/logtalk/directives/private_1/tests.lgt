%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		date is 2014/05/07,
		comment is 'Unit tests for the private/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive
	:- private(a/0).
	:- private((b/1, c/2)).
	:- private([d/3, e/4]).

	test(private_1_1) :-
		predicate_property(a, private),
		predicate_property(a, static).

	test(private_1_2) :-
		predicate_property(b(_), private),
		predicate_property(b(_), static),
		predicate_property(c(_,_), private),
		predicate_property(c(_,_), static).

	test(private_1_3) :-
		predicate_property(d(_,_,_), private),
		predicate_property(d(_,_,_), static),
		predicate_property(e(_,_,_,_), private),
		predicate_property(e(_,_,_,_), static).

:- end_object.
