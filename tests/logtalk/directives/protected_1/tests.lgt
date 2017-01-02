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
		comment is 'Unit tests for the protected/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive
	:- protected(a/0).
	:- protected((b/1, c/2)).
	:- protected([d/3, e/4]).

	test(protected_1_1) :-
		predicate_property(a, protected),
		predicate_property(a, static).

	test(protected_1_2) :-
		predicate_property(b(_), protected),
		predicate_property(b(_), static),
		predicate_property(c(_,_), protected),
		predicate_property(c(_,_), static).

	test(protected_1_3) :-
		predicate_property(d(_,_,_), protected),
		predicate_property(d(_,_,_), static),
		predicate_property(e(_,_,_,_), protected),
		predicate_property(e(_,_,_,_), static).

:- end_object.
