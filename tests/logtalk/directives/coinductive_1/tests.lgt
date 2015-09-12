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
		author is 'Paulo Moura',
		date is 2014/11/16,
		comment is 'Unit tests for the coinductive/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(a/1).
	:- coinductive(a/1).

	test(coinductive_1_1) :-
		predicate_property(a(_), coinductive(Template)),
		Template == a(+).

	:- public(b/2).
	:- coinductive(b(+, -)).

	test(coinductive_1_2) :-
		predicate_property(b(_, _), coinductive(Template)),
		Template == b(+, -).

	% calls to predicates declared coinductive but not defined
	% must fail instead of throwing an existence error

	:- coinductive(c/3).

	test(coinductive_1_3) :-
		\+ c(_, _, _).

:- end_object.
