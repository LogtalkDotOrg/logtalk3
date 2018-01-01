%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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
		date is 2013/11/18,
		comment is 'Unit tests for the meta_predicate/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(foo/1).
	:- meta_predicate(foo(0)).

	test(meta_predicate_1_1) :-
		predicate_property(foo(_), meta_predicate(Template)),
		Template == foo(0).

	:- public(bar/2).
	:- meta_predicate(bar(^, *)).

	test(meta_predicate_1_2) :-
		predicate_property(bar(_,_), meta_predicate(Template)),
		Template == bar(^, *).

	:- public(baz/3).
	:- meta_predicate(baz(2, *, *)).

	test(meta_predicate_1_3) :-
		predicate_property(baz(_,_,_), meta_predicate(Template)),
		Template == baz(2, *, *).

	:- public(qux/2).
	:- meta_predicate(qux(::, *)).

	test(meta_predicate_1_4) :-
		predicate_property(qux(_,_), meta_predicate(Template)),
		Template == qux(::, *).

:- end_object.
