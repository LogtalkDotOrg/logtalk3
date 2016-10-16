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
		date is 2016/10/16,
		comment is 'Unit tests for the info/2 built-in directive.'
	]).

	:- public(a/1).
	:- info(a/1, [
		comment is 'A public predicate.',
		argnames is ['Arg'],
		custom is value
	]).

	:- protected(b/2).
	:- info(b/2, [
		comment is 'A protected predicate.',
		argnames is ['Arg1', 'Arg2'],
		custom is value
	]).

	:- private(c/3).
	:- info(c/3, [
		comment is 'A private predicate.',
		argnames is ['Arg1', 'Arg2', 'Arg3'],
		custom is value
	]).

	:- uses(list, [
		memberchk/2
	]).

	test(info_2_01) :-
		this(This),
		object_property(This, declares(a/1, Properties)),
		ground(Properties),
		memberchk(scope(public), Properties),
		memberchk((public), Properties),
		memberchk(static, Properties).

	test(info_2_02) :-
		this(This),
		object_property(This, declares(a/1, Properties)),
		ground(Properties),
		memberchk(info(Info), Properties),
		memberchk(comment('A public predicate.'), Info),
		memberchk(custom(value), Info).

	test(info_2_03) :-
		this(This),
		object_property(This, declares(b/2, Properties)),
		ground(Properties),
		memberchk(scope(protected), Properties),
		memberchk(protected, Properties),
		memberchk(static, Properties).

	test(info_2_04) :-
		this(This),
		object_property(This, declares(b/2, Properties)),
		ground(Properties),
		memberchk(info(Info), Properties),
		memberchk(comment('A protected predicate.'), Info),
		memberchk(custom(value), Info).

	test(info_2_05) :-
		this(This),
		object_property(This, declares(c/3, Properties)),
		ground(Properties),
		memberchk(scope(private), Properties),
		memberchk((private), Properties),
		memberchk(static, Properties).

	test(info_2_06) :-
		this(This),
		object_property(This, declares(c/3, Properties)),
		ground(Properties),
		memberchk(info(Info), Properties),
		memberchk(comment('A private predicate.'), Info),
		memberchk(custom(value), Info).

:- end_object.
