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
		date is 2016/10/16,
		comment is 'Unit tests for the info/2 built-in directive.'
	]).

	:- public(a/1).
	:- info(a/1, [
		comment is 'A public predicate.',
		remarks is [
			'key' - 'value'
		],
		redefinition is specialize,
		allocation is any,
		arguments is ['Arg' - 'Arg description'],
		examples is [
			'Sample call' - a(X) - {X = 1}
		],
		exceptions is [
			'Description' - error
		],
		custom is value
	]).

	:- protected(b/2).
	:- info(b/2, [
		comment is 'A protected predicate.',
		remarks is [
			'key1' - 'value1',
			'key2' - 'value2'
		],
		redefinition is call_super_first,
		allocation is descendants,
		argnames is ['Arg1', 'Arg2'],
		examples is [
			'Sample call' - b(X, Y) - {X = 1, Y = 2}
		],
		exceptions is [
			'Description1' - error1,
			'Description2' - error2
		],
		custom is value
	]).

	:- private(c/3).
	:- info(c/3, [
		comment is 'A private predicate.',
		remarks is [
			'key1' - 'value1',
			'key2' - 'value2',
			'key3' - 'value3'
		],
		redefinition is never,
		allocation is container,
		argnames is ['Arg1', 'Arg2', 'Arg3'],
		examples is [
			'Sample call' - c(X, Y, Z) - {X = 1, Y = 2, Z = 3}
		],
		exceptions is [
			'Description1' - error1,
			'Description2' - error2,
			'Description3' - error3
		],
		custom is value
	]).

	:- uses(list, [
		memberchk/2
	]).

	test(info_2_01) :-
		this(This),
		object_property(This, declares(a/1, Properties)),
		memberchk(scope(Scope), Properties), Scope == (public),
		memberchk((public), Properties),
		memberchk(static, Properties).

	test(info_2_02) :-
		this(This),
		object_property(This, declares(a/1, Properties)),
		memberchk(info(Info), Properties),
		memberchk(comment(Comment), Info), Comment == 'A public predicate.',
		memberchk(remarks(Remarks), Info), Remarks == ['key'-'value'],
		memberchk(redefinition(Redefinition), Info), Redefinition == specialize,
		memberchk(allocation(Allocation), Info), Allocation == any,
		memberchk(arguments(Arguments), Info), Arguments == ['Arg' - 'Arg description'],
		memberchk(examples(Examples), Info), ^^variant(Examples, ['Sample call'-a(X)-{X=1}]),
		memberchk(exceptions(Exceptions), Info), Exceptions == ['Description'-error],
		memberchk(custom(Value), Info), Value == value.

	test(info_2_03) :-
		this(This),
		object_property(This, declares(b/2, Properties)),
		memberchk(scope(Scope), Properties), Scope == protected,
		memberchk(protected, Properties),
		memberchk(static, Properties).

	test(info_2_04) :-
		this(This),
		object_property(This, declares(b/2, Properties)),
		memberchk(info(Info), Properties),
		memberchk(comment(Comment), Info), Comment == 'A protected predicate.',
		memberchk(remarks(Remarks), Info), Remarks == ['key1'-'value1', 'key2'-'value2'],
		memberchk(redefinition(Redefinition), Info), Redefinition == call_super_first,
		memberchk(allocation(Allocation), Info), Allocation == descendants,
		memberchk(argnames(Argnames), Info), Argnames == ['Arg1', 'Arg2'],
		memberchk(examples(Examples), Info), ^^variant(Examples, ['Sample call'-b(X,Y)-{X=1,Y=2}]),
		memberchk(exceptions(Exceptions), Info), Exceptions == ['Description1'-error1,'Description2'-error2],
		memberchk(custom(Value), Info), Value == value.

	test(info_2_05) :-
		this(This),
		object_property(This, declares(c/3, Properties)),
		memberchk(scope(Scope), Properties), Scope == (private),
		memberchk((private), Properties),
		memberchk(static, Properties).

	test(info_2_06) :-
		this(This),
		object_property(This, declares(c/3, Properties)),
		memberchk(info(Info), Properties),
		memberchk(comment(Comment), Info), Comment == 'A private predicate.',
		memberchk(remarks(Remarks), Info), Remarks == ['key1'-'value1', 'key2'-'value2', 'key3'-'value3'],
		memberchk(redefinition(Redefinition), Info), Redefinition == never,
		memberchk(allocation(Allocation), Info), Allocation == container,
		memberchk(argnames(Argnames), Info), Argnames == ['Arg1', 'Arg2', 'Arg3'],
		memberchk(examples(Examples), Info), ^^variant(Examples, ['Sample call'-c(X,Y,Z)-{X=1,Y=2,Z=3}]),
		memberchk(exceptions(Exceptions), Info), Exceptions == ['Description1'-error1,'Description2'-error2,'Description3'-error3],
		memberchk(custom(Value), Info), Value == value.

:- end_object.
