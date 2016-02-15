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
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "inheritance" example.'
	]).

	test(inheritance_01) :-
		parent::current_predicate((public)/0),
		\+ parent::current_predicate(protected/0),
		\+ parent::current_predicate(private/0),
		parent << predicate_property(public, public),
		parent << predicate_property(protected, protected),
		parent << predicate_property(private, private).

	test(inheritance_02) :-
		prototype1::current_predicate((public)/0),
		\+ prototype1::current_predicate(protected/0),
		\+ prototype1::current_predicate(private/0),
		prototype1 << predicate_property(public, public),
		prototype1 << predicate_property(protected, protected),
		\+ prototype1 << predicate_property(private, _).

	test(inheritance_03) :-
		\+ prototype2::current_predicate((public)/0),
		\+ prototype2::current_predicate(protected/0),
		\+ prototype2::current_predicate(private/0),
		prototype2 << predicate_property(public, protected),
		prototype2 << predicate_property(protected, protected),
		\+ prototype1 << predicate_property(private, _).

	test(inheritance_04) :-
		\+ prototype3::current_predicate((public)/0),
		\+ prototype3::current_predicate(protected/0),
		\+ prototype3::current_predicate(private/0),
		prototype3 << predicate_property(public, private),
		prototype3 << predicate_property(protected, private),
		\+ prototype3 << predicate_property(private, _).

	test(inheritance_05) :-
		descendant1::current_predicate((public)/0),
		\+ descendant1::current_predicate(protected/0),
		\+ descendant1::current_predicate(private/0),
		descendant1 << predicate_property(public, public),
		descendant1 << predicate_property(protected, protected),
		\+ descendant1 << predicate_property(private, _).

	test(inheritance_06) :-
		\+ descendant2::current_predicate((public)/0),
		\+ descendant2::current_predicate(protected/0),
		\+ descendant2::current_predicate(private/0),
		descendant2 << predicate_property(public, protected),
		descendant2 << predicate_property(protected, protected),
		\+ descendant2 << predicate_property(private, _).

	test(inheritance_07) :-
		\+ descendant3::current_predicate((public)/0),
		\+ descendant3::current_predicate(protected/0),
		\+ descendant3::current_predicate(private/0),
		\+ descendant3 << predicate_property(public, _),
		\+ descendant3 << predicate_property(protected, _),
		\+ descendant3 << predicate_property(private, _).

	test(inheritance_08) :-
		root::current_predicate((public)/0),
		\+ root::current_predicate(protected/0),
		\+ root::current_predicate(private/0),
		root << predicate_property(public, public),
		root << predicate_property(protected, protected),
		root << predicate_property(private, private).

	test(inheritance_09) :-
		instance1::current_predicate((public)/0),
		\+ instance1::current_predicate(protected/0),
		\+ instance1::current_predicate(private/0),
		instance1 << predicate_property(public, public),
		instance1 << predicate_property(protected, protected),
		\+ instance1 << predicate_property(private, _).

	test(inheritance_10) :-
		\+ instance2::current_predicate((public)/0),
		\+ instance2::current_predicate(protected/0),
		\+ instance2::current_predicate(private/0),
		instance2 << predicate_property(public, protected),
		instance2 << predicate_property(protected, protected),
		\+ instance2 << predicate_property(private, _).

	test(inheritance_11) :-
		\+ instance3::current_predicate((public)/0),
		\+ instance3::current_predicate(protected/0),
		\+ instance3::current_predicate(private/0),
		\+ instance3 << predicate_property(public, _),
		\+ instance3 << predicate_property(protected, _),
		\+ instance3 << predicate_property(private, _).

:- end_object.
