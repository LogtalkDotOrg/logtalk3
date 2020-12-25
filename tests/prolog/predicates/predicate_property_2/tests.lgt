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


% database for tests

:- dynamic(cat/1).
cat(felix).

elk(X) :- moose(X).

:- multifile(scattered/1).
:- dynamic(scattered/1).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2020-12-09,
		comment is 'Unit tests for the de facto standard predicate_property/2 built-in predicate.'
	]).

	% tests use non-module predicate properties found on the ISO/IEC 13211-2:2000(en) standard, section 6.8
	% the predicate_property/2 built-in predicate is specified in the same standard in section 7.2.2 but
	% the example in section 7.2.2.4 are module related

	% some of tests below are taken from the ISO/IEC DTR 13211--1:2006 - New built-in flags, predicates,
	% and functions standardization proposal (October 5, 2009 revision)

	test(commons_predicate_property_2_01, true) :-
		forall(
			{predicate_property(once(_), Property)},
			^^assertion(ground(Property))
		).

	test(commons_predicate_property_2_02, true) :-
		{predicate_property(once(_), built_in)}.

	test(commons_predicate_property_2_03, true) :-
		{predicate_property(atom_codes(_,_), built_in)}.

	test(commons_predicate_property_2_04, subsumes(findall(_,0,_),Template)) :-
		{predicate_property(findall(_,_,_), meta_predicate(Template))}.

	test(commons_predicate_property_2_05, subsumes(call(2,_,_),Template)) :-
		{predicate_property(call(_,_,_), meta_predicate(Template))}.

	test(commons_predicate_property_2_06, true) :-
		{predicate_property(cat(_), (dynamic))}.

	test(commons_predicate_property_2_07, true) :-
		{predicate_property(elk(_), static)}.

	test(commons_predicate_property_2_08, true) :-
		{predicate_property(scattered(_), (multifile))}.

	test(commons_predicate_property_2_09, true) :-
		{predicate_property(scattered(_), (dynamic))}.

	test(commons_predicate_property_2_10, error(instantiation_error)) :-
		{predicate_property(_, _)}.

	test(commons_predicate_property_2_11, error(type_error(callable,1))) :-
		{predicate_property(1, _)}.

	test(commons_predicate_property_2_12, error(domain_error(predicate_property,foobar))) :-
		{predicate_property(atom_chars(_,_), foobar)}.

:- end_object.
