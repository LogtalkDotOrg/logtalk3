%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
		comment is 'Unit tests for the de facto standard predicate_property/2 built-in predicate.'
	]).

	% tests use non-module predicate properties found on the ISO/IEC 13211-2:2000(en) standard, section 6.8
	% the predicate_property/2 built-in predicate is specified in the same standard in section 7.2.2 but
	% the example in section 7.2.2.4 are module related

	% some of tests below are taken from the ISO/IEC DTR 13211--1:2006 - New built-in flags, predicates,
	% and functions standardization proposal (October 5, 2009 revision)

	succeeds(commons_predicate_property_2_01) :-
		{predicate_property(once(_), built_in)}.

	succeeds(commons_predicate_property_2_02) :-
		{predicate_property(atom_codes(_,_), built_in)}.

	succeeds(commons_predicate_property_2_03) :-
		{predicate_property(findall(_,_,_), meta_predicate(findall(_,0,_)))}.

	succeeds(commons_predicate_property_2_04) :-
		{predicate_property(call(_,_,_), meta_predicate(call(2,_,_)))}.

	succeeds(commons_predicate_property_2_05) :-
		{predicate_property(cat(_), (dynamic))}.

	succeeds(commons_predicate_property_2_06) :-
		{predicate_property(elk(_), static)}.

	succeeds(commons_predicate_property_2_07) :-
		{predicate_property(scattered(_), (multifile))}.

:- end_object.
