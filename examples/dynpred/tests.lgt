%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2.3,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/04,
		comment is 'Unit tests for the "dynpred" example.'
	]).

	cover(ctg).
	cover(top).
	cover(middle).
	cover(bottom).

	cover(metaclass).
	cover(class).
	cover(instance).

	cover(root).
	cover(descendant).

	cover(prototype).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	succeeds(dynpred_1) :-
		findall(Value, descendant::p(Value), Solutions),
		Solutions == [root].

	succeeds(dynpred_2) :-
		descendant::assertz(p(descendant)),
		findall(Value, descendant::p(Value), Solutions),
		Solutions == [descendant].

	succeeds(dynpred_3) :-
		descendant::retractall(p(_)),
		findall(Value, descendant::p(Value), Solutions),
		Solutions == [root].

	throws(dynpred_4, error(existence_error(predicate_declaration,p1/1), logtalk(_,_))) :-
		class::p1(_).

	succeeds(dynpred_5) :-
		findall(X, instance::p1(X), Solutions),
		Solutions == [class].

	succeeds(dynpred_6) :-
		class::assertz(p2(class)).

	throws(dynpred_7, error(existence_error(predicate_declaration,p2/1), logtalk(_,_))) :-
		class::p2(_).

	succeeds(dynpred_8) :-
		findall(X, instance::p2(X), Solutions),
		Solutions == [class].

	succeeds(dynpred_9) :-
		class::abolish(p2/1).

	throws(dynpred_10, error(existence_error(predicate_declaration,p2/1), logtalk(_,_))) :-
		instance::p2(_).

	succeeds(dynpred_11) :-
		prototype::(object_assert, self_assert, this_assert).

	succeeds(dynpred_12) :-
		\+ top::get_default(_),
		\+ top::get_value(_),
		\+ middle::get_default(_),
		\+ middle::get_value(_),
		\+ bottom::get_default(_),
		\+ bottom::get_value(_).

	succeeds(dynpred_13) :-
		top::set_default(1),
		top::get_default(Default), Default == 1,
		top::get_value(Default), Default == 1,
		middle::get_default(Default), Default == 1,
		middle::get_value(Default), Default == 1,
		bottom::get_default(Default), Default == 1,
		bottom::get_value(Default), Default == 1.

	succeeds(dynpred_14) :-
		top::set_value(2),
		top::get_default(Default), Default == 2,
		top::get_value(Default), Default == 2,
		middle::set_value(3),
		middle::get_default(Default), Default == 2,
		middle::get_value(MiddleValue), MiddleValue == 3,
		bottom::set_value(4),
		bottom::get_default(Default), Default == 2,
		bottom::get_value(BottomValue), BottomValue == 4.

:- end_object.
