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

	:- set_logtalk_flag(unknown_entities, silent).

	:- info([
		version is 1.3,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/04,
		comment is 'Unit tests for the "reflection" example.'
	]).

	unit(object).
	unit(class).
	unit(abstract_class).

	:- discontiguous(succeeds/1).
	:- discontiguous(throws/2).

	succeeds(reflection_1) :-
		setof(Predicate1, object::current_predicate(Predicate1), Predicates1),
		Predicates1 == [abstract_class/0, delete/1, instances/1, metaclass/0, new/1, print/0, strict_instance/0],
		setof(Predicate2, abstract_class::current_predicate(Predicate2), Predicates2),
		Predicates2 == [abstract_class/0, delete/1, instances/1, metaclass/0, new/1, print/0, strict_instance/0],
		setof(Predicate3, class::current_predicate(Predicate3), Predicates3),
		Predicates3 == [abstract_class/0, delete/1, instances/1, metaclass/0, new/1, print/0, strict_instance/0].

	succeeds(reflection_2) :-
		class::instances(Instances), 
		class::metaclass,
		list::msort(Instances, InstancesSorted),
		InstancesSorted == [abstract_class, class, object].

	succeeds(reflection_3) :-
		abstract_class::new(ac),
		ac::abstract_class,
		setof(Predicate, ac::current_predicate(Predicate), Predicates),
		Predicates == [abstract_class/0, metaclass/0, print/0, strict_instance/0].

	throws(reflection_4, error(existence_error(predicate_declaration,new/1), logtalk(ac::new(i),This))) :-
		this(This),
		ac::new(i).

	succeeds(reflection_5) :-
		class::new(c),
		c::new(i), 
		c::instances(Instances),
		Instances == [i].

	succeeds(reflection_6) :-
    	\+ i::current_predicate(_Predicate).

	succeeds(reflection_7) :-
		object::new(j),
		setof(Predicate, j::current_predicate(Predicate), Predicates),
		Predicates == [print/0, strict_instance/0].

	succeeds(reflection_8) :-
		c::delete(i),
		class::delete(c),
		abstract_class::delete(ac),
		object::delete(j).

:- end_object.
