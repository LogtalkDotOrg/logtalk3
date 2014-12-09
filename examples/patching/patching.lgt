%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% define an broken object, which will be fixed by defining a complementing
% category containing patches:

:- object(broken).

	% ensure that the object allows complementing categories:
	:- set_logtalk_flag(complements, allow).

	:- public(is_proper_list/1).
	% broken definition that fails to catch variables and lists with unbound tails
	is_proper_list([]).
	is_proper_list([_| List]) :-
		is_proper_list(List).

	:- public(last/2).
	last([Head| Tail], Last) :-
		last(Tail, Head, Last).

	% wrong scope as last/3 is a private, auxiliary predicate
	:- public(last/3).
	last([], Last, Last).
	last([Head| Tail], _, Last) :-
		last(Tail, Head, Last).

:- end_object.


% same example as above but using classes/instances instead of prototypes


% define an broken class, which will be fixed by defining a complementing
% category containing patches:


:- object(metaclass,
	instantiates(metaclass)).

:- end_object.


:- object(broken_class,
	instantiates(metaclass)).

	% ensure that the object allows complementing categories:
	:- set_logtalk_flag(complements, allow).

	:- public(is_proper_list/1).
	% broken definition that fails to catch variables and lists with unbound tails
	is_proper_list([]).
	is_proper_list([_| List]) :-
		is_proper_list(List).

	:- public(last/2).
	last([Head| Tail], Last) :-
		last(Tail, Head, Last).

	% wrong scope as last/3 is a private, auxiliary predicate
	:- public(last/3).
	last([], Last, Last).
	last([Head| Tail], _, Last) :-
		last(Tail, Head, Last).

:- end_object.


:- object(instance,
	instantiates(broken_class)).

:- end_object.


% define a category that patches the broken definition of the predicate
% is_proper_list/1 in the "broken" prototype and in the "broken_class"
% class:

:- category(patch,
	complements((broken, broken_class))).

	% define a correct implementation of the is_proper_list/1 predicate:
	is_proper_list((-)) :-
		!,
		fail.
	is_proper_list([]).
	is_proper_list([_| List]) :-
		is_proper_list(List).

	% correct the scope of the last/3 auxiliary predicate
	:- private(last/3).

:- end_category.
