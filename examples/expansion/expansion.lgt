%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(number_conversion,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	term_expansion(0, zero).
	term_expansion(1, one).
	term_expansion(2, two).
	term_expansion(3, three).
	term_expansion(4, four).
	term_expansion(5, five).
	term_expansion(6, six).
	term_expansion(7, seven).
	term_expansion(8, eight).
	term_expansion(9, nine).

	goal_expansion(write(Term), writeq(Term)).
	goal_expansion(writeq(Term), write_term(Term, [quoted(true)])).

:- end_category.


:- category(conversion_test).

	:- public(test_term_expansion/2).

	test_term_expansion(Term, Expansion) :-
		expand_term(Term, Expansion).	% called in the context of "this"

	:- public(test_goal_expansion/2).

	test_goal_expansion(Goal, EGoal) :-
		expand_goal(Goal, EGoal).		% i.e. from within the object
										% importing the category
:- end_category.


:- object(exp_public,
	imports(public::number_conversion)).

	% the "expanding" protocol implemented by the imported category,
	% "number_conversion", declares term_expansion/2 and goal_expansion/2
	% as public predicates

:- end_object.


:- object(desc_public,
	imports(conversion_test),
	extends(exp_public)).

:- end_object.


:- object(exp_protected,
	imports(protected::number_conversion)).

	% make the predicates term_expansion/2 and goal_expansion/2,
	% defined in the imported category "number_conversion",
	% protected to this prototype and its descendents

:- end_object.


:- object(desc_protected,
	imports(conversion_test),
	extends(exp_protected)).

:- end_object.


:- object(exp_private,
	imports(private::number_conversion)).

	% make the predicates term_expansion/2 and goal_expansion/2,
	% defined in the imported category "number_conversion",
	% private to this prototype

:- end_object.


:- object(desc_private,
	imports(conversion_test),
	extends(exp_private)).

:- end_object.
