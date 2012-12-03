
:- object(clause_1_test_object).

	:- public(p/1).
	:- dynamic(p/1).

	:- protected(q/2).
	:- dynamic(q/2).

	:- private(r/3).
	:- dynamic(r/3).

	:- public(s/4).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/03,
		comment is 'Unit tests for the clause/2 built-in method.'
	]).

	throws(clause_1_1, error(instantiation_error, logtalk(clause_1_test_object::clause(_,_),user))) :-
		{clause_1_test_object::clause(_, _)}.

	throws(clause_1_2, error(type_error(callable, 1), logtalk(clause_1_test_object::clause(1,_),user))) :-
		{clause_1_test_object::clause(1, _)}.

	throws(clause_1_3, error(type_error(callable, 1), logtalk(clause_1_test_object::clause(head,1),user))) :-
		{clause_1_test_object::clause(head, 1)}.

	throws(clause_1_4, error(permission_error(access, protected_predicate, q/2), logtalk(clause_1_test_object::clause(q(_,_),_),user))) :-
		{clause_1_test_object::clause(q(_,_), _)}.

	throws(clause_1_5, error(permission_error(access, private_predicate, r/3), logtalk(clause_1_test_object::clause(r(_,_,_),_),user))) :-
		{clause_1_test_object::clause(r(_,_,_), _)}.

	throws(clause_1_6, error(permission_error(access, static_predicate, s/4), logtalk(clause_1_test_object::clause(s(_,_,_,_),_),user))) :-
		{clause_1_test_object::clause(s(_,_,_,_), _)}.

	throws(clause_1_7, error(existence_error(predicate_declaration, unknown/1), logtalk(clause_1_test_object::clause(unknown(_),_),user))) :-
		{clause_1_test_object::clause(unknown(_), _)}.

:- end_object.
