
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/05,
		comment is 'Unit tests for the set_logtalk_flag/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(set_logtalk_flag_2_1, error(instantiation_error, logtalk(set_logtalk_flag(_,_),_))) :-
		{set_logtalk_flag(_, _)}.

	throws(set_logtalk_flag_2_2, error(type_error(atom,1), logtalk(set_logtalk_flag(1,a),_))) :-
		{set_logtalk_flag(1, a)}.

	throws(set_logtalk_flag_2_3, error(domain_error(logtalk_flag,non_existing_flag), logtalk(set_logtalk_flag(non_existing_flag,a),_))) :-
		{set_logtalk_flag(non_existing_flag, a)}.

:- end_object.
