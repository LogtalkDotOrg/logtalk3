
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/03/24,
		comment is 'Unit tests for the "errors" example.'
	]).

	% redefinition of built-in methods

	throws(
		object_redefines_built_in_method,
		error(permission_error(modify,built_in_method,asserta/1),_)
	) :-
		create_object(_, [], [], [asserta(_)]).

	throws(
		category_redefines_built_in_method,
		error(permission_error(modify,built_in_method,asserta/1),_)
	) :-
		create_category(_, [], [], [asserta(_)]).

	% invalid clause heads

	throws(
		object_invalid_clause_head,
		error(type_error(callable,1234),_)
	) :-
		create_object(_, [], [], [(1234 :- write(hello))]).

	throws(
		category_invalid_clause_head,
		error(type_error(callable,1234),_)
	) :-
		create_category(_, [], [], [(1234 :- write(hello))]).

	% invalid clause body goals

	throws(
		object_invalid_clause_goal,
		error(type_error(callable,1234),_)
	) :-
		create_object(_, [], [], [(foo :- 1234)]).

	throws(
		category_invalid_clause_goal,
		error(type_error(callable,1234),_)
	) :-
		create_category(_, [], [], [(foo :- 1234)]).

	% unknown directives

	throws(
		object_unknown_directive,
		error(domain_error(directive,index/2),_)
	) :-
		create_object(_, [], [index(predicate/3, [1, 2])], []).

	throws(
		category_unknown_directive,
		error(domain_error(directive,index/2),_)
	) :-
		create_category(_, [], [index(predicate/3, [1, 2])], []).

	throws(
		protocol_unknown_directive,
		error(domain_error(directive,index/2),_)
	) :-
		create_protocol(_, [], [index(predicate/3, [1, 2])]).

	% invalid directive arguments

	throws(
		object_invalid_directive_argument,
		error(type_error(predicate_indicator,1234),_)
	) :-
		create_object(_, [], [public(1234)], []).

	throws(
		category_invalid_directive_argument,
		error(type_error(predicate_indicator,1234),_)
	) :-
		create_category(_, [], [public(1234)], []).

	throws(
		protocol_invalid_directive_argument,
		error(type_error(predicate_indicator,1234),_)
	) :-
		create_protocol(_, [], [public(1234)]).

	% category defining dynamic predicate

	throws(
		category_defines_dynamic_predicate,
		error(permission_error(define,dynamic_predicate,dynpred/1),_)
	) :-
		create_category(_, [], [dynamic(dynpred/1)], [dynpred(1)]).

	% control-constructs redefinition

	throws(
		object_control_construct_redefinition,
		error(permission_error(modify,built_in_method,(::)/1),_)
	) :-
		create_object(_, [], [], [::(_)]).

	throws(
		category_control_construct_redefinition,
		error(permission_error(modify,built_in_method,(::)/1),_)
	) :-
		create_category(_, [], [], [::(_)]).

	% conflict between uses/2 directives

	throws(
		object_uses_predicate_repeated,
		error(permission_error(modify,uses_object_predicate,member/2),_)
	) :-
		create_object(_, [], [uses(list,[member/2]), uses(set,[member/2])], []).

	throws(
		category_uses_predicate_repeated,
		error(permission_error(modify,uses_object_predicate,member/2),_)
	) :-
		create_category(_, [], [uses(list,[member/2]), uses(set,[member/2])], []).

	% conflict between uses/2 directive and local predicate definition

	throws(
		object_uses_predicate_conflict,
		error(permission_error(modify,uses_object_predicate,member/2),_)
	) :-
		create_object(_, [], [uses(list,[member/2])], [member(H, [H| _])]).

	throws(
		category_uses_predicate_conflict,
		error(permission_error(modify,uses_object_predicate,member/2),_)
	) :-
		create_category(_, [], [uses(list,[member/2])], [member(H, [H| _])]).

:- end_object.
