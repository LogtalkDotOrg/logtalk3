%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(mode_checker,
	implements(expanding),
	imports(monitor)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2009/10/24,
		comment is 'Simple message mode checker based on hooks and events.'
	]).

	:- private(compiling_object_/1).
	:- dynamic(compiling_object_/1).

	term_expansion((:- object(Object)), [(:- object(Object))]) :-
		asserta(compiling_object_(Object)).
	term_expansion((:- end_object), [(:- end_object)]) :-
		retractall(compiling_object_(_)).

	term_expansion((:- mode(Template, NumberOfSolutions)), [(:- mode(Template, NumberOfSolutions))]) :-
		Template =.. [Functor| Args],
		generate_checks(Args, 0, Arity),
		functor(Message, Functor, Arity),
		this(This),
		compiling_object_(Object),
		define_events(before, Object, Message, _, This),
		define_events(after, Object, Message, _, This).

	generate_checks(Args, Arity0, Arity) :-
		generate_checks(Args, Arity0, Arity, true, BeforeGoals, true, AfterGoals).

	generate_checks([], Arity, Arity, BeforeGoals, BeforeGoals, AfterGoals, AfterGoals).
	generate_checks([Arg| Args], Arity0, Arity, BeforeGoals0, BeforeGoals, AfterGoals0, AfterGoals) :-
		Arity1 is Arity0 + 1,
		generate_check(Arg, Arity1, BeforeGoals0, BeforeGoals1, AfterGoals0, AfterGoals1),
		generate_checks(Args, Arity1, Arity, BeforeGoals1, BeforeGoals, AfterGoals1, AfterGoals).

	generate_check(-Type, N, BeforeGoals0, BeforeGoals1, AfterGoals0, AfterGoals1) :-
		BeforeGoals1 = (BeforeGoals0, arg(N, Message, Arg), var(Arg))

	before(Object, Message, Sender) :-
		before_check_goal(Object, Message, Sender).

	after(Object, Message, Sender) :-
		after_check_goal(Object, Message, Sender).

:- end_object.
