%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(problog), []).
:- use_module(library(dtproblog), []).
:- use_module(library(problog_learning), []).


:- multifile(user:problog_user_ground/1).
user:problog_user_ground(THead) :-
	logtalk::decompile_predicate_heads(THead, Head),
	ground(Head).


:- op(550, yfx, ~).		% alternative to ProbLog (::)/2 operator
:- discontiguous((~)/2).


:- object(problog_hook,
	implements(expanding)).

	:- info([
		version is 0.4,
		author is 'Paulo Moura',
		date is 2010/12/02,
		comment is 'Hook object for compiling objects and categories containing ProbLog code.']).

	term_expansion((:- set_problog_flag(Flag, Value)), [{(:- flags:set_problog_flag(Flag, Value))}]).

	term_expansion((:- problog_table(PI)), [{(:- problog:problog_table(user:TPI))}]) :-
		logtalk::compile_predicate_indicators(PI, TPI).

	term_expansion((:- Directive), [(:- Directive)| Annotations]) :-
		nonvar(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		),
		problog_annotations(Annotations).

	problog_annotations([
		(:- annotation('=>'(0, *))),
		(:- annotation('~'(*, 0))),
		(:- annotation('<--'(0, 0)))
	]).

	:- multifile(user:term_expansion/2).
	:- dynamic(user:term_expansion/2).

	user:term_expansion('~'(Prob, Fact), Expansion) :-
		user:term_expansion('::'(Prob, Fact), Expansion).

	user:term_expansion(('~'(Prob, Head) :- Body), Expansion) :-
		user:term_expansion(('::'(Prob, Head) :- Body), Expansion).

:- end_object.
