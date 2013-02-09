%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- multifile(user:problog_user_ground/1).
user:problog_user_ground(THead) :-
	logtalk::decompile_predicate_heads(THead, Head),
	ground(Head).
user:problog_user_ground(THead) :-
	THead =.. [ProbLogFunctor| Args],
	atom_concat(problogcontinuous_, LogtalkFunctor, ProbLogFunctor),
	THead2 =.. [LogtalkFunctor| Args],
	logtalk::decompile_predicate_heads(THead2, Head),
	ground(Head).


:- op(550, yfx, ~).		% alternative to ProbLog (::)/2 operator
:- multifile(problog:(~)/2).


:- object(problog_hook,
	implements(expanding)).

	:- info([
		version is 0.5,
		author is 'Paulo Moura',
		date is 2012/12/08,
		comment is 'Hook object for compiling objects and categories containing ProbLog code.'
	]).

	term_expansion((:- set_problog_flag(Flag, Value)), [{(:- flags:set_problog_flag(Flag, Value))}]).

	term_expansion((:- problog_table(PI)), [{(:- problog:problog_table(user:TPI))}]) :-
		logtalk::compile_predicate_indicators(PI, TPI).

	value_annotation('=>'(Head, Value), Value, Head, Head).
	value_annotation('~'(Value, Head), Value, Head, Head).
	goal_annotation('<--'(Left,Right), Left, Right, Left).

	:- multifile(user:term_expansion/2).
	:- dynamic(user:term_expansion/2).

	user:term_expansion('~'(Prob, Fact), Expansion) :-
		user:term_expansion('::'(Prob, Fact), Expansion).

	user:term_expansion(('~'(Prob, Head) :- Body), Expansion) :-
		user:term_expansion(('::'(Prob, Head) :- Body), Expansion).

:- end_object.
