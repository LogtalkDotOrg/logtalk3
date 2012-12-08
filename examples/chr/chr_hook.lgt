%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(chr_hook,
	implements(expanding)).

	:- info([
		version is 0.3,
		author is 'Paulo Moura',
		date is 2011/12/07,
		comment is 'Hook object for compiling objects and categories containing CHR code.']).

	term_expansion((:- chr_constraint(PIs)), [{(:- chr_constraint(TPIs))}]) :-
		logtalk::compile_predicate_indicators(PIs, TPIs).

	term_expansion((:- chr_constraint(H)), [{(:- chr_constraint(TH))}]) :-
		logtalk::compile_predicate_heads(H, TH, '?'(any)).

	% for now, CHR type declarations are global
	term_expansion((:- chr_type(T)), [{(:- chr_type(T))}]).
	% the same for CHR options
	term_expansion((:- chr_option(Option, Value)), [{(:- chr_option(Option, Value))}]).

	value_annotation('@'(Value,Goal), Value, Goal, Head) :-
		goal_annotation(Goal, _, _, Head).
	value_annotation('#'(Head,Value), Value, Head, Head).
	value_annotation(pragma(Head,Value), Value, Head, Head).

	goal_annotation('<=>'(Goal,Body), Goal, Body, Head) :-
		(	goal_annotation(Goal, _, _, Head) ->
			true
		;	Goal = (Head, _) ->
			true
		;	Goal = Head
		).
	goal_annotation('==>'(Head,Body), Head, Body, Head) :-
		(	Goal = (Head, _) ->
			true
		;	Goal = Head
		).
	goal_annotation('\\'(Head,Body), Head, Body, Head).

	body_annotation('|'(Left,Right), Left, Right).

	:- multifile(user::portray/1).
	:- dynamic(user::portray/1).
	user::portray(THead) :-
		callable(THead),
		logtalk::decompile_predicate_heads(THead, Entity, Head),
		writeq(Entity::Head).

:- end_object.
