%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(chr_hook,
	implements(expanding)).

	:- info([
		version is 0.21,
		author is 'Paulo Moura',
		date is 2011/02/24,
		comment is 'Hook object for compiling objects and categories containing CHR code.']).

	term_expansion((:- chr_constraint(PIs)), [{(:- chr_constraint(TPIs))}]) :-
		logtalk::compile_predicate_indicators(PIs, TPIs).

	term_expansion((:- chr_constraint(H)), [{(:- chr_constraint(TH))}]) :-
		logtalk::compile_predicate_heads(H, TH, '?'(any)).

	% for now, CHR type declarations are global
	term_expansion((:- chr_type(T)), [{(:- chr_type(T))}]).
	% the same for CHR options
	term_expansion((:- chr_option(Option, Value)), [{(:- chr_option(Option, Value))}]).

	term_expansion((:- Directive), [(:- Directive)| Annotations]) :-
		nonvar(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		),
		chr_annotations(Annotations).

	chr_annotations([
		(:- annotation('@'(*,0))),
		(:- annotation('==>'(0,0))),
		(:- annotation('<=>'(0,0))),
		(:- annotation('|'(0,0))),
		(:- annotation('\\'(0,0))),
		(:- annotation('#'(0,*))),
		(:- annotation(pragma(0,*)))
	]).

	:- multifile(user::portray/1).
	:- dynamic(user::portray/1).
	user::portray(THead) :-
		callable(THead),
		logtalk::decompile_predicate_heads(THead, Entity, Head),
		writeq(Entity::Head).

:- end_object.
