%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(sicstus_clpfd_hook,
	implements(expanding)).

	:- info([
		version is 0.4,
		author is 'Paulo Moura',
		date is 2013/09/23,
		comment is 'Hook object for compiling objects and categories containing CLP(FD) code when using SICStus Prolog.'
	]).

	term_expansion((:- Directive), [(:- Directive)| Annotations]) :-
		nonvar(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		),
		clpfd_annotations(Annotations).

	term_expansion('+:'(Head, Body), [{'+:'(THead, Body)}, (Head :- {THead})]) :-
		compile_indexical_head(Head, THead).
	term_expansion('-:'(Head, Body), [{'+:'(THead, Body)}, (Head :- {THead})]) :-
		compile_indexical_head(Head, THead).
	term_expansion('+?'(Head, Body), [{'+:'(THead, Body)}, (Head :- {THead})]) :-
		compile_indexical_head(Head, THead).
	term_expansion('-?'(Head, Body), [{'+:'(THead, Body)}, (Head :- {THead})]) :-
		compile_indexical_head(Head, THead).

	compile_indexical_head(Head, THead) :-
		logtalk::compile_predicate_heads(Head, _, CHead, _),
		CHead =.. [CFunctor| CArgs],
		copy_args_except_last(CArgs, TArgs),	% remove execution-context argument
		atom_concat(CFunctor, '+', TFunctor0),
		functor(Head, _, Arity),
		number_codes(Arity, ArityCodes),
		atom_codes(ArityAtom, ArityCodes),
		atom_concat(TFunctor0, ArityAtom, TFunctor),
		THead =.. [TFunctor| TArgs].

	copy_args_except_last([_], []) :-
		!.
	copy_args_except_last([Arg| CArgs], [Arg| TArgs]) :-
		copy_args_except_last(CArgs, TArgs).

	clpfd_annotations([
		(:- meta_predicate(clpfd:labeling(*,*))),
		(:- meta_predicate(clpfd:maximize(0,*))),
		(:- meta_predicate(clpfd:minimize(0,*))),
		(:- meta_predicate(clpfd:fd_global(0,*,*))),
		(:- meta_predicate(clpfd:'#\\'(*))),
		(:- meta_predicate(clpfd:'#/\\'(*,*))),
		(:- meta_predicate(clpfd:'#\\'(*,*))),
		(:- meta_predicate(clpfd:'#\\/'(*,*))),
		(:- meta_predicate(clpfd:'#=>'(*,*))),
		(:- meta_predicate(clpfd:'#<='(*,*))),
		(:- meta_predicate(clpfd:'#<=>'(*,*)))
	]).

	:- multifile(user::portray/1).
	:- dynamic(user::portray/1).
	user::portray(THead) :-
		callable(THead),
		logtalk::decompile_predicate_heads(THead, Entity, _, Head),
		writeq(Entity::Head).

:- end_object.
