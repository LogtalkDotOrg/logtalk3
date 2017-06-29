%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


/*
This file contains an experimental alternative for the code for logical
assignment of Prolog terms developed by Nobukuni Kino. For more information,
please consult the URL http://www.kprolog.com/en/logical_assignment/
*/


:- op(100, xfx, '<=').
:- op(100, xfx, '=>').


:- object(assignvars,
	implements(assignvarsp)).

	:- info([
		version is 1.5,
		author is 'Nobukuni Kino and Paulo Moura',
		date is 2017/06/29,
		comment is 'Assignable variables (supporting backtracable assignement of non-variable terms).'
	]).

	:- if(current_logtalk_flag(prolog_dialect, sicstus)).

		assignable(Assignable) :-
			nonvar(Assignable),
			context(Context),
			throw(error(type_error(variable, Assignable), Context)).
		assignable(Assignable) :-
			create_mutable(s(_), Assignable).

		assignable(Assignable, Init) :-
			nonvar(Assignable),
			context(Context),
			throw(error(type_error(variable, Assignable), Context)).
		assignable(Assignable, Init) :-
			var(Init),
			context(Context),
			throw(error(instantiation_error, Context)).
		assignable(Assignable, Init) :-
			create_mutable(s(Init), Assignable).

		Assignable <= Value :-
			update_mutable(s(Value), Assignable).

		Assignable => Value :-
			get_mutable(s(Value), Assignable),
			nonvar(Value).

	:- elif(current_logtalk_flag(prolog_dialect, cx)).

		assignable(Assignable) :-
			nonvar(Assignable),
			context(Context),
			throw(error(type_error(variable, Assignable), Context)).
		assignable(Assignable) :-
			gensym(Assignable),
			'?:='(Assignable, s(_)).

		assignable(Assignable, Init) :-
			nonvar(Assignable),
			context(Context),
			throw(error(type_error(variable, Assignable), Context)).
		assignable(Assignable, Init) :-
			var(Init),
			context(Context),
			throw(error(instantiation_error, Context)).
		assignable(Assignable, Init) :-
			gensym(Assignable),
			'?:='(Assignable, s(Init)).

		Assignable <= Value :-
			'&:='(Assignable, s(Value)).

		Assignable => Value :-
			'=:'(Assignable, s(Value)),
			nonvar(Value).

	:- elif((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == b; Dialect == eclipse; Dialect == gnu; Dialect == qp; Dialect == swi; Dialect == yap)
	)).

		assignable(Assignable) :-
			nonvar(Assignable),
			context(Context),
			throw(error(type_error(variable, Assignable), Context)).
		assignable('$'(_,_)).

		assignable(Assignable, _) :-
			nonvar(Assignable),
			context(Context),
			throw(error(type_error(variable, Assignable), Context)).
		assignable(_, Init) :-
			var(Init),
			context(Context),
			throw(error(instantiation_error, Context)).
		assignable('$'(Init,_), Init).

		Assignable <= Value :-
			{setarg(1, Assignable, Value)}.

		'$'(Value,_) => Value :-
			nonvar(Value).

	:- endif.

:- end_object.
