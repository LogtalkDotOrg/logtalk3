%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


/*
This file contains an adaptation to Logtalk of code for logical assignment 
of Prolog terms developed by Nobukuni Kino. For more information, please 
consult the URL http://www.kprolog.com/en/logical_assignment/

As a derivative work, this file is licensed under the Open Software License 
version 2.1 (http://opensource.org/licenses/osl-2.1.php).
*/


:- op(100, xfx, '<=').
:- op(100, xfx, '=>').


:- object(assignvars,
	implements(assignvarsp)).

	:- info([
		version is 1.4,
		author is 'Nobukuni Kino and Paulo Moura',
		date is 2017/06/29,
		comment is 'Assignable variables (supporting backtracable assignement of non-variable terms).'
	]).

	assignable(Assignable) :-
		nonvar(Assignable),
		context(Context),
		throw(error(type_error(variable, Assignable), Context)).
	assignable([_| _]).

	assignable(Assignable, Init) :-
		nonvar(Assignable),
		context(Context),
		throw(error(type_error(variable, Assignable), Context)).
	assignable(Assignable, Init) :-
		var(Init),
		context(Context),
		throw(error(instantiation_error, Context)).
	assignable([_, Init| _], Init).

	Assignable <= Value :-
		var(Value),
		context(Context),
		throw(error(instantiation_error, Context)).

	[_| Tail] <= Value :-
		put_assign([_| Tail], Value).

	put_assign([_| Tail], Value) :-
		(	nonvar(Tail) ->
			put_assign(Tail, Value)
		;	Tail = [Value| _]
		).

	Assignable => Value :-
		var(Assignable),
		context(Context),
		throw(error(instantiation_error, Context)).

	[_| Tail] => Value :-
		nonvar(Tail),
		peek_assign(Tail, Value).

	peek_assign([Current| Tail], Value) :-
		(	nonvar(Tail) ->
			peek_assign(Tail, Value)
		;	Current = Value
		).

:- end_object.
