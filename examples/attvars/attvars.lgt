%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- category(attvars).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/03/08,
		comment is 'Runtime support for attributed variables.'
	]).

	:- public(get_attr/2).
	:- mode(get_attr(@var, -term), one).
	:- info(get_attr/2, [
		comment is 'Description',
		argnames is ['Var', 'Value']
	]).

	:- public(put_attr/2).
	:- mode(put_attr(@var, +term), zero_or_one).
	:- info(put_attr/2, [
		comment is 'Description',
		argnames is ['Var', 'Value']
	]).

	:- public(del_attr/1).
	:- mode(del_attr(@var), one).
	:- info(del_attr/1, [
		comment is 'Description',
		argnames is ['Var']
	]).

	:- if(current_logtalk_flag(prolog_dialect, b)).

		get_attr(Var, Value) :-
			this(This),
			Goal =.. [get_attr, Var, This, Value],
			{call(Goal)}.

		put_attr(Var, Value) :-
			this(This),
			Goal =.. [put_attr, Var, This, Value],
			{call(Goal)}.

		del_attr(Var) :-
			this(This),
			{del_attr(Var, This)}.

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		get_attr(Var, Value) :-
			this(This),
			var(Var),
			{get_attr(Var, This, Value)}.

		put_attr(Var, Value) :-
			this(This),
			{put_attr(Var, This, Value)}.

		del_attr(Var) :-
			this(This),
			{del_attr(Var, This)}.

	:- else.

		get_attr(Var, Value) :-
			this(This),
			{get_attr(Var, dispatch, This-Value)}.

		put_attr(Var, Value) :-
			this(This),
			{put_attr(Var, dispatch, This-Value)}.

		del_attr(Var) :-
			{del_attr(Var, dispatch)}.

	:- endif.

:- end_category.
