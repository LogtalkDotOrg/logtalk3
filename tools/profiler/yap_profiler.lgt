%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(profiler,
	implements(profilerp)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura and Vitor Santos Costa',
		date is 2013/09/23,
		comment is 'Simple wrapper for the YAP count profiler.'
	]).

	load(File) :-
		current_prolog_flag(profiling, Current),
		set_prolog_flag(profiling, on),
		logtalk_load(File),
		set_prolog_flag(profiling, Current).

	load(File, Options) :-
		current_prolog_flag(profiling, Current),
		set_prolog_flag(profiling, on),
		logtalk_load(File, Options),
		set_prolog_flag(profiling, Current).

	:- meta_predicate(profile(0)).
	profile(Goal) :-
		call(Goal).

	data :-
		(	setof(Calls-[Predicate|Retries], data(Predicate, Calls, Retries), Data) ->
			write_profile_data(Data)
		;	write_profile_data([])
		).

	data(Predicate, Calls, Retries) :-
		{profile_data(Predicate, calls, Calls),
		 profile_data(Predicate, retries, Retries)}.

	write_profile_data(Data) :-
		format('~*c~n',[64,0'-]),
		format('~w ~40+~t~w~12+~t~w~12+~n', ['Predicate', 'Calls', 'Retries']),
		format('~*c~n',[64,0'-]),
		(	Data == [] ->
			format('~w~n', ['(no profiling data available)'])
		;	write_profile_data_rows(Data)
		),
		format("~*c~n",[64,0'-]).

	write_profile_data_rows([]).
	write_profile_data_rows([Calls-[Predicate|Retries]| Rest]) :-
		predicate_label(Predicate, Label),
		format('~w ~40+~t~d~12+~t~d~12+~n', [Label, Calls, Retries]),
		write_profile_data_rows(Rest).

	predicate_label(TFunctor/TArity, Label) :-
		predicate_label(user:TFunctor/TArity, Label).
	predicate_label(Module:TFunctor/TArity, Label) :-
		(	Module == user,
			logtalk::decompile_predicate_indicators(TFunctor/TArity, Entity, _, Functor/Arity) ->
			(	atom(Entity) ->
				atomic_list_concat([Entity, '::', Functor, '/', Arity], Label)
			;	functor(Entity, EntityFunctor, EntityArity),
				atomic_list_concat([EntityFunctor, '/', EntityArity, '::', Functor, '/', Arity], Label)
			)
		;	Label = Module:TFunctor/TArity
		).

	data(Entity) :-
		nonvar(Entity),
		(	setof(Calls-[Predicate|Retries], data(Entity, Predicate, Calls, Retries), Data) ->
			write_entity_profile_data(Entity, Data)
		;	write_entity_profile_data(Entity, [])
		).

	data(Entity, Predicate, Calls, Retries) :-
		{profile_data(Term, calls, Calls)},
		(	Term = Module:TFunctor/TArity ->
			Module == user
		;	Term = TFunctor/TArity
		),
		logtalk::decompile_predicate_indicators(TFunctor/TArity, Entity, _, Functor/Arity),
		{profile_data(Term, retries, Retries)},
		Predicate = Functor/Arity.

	write_entity_profile_data(Entity, Data) :-
		format("~*c~n",[64,0'-]),
		(	atom(Entity) ->
			EntityLabel = Entity
		;	functor(Entity, EntityFunctor, EntityArity),
			atomic_list_concat([EntityFunctor, '/', EntityArity], EntityLabel)
		),
		format("~w~n",[EntityLabel]),
		format('~w ~40+~t~w~12+~t~w~12+~n', ['Predicate', 'Calls', 'Retries']),
		format("~*c~n",[64,0'-]),
		(	Data == [] ->
			format('~w~n', ['(no profiling data available for this entity)'])
		;	write_entity_profile_data_rows(Data)
		),
		format("~*c~n",[64,0'-]).

	write_entity_profile_data_rows([]).
	write_entity_profile_data_rows([Calls-[Functor/Arity|Retries]| Rest]) :-
		atomic_concat([Functor, '/', Arity], Label),
		format('~w ~40+~t~d~12+~t~d~12+~n', [Label, Calls, Retries]),
		write_entity_profile_data_rows(Rest).

	reset :-
		profile_reset.

:- end_object.
