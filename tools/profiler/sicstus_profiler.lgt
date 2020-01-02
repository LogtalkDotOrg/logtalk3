%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.6,
		author is 'Paulo Moura',
		date is 2018/07/12,
		comment is 'Simple wrapper for the SICStus Prolog profiler.'
	]).

	:- if((current_logtalk_flag(prolog_version, v(4, Minor, _)), Minor >= 2)).

		load(File) :-
			logtalk_load(File).

		load(File, Options) :-
			logtalk_load(File, Options).

		:- meta_predicate(profile(0)).
		profile(Goal) :-
			current_prolog_flag(profiling, Current),
			set_prolog_flag(profiling, on),
			call_cleanup(Goal, set_prolog_flag(profiling, Current)).

		data :-
			{profile_data(Data)},
			filter_profile_data(Data, FilteredData),
			{print_profile(FilteredData)}.

		filter_profile_data([], []).
		filter_profile_data([(user:Functor/_)-_| Data], FilteredData) :-
			(	sub_atom(Functor, 0, 5, _, '$lgt_')
			;	sub_atom(Functor, _, _, 0, '._dcl')
			;	sub_atom(Functor, _, _, 0, '._idcl')
			;	sub_atom(Functor, _, _, 0, '._def')
			;	sub_atom(Functor, _, _, 0, '._ddef')
			;	sub_atom(Functor, _, _, 0, '._idef')
			;	sub_atom(Functor, _, _, 0, '._super')
			;	sub_atom(Functor, _, _, 0, '._alias')
			),
			% Logtalk compiler/runtime internal predicate
			!,
			filter_profile_data(Data, FilteredData).
		filter_profile_data([Caller-counter(Callees,Insns,Chpts,TaggedCalls)| Data], [Label-counter(FilteredCallees,Insns,Chpts,TaggedCalls)| FilteredData]) :-
			predicate_label(Caller, Label),
			filter_profile_data(Callees, FilteredCallees),
			!,
			filter_profile_data(Data, FilteredData).
		filter_profile_data([Caller-Info| Data], [Label-Info| FilteredData]) :-
			predicate_label(Caller, Label),
			!,
			filter_profile_data(Data, FilteredData).
		filter_profile_data([Datum| Data], [Datum| FilteredData]) :-
			filter_profile_data(Data, FilteredData).

		reset :-
			{profile_reset}.

	:- else.

		load(File) :-
			current_prolog_flag(compiling, Current),
			set_prolog_flag(compiling, profiledcode),
			logtalk_load(File),
			set_prolog_flag(compiling, Current).

		load(File, Options) :-
			current_prolog_flag(compiling, Current),
			set_prolog_flag(compiling, profiledcode),
			logtalk_load(File, Options),
			set_prolog_flag(compiling, Current).

		:- meta_predicate(profile(0)).
		profile(Goal) :-
			call(Goal).

		data :-
			data(_:_, CallsData, ChoicePointsData, InstructionsData),
			(	setof(
					Calls-[Predicate,ChoicePoints,Instructions],
					(member(Predicate-Calls, CallsData), Calls > 0,
					member(Predicate-ChoicePoints, ChoicePointsData),
					member(Predicate-Instructions, InstructionsData)),
					Data) ->
				write_profile_data(Data)
			;	write_profile_data([])
			).

		data(Spec, CallsData, ChoicePointsData, InstructionsData) :-
			{profile_data(Spec, calls, predicate, CallsData),
			 profile_data(Spec, choice_points, predicate, ChoicePointsData),
			 profile_data(Spec, instructions, predicate, InstructionsData)}.

		write_profile_data(Data) :-
			format("~*c~n",[82,0'-]),
			format('~w ~40+~t~w~14+~t~w~14+~t~w~14+~n', ['Predicate', 'Calls', 'Choice-points', 'Instructions']),
			format("~*c~n",[82,0'-]),
			(	Data == [] ->
				format('~w~n', ['(no profiling data available)'])
			;	write_profile_data_rows(Data)
			),
			format("~*c~n",[82,0'-]).

		write_profile_data_rows([]).
		write_profile_data_rows([Calls-[Predicate,ChoicePoints,Instructions]| Rest]) :-
			predicate_label(Predicate, Label),
			format('~w ~40+~t~d~14+~t~d~14+~t~d~14+~n', [Label, Calls, ChoicePoints, Instructions]),
			write_profile_data_rows(Rest).

		data(Entity) :-
			nonvar(Entity),
			data(user:_, CallsData, ChoicePointsData, InstructionsData),
			(	setof(
					Calls-[Functor/Arity,ChoicePoints,Instructions],
					Type^TFunctor^TArity^(member((user:TFunctor/TArity)-Calls, CallsData), Calls > 0,
					 logtalk::decompile_predicate_indicators(TFunctor/TArity, Entity, Type, Functor/Arity),
					 member((user:TFunctor/TArity)-ChoicePoints, ChoicePointsData),
					 member((user:TFunctor/TArity)-Instructions, InstructionsData)),
					Data) ->
				write_entity_profile_data(Entity, Data)
			;	write_entity_profile_data(Entity, [])
			).

		write_entity_profile_data(Entity, Data) :-
			format("~*c~n",[82,0'-]),
			(	atom(Entity) ->
				EntityLabel = Entity
			;	functor(Entity, EntityFunctor, EntityArity),
				atomic_list_concat([EntityFunctor, '/', EntityArity], EntityLabel)
			),
			format("~w~n",[EntityLabel]),
			format('~w ~40+~t~w~14+~t~w~14+~t~w~14+~n', ['Predicate', 'Calls', 'Choice-points', 'Instructions']),
			format("~*c~n",[82,0'-]),
			(	Data == [] ->
				format('~w~n', ['(no profiling data available for this entity)'])
			;	write_entity_profile_data_rows(Data)
			),
			format("~*c~n",[82,0'-]).

		write_entity_profile_data_rows([]).
		write_entity_profile_data_rows([Calls-[Functor/Arity,ChoicePoints,Instructions]| Rest]) :-
			atomic_list_concat([Functor, '/', Arity], Label),
			format('~w ~40+~t~d~14+~t~d~14+~t~d~14+~n', [Label, Calls, ChoicePoints, Instructions]),
			write_entity_profile_data_rows(Rest).

		reset :-
			{profile_reset(_:_)}.

		atomic_list_concat(List, Atom) :-
			atomic_list_concat(List, '', Atom).

		atomic_list_concat([], Atom, Atom).
		atomic_list_concat([Atomic| Atomics], Acc, Atom) :-
			(	atom(Atomic) ->
				atom_concat(Acc, Atomic, Acc2)
			;	number_codes(Atomic, Codes),
				atom_codes(Converted, Codes),
				atom_concat(Acc, Converted, Acc2)
			),
			atomic_list_concat(Atomics, Acc2, Atom).

	:- endif.

		predicate_label(TFunctor/TArity, Label) :-
			predicate_label(user:TFunctor/TArity, Label).
		predicate_label(Module:TFunctor/TArity, Label) :-
			(	Module:TFunctor/TArity == user:(::)/2 ->
				Label = (::)/2
			;	Module:TFunctor/TArity == user:(<<)/2 ->
				Label = (<<)/2
			;	Module == user,
				logtalk::decompile_predicate_indicators(TFunctor/TArity, Entity, _, Functor/Arity) ->
				(	atom(Entity) ->
					Label = Entity::Functor/Arity
				;	functor(Entity, EntityFunctor, EntityArity),
					Label = EntityFunctor/EntityArity::Functor/Arity
				)
			;	Label = Module:TFunctor/TArity
			).

:- end_object.
