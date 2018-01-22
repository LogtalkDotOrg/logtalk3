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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2016/07/07,
		comment is 'Unit tests for the logtalk_load/1-2 built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% logtalk_load/1

	throws(logtalk_load_1_1, error(instantiation_error, logtalk(logtalk_load(_), _))) :-
		logtalk_load(_).

	throws(logtalk_load_1_2, error(instantiation_error, logtalk(logtalk_load([_]), _))) :-
		logtalk_load([_]).

	throws(logtalk_load_1_3, error(type_error(source_file_name,1), logtalk(logtalk_load(1), _))) :-
		logtalk_load(1).

	throws(logtalk_load_1_4, error(type_error(source_file_name,1), logtalk(logtalk_load([1]), _))) :-
		logtalk_load([1]).

	throws(logtalk_load_1_5, error(type_error(source_file_name,a(1,2)), logtalk(logtalk_load(a(1,2)), _))) :-
		logtalk_load(a(1,2)).

	throws(logtalk_load_1_6, error(type_error(source_file_name,a(1,2)), logtalk(logtalk_load([a(1,2)]), _))) :-
		logtalk_load([a(1,2)]).

	throws(logtalk_load_1_7, error(existence_error(library, non_exisiting_library), logtalk(logtalk_load(non_exisiting_library(file)), _))) :-
		logtalk_load(non_exisiting_library(file)).

	throws(logtalk_load_1_8, error(existence_error(library, non_exisiting_library), logtalk(logtalk_load([non_exisiting_library(file)]), _))) :-
		logtalk_load([non_exisiting_library(file)]).

	throws(logtalk_load_1_9, error(existence_error(file, non_exisiting_file), logtalk(logtalk_load(non_exisiting_file), _))) :-
		logtalk_load(non_exisiting_file).

	throws(logtalk_load_1_10, error(existence_error(file, non_exisiting_file), logtalk(logtalk_load([non_exisiting_file]), _))) :-
		logtalk_load([non_exisiting_file]).

	succeeds(logtalk_load_1_11) :-
		logtalk_load(a1, []).

	succeeds(logtalk_load_1_12) :-
		logtalk_load(a2, []).

	succeeds(logtalk_load_1_13) :-
		logtalk_load(a3, []).

	succeeds(logtalk_load_1_14) :-
		logtalk_load(a4, []).

	succeeds(logtalk_load_1_15) :-
		logtalk_load('a5.b.c', []).

	succeeds(logtalk_load_1_16) :-
		logtalk_load('a6.b.c', []).

	succeeds(logtalk_load_1_17) :-
		logtalk_load('a7.b.c', []).

	succeeds(logtalk_load_1_18) :-
		logtalk_load('a8.b.c', []).

	succeeds(logtalk_load_1_19) :-
		logtalk_load('a9.foo', []).

	succeeds(logtalk_load_1_20) :-
		logtalk_load(a10, []).

	% logtalk_load/2

	throws(logtalk_load_2_1, error(instantiation_error, logtalk(logtalk_load(_,[]), _))) :-
		logtalk_load(_, []).

	throws(logtalk_load_2_2, error(instantiation_error, logtalk(logtalk_load([_],[]), _))) :-
		logtalk_load([_], []).

	throws(logtalk_load_2_3, error(type_error(source_file_name,1), logtalk(logtalk_load(1,[]), _))) :-
		logtalk_load(1, []).

	throws(logtalk_load_2_4, error(type_error(source_file_name,1), logtalk(logtalk_load([1],[]), _))) :-
		logtalk_load([1], []).

	throws(logtalk_load_2_5, error(type_error(source_file_name,a(1,2)), logtalk(logtalk_load(a(1,2),[]), _))) :-
		logtalk_load(a(1,2), []).

	throws(logtalk_load_2_6, error(type_error(source_file_name,a(1,2)), logtalk(logtalk_load([a(1,2)],[]), _))) :-
		logtalk_load([a(1,2)], []).

	throws(logtalk_load_2_7, error(existence_error(library, non_exisiting_library), logtalk(logtalk_load(non_exisiting_library(file),[]), _))) :-
		logtalk_load(non_exisiting_library(file), []).

	throws(logtalk_load_2_8, error(existence_error(library, non_exisiting_library), logtalk(logtalk_load([non_exisiting_library(file)],[]), _))) :-
		logtalk_load([non_exisiting_library(file)], []).

	throws(logtalk_load_2_9, error(existence_error(file, non_exisiting_file), logtalk(logtalk_load(non_exisiting_file,[]), _))) :-
		logtalk_load(non_exisiting_file, []).

	throws(logtalk_load_2_10, error(existence_error(file, non_exisiting_file), logtalk(logtalk_load([non_exisiting_file],[]), _))) :-
		logtalk_load([non_exisiting_file], []).

	throws(logtalk_load_2_11, error(instantiation_error, logtalk(logtalk_load([],_), _))) :-
		logtalk_load([], _).

	throws(logtalk_load_2_12, error(instantiation_error, logtalk(logtalk_load([],[_]), _))) :-
		logtalk_load([], [_]).

	throws(logtalk_load_2_13, error(type_error(list,1), logtalk(logtalk_load([],1), _))) :-
		logtalk_load([], 1).

	throws(logtalk_load_2_14, error(type_error(compound,1), logtalk(logtalk_load([],[1]), _))) :-
		logtalk_load([], [1]).

	throws(logtalk_load_2_15, error(domain_error(compiler_flag,a(1,2)), logtalk(logtalk_load([],[a(1,2)]), _))) :-
		logtalk_load([], [a(1,2)]).

	throws(logtalk_load_2_16, error(domain_error(flag_value,portability+invalid_value), logtalk(logtalk_load([],[portability(invalid_value)]), _))) :-
		logtalk_load([], [portability(invalid_value)]).

	throws(logtalk_load_2_17, error(permission_error(modify,flag,threads), logtalk(logtalk_load([],[threads(supported)]), _))) :-
		logtalk_load([], [threads(supported)]).

	succeeds(logtalk_load_2_18) :-
		logtalk_load(a1, []).

	succeeds(logtalk_load_2_19) :-
		logtalk_load(a2, []).

	succeeds(logtalk_load_2_20) :-
		logtalk_load(a3, []).

	succeeds(logtalk_load_2_21) :-
		logtalk_load(a4, []).

	succeeds(logtalk_load_2_22) :-
		logtalk_load('a5.b.c', []).

	succeeds(logtalk_load_2_23) :-
		logtalk_load('a6.b.c', []).

	succeeds(logtalk_load_2_24) :-
		logtalk_load('a7.b.c', []).

	succeeds(logtalk_load_2_25) :-
		logtalk_load('a8.b.c', []).

	succeeds(logtalk_load_2_26) :-
		logtalk_load('a9.foo', []).

	succeeds(logtalk_load_2_27) :-
		logtalk_load(a10, []).

:- end_object.
