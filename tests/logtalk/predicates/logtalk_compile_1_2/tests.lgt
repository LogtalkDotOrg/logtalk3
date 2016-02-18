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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.01,
		author is 'Paulo Moura',
		date is 2015/02/13,
		comment is 'Unit tests for the logtalk_compile/1-2 built-in predicates.'
	]).

	% logtalk_compile/1

	throws(logtalk_compile_1_1, error(instantiation_error, logtalk(logtalk_compile(_), _))) :-
		logtalk_compile(_).

	throws(logtalk_compile_1_2, error(instantiation_error, logtalk(logtalk_compile([_]), _))) :-
		logtalk_compile([_]).

	throws(logtalk_compile_1_3, error(type_error(source_file_name,1), logtalk(logtalk_compile(1), _))) :-
		logtalk_compile(1).

	throws(logtalk_compile_1_4, error(type_error(source_file_name,1), logtalk(logtalk_compile([1]), _))) :-
		logtalk_compile([1]).

	throws(logtalk_compile_1_5, error(type_error(source_file_name,a(1,2)), logtalk(logtalk_compile(a(1,2)), _))) :-
		logtalk_compile(a(1,2)).

	throws(logtalk_compile_1_6, error(type_error(source_file_name,a(1,2)), logtalk(logtalk_compile([a(1,2)]), _))) :-
		logtalk_compile([a(1,2)]).

	throws(logtalk_compile_1_7, error(existence_error(library, non_exisiting_library), logtalk(logtalk_compile(non_exisiting_library(file)), _))) :-
		logtalk_compile(non_exisiting_library(file)).

	throws(logtalk_compile_1_8, error(existence_error(library, non_exisiting_library), logtalk(logtalk_compile([non_exisiting_library(file)]), _))) :-
		logtalk_compile([non_exisiting_library(file)]).

	throws(logtalk_compile_1_9, error(existence_error(file, non_exisiting_file), logtalk(logtalk_compile(non_exisiting_file), _))) :-
		logtalk_compile(non_exisiting_file).

	throws(logtalk_compile_1_10, error(existence_error(file, non_exisiting_file), logtalk(logtalk_compile([non_exisiting_file]), _))) :-
		logtalk_compile([non_exisiting_file]).

	% logtalk_compile/2

	throws(logtalk_compile_2_1, error(instantiation_error, logtalk(logtalk_compile(_,[]), _))) :-
		logtalk_compile(_, []).

	throws(logtalk_compile_2_2, error(instantiation_error, logtalk(logtalk_compile([_],[]), _))) :-
		logtalk_compile([_], []).

	throws(logtalk_compile_2_3, error(type_error(source_file_name,1), logtalk(logtalk_compile(1,[]), _))) :-
		logtalk_compile(1, []).

	throws(logtalk_compile_2_4, error(type_error(source_file_name,1), logtalk(logtalk_compile([1],[]), _))) :-
		logtalk_compile([1], []).

	throws(logtalk_compile_2_5, error(type_error(source_file_name,a(1,2)), logtalk(logtalk_compile(a(1,2),[]), _))) :-
		logtalk_compile(a(1,2), []).

	throws(logtalk_compile_2_6, error(type_error(source_file_name,a(1,2)), logtalk(logtalk_compile([a(1,2)],[]), _))) :-
		logtalk_compile([a(1,2)], []).

	throws(logtalk_compile_2_7, error(existence_error(library, non_exisiting_library), logtalk(logtalk_compile(non_exisiting_library(file),[]), _))) :-
		logtalk_compile(non_exisiting_library(file), []).

	throws(logtalk_compile_2_8, error(existence_error(library, non_exisiting_library), logtalk(logtalk_compile([non_exisiting_library(file)],[]), _))) :-
		logtalk_compile([non_exisiting_library(file)], []).

	throws(logtalk_compile_2_9, error(existence_error(file, non_exisiting_file), logtalk(logtalk_compile(non_exisiting_file,[]), _))) :-
		logtalk_compile(non_exisiting_file, []).

	throws(logtalk_compile_2_10, error(existence_error(file, non_exisiting_file), logtalk(logtalk_compile([non_exisiting_file],[]), _))) :-
		logtalk_compile([non_exisiting_file], []).

	throws(logtalk_compile_2_11, error(instantiation_error, logtalk(logtalk_compile([],_), _))) :-
		logtalk_compile([], _).

	throws(logtalk_compile_2_12, error(instantiation_error, logtalk(logtalk_compile([],[_]), _))) :-
		logtalk_compile([], [_]).

	throws(logtalk_compile_2_13, error(type_error(list,1), logtalk(logtalk_compile([],1), _))) :-
		logtalk_compile([], 1).

	throws(logtalk_compile_2_14, error(type_error(compound,1), logtalk(logtalk_compile([],[1]), _))) :-
		logtalk_compile([], [1]).

	throws(logtalk_compile_2_15, error(domain_error(compiler_flag,a(1,2)), logtalk(logtalk_compile([],[a(1,2)]), _))) :-
		logtalk_compile([], [a(1,2)]).

	throws(logtalk_compile_2_16, error(domain_error(flag_value,portability+invalid_value), logtalk(logtalk_compile([],[portability(invalid_value)]), _))) :-
		logtalk_compile([], [portability(invalid_value)]).

	throws(logtalk_compile_2_17, error(permission_error(modify,flag,threads), logtalk(logtalk_compile([],[threads(supported)]), _))) :-
		logtalk_compile([], [threads(supported)]).

:- end_object.
