%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- initialization((
	set_logtalk_flag(optimize, on),
	logtalk_load(lgtunit(loader)),
	logtalk_load([
		protocol,
		crypt,
		derive,
		divide10,
		log10,
		meta_qsort,
		mu,
		nreverse,
		ops8,
		poly_10,
		qsort,
		queens_8,
		query,
		prover,
		reducer,
		sendmore,
		serialise,
		simple_analyzer,
		tak,
		times10,
		unify,
		zebra
	], [
		singleton_variables(silent)
	]),
	logtalk_load(run)
)).


:- if(predicate_property(statistics(_,_), built_in)).

	:- initialization(
		logtalk_load([
			boyer,
			chat_parser
		], [
			optimize(on),
			singleton_variables(silent)
		])
	).

:- endif.


:- if(predicate_property(length(_,_), built_in)).

	:- initialization(
		logtalk_load([
			browse,
			fast_mu
		], [
			optimize(on),
			singleton_variables(silent)
		])
	).

:- endif.


:- if(predicate_property(name(_,_), built_in)).

	:- initialization(
		logtalk_load([
			flatten
		], [
			optimize(on)
		])
	).

:- endif.


:- if(predicate_property(recorda(_,_,_), built_in)).

	:- initialization(
		logtalk_load([
			nand
		], [
			optimize(on),
			singleton_variables(silent)
		])
	).

:- endif.


:- if(current_prolog_flag(bounded, false)).

	:- initialization(
		logtalk_load([
			perfect
		], [
			optimize(on),
			singleton_variables(silent)
		])
	).

:- endif.
