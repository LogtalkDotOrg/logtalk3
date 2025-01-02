%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- if(current_logtalk_flag(prolog_dialect, swi)).
	:- use_module(library(statistics), []).
:- endif.


:- object(benchmarks).

	:- info([
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2023-03-20,
		comment is 'Some benchmarks for the "jpl" example.'
	]).

	:- if(current_logtalk_flag(prolog_dialect, swi)).
		:- use_module(prolog_statistics, [time/1]).
	:- endif.

	:- if(current_logtalk_flag(prolog_dialect, xvm)).

		:- uses(user, [
			jpl_new/3,
			jpl_call/4
		]).

	:- else.

		:- use_module(jpl, [
			jpl_new/3,
			jpl_call/4
		]).

	:- endif.

	:- public(run/0).
	run :-
		% trigger class loading so that it doesn't interfere with the benchmarks
		jpl_new('java.util.Date', [], _),
		% now we can run the benchmarks
		writeq(((jpl_new('java.util.Date', [], I1), jpl_call(I1, getYear, [], _)))), nl,
		time((jpl_new('java.util.Date', [], I1), jpl_call(I1, getYear, [], _))),
		writeq((java('java.util.Date')::new(I4), java(I4, Year3)::getYear)), nl,
		time((java('java.util.Date')::new(I4), java(I4, Year3)::getYear)),
		writeq((java('java.util.Date')::new(I5), java(I5)::getYear)), nl,
		time((java('java.util.Date')::new(I5), java(I5)::getYear)).

:- end_object.
