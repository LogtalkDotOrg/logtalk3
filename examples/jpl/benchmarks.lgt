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


:- set_logtalk_flag(optimize, on).

:- if(current_logtalk_flag(prolog_dialect, swi)).
	:- use_module(library(statistics), []).
:- endif.


:- object(benchmarks).

	:- info([
		version is 0.2,
		author is 'Paulo Moura',
		date is 2016/09/19,
		comment is 'Some benchmarks for the "jpl" example.'
	]).

	:- if(current_logtalk_flag(prolog_dialect, swi)).
		:- use_module(prolog_statistics, [time/1]).
	:- endif.

	:- public(run/0).
	run :-
		% trigger class loading so that it doesn't interfere with the benchmarks
		jpl:jpl_new('java.util.Date', [], _),
		% now we can run the benchmarks
		writeq(((jpl:jpl_new('java.util.Date', [], I1), jpl:jpl_call(I1, getYear, [], _)))), nl,
		time((jpl:jpl_new('java.util.Date', [], I1), jpl:jpl_call(I1, getYear, [], _))),
		writeq((java('java.util.Date')::new(I2), java(I2, Year2)::invoke(getYear))), nl,
		time((java('java.util.Date')::new(I2), java(I2, Year2)::invoke(getYear))),
		writeq((java('java.util.Date')::new(I3), java(I3)::invoke(getYear))), nl,
		time((java('java.util.Date')::new(I3), java(I3)::invoke(getYear))),
		writeq((java('java.util.Date')::new(I4), java(I4, Year3)::getYear)), nl,
		time((java('java.util.Date')::new(I4), java(I4, Year3)::getYear)),
		writeq((java('java.util.Date')::new(I5), java(I5)::getYear)), nl,
		time((java('java.util.Date')::new(I5), java(I5)::getYear)).

:- end_object.
