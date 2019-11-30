%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- object(timeout).

	:- info([
		version is 0.4,
		author is 'Paulo Moura',
		date is 2019/11/29,
		comment is 'Predicates for calling goal with a time limit.',
		remarks is [
			'Supported backend Prolog systems' - 'B-Prolog, ECLiPSe, SICStus Prolog, SWI-Prolog, XSB, and YAP.'
		]
	]).

	:- public(call_with_timeout/2).
	:- meta_predicate(call_with_timeout(0, *)).
	:- mode(call_with_timeout(+callable, +positive_number), zero_or_one).
	:- info(call_with_timeout/2, [
		comment is 'Calls a goal deterministically with the given time limit (expressed in seconds).',
		argnames is ['Goal', 'Timeout'],
		exceptions is [
			'Goal does not complete in the allowed time' - timeout('Goal')
		]
	]).

	:- if(current_logtalk_flag(prolog_dialect, b)).

		call_with_timeout(Goal, Time) :-
			context(Context),
			MilliSeconds is truncate(Time * 1000),
			time_out(Goal, MilliSeconds, Result),
			(	Result == time_out ->
				throw(error(timeout(Goal),Context))
			;	true
			).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		:- meta_predicate(timeout:timeout(0, *, *)).

		call_with_timeout(Goal, Time) :-
			context(Context),
			timeout:timeout(Goal, Time, throw(error(timeout(Goal),Context))),
			!.

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		call_with_timeout(Goal, Time) :-
			context(Context),
			MilliSeconds is truncate(Time * 1000),
			timeout:time_out(Goal, MilliSeconds, Result),
			!,
			(	Result == time_out ->
				throw(error(timeout(Goal),Context))
			;	true
			).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		call_with_timeout(Goal, Time) :-
			context(Context),
			catch(
				time:call_with_time_limit(Time, Goal),
				time_limit_exceeded,
				throw(error(timeout(Goal),Context))
			).

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		:- meta_predicate(timed_call(0, *)).

		call_with_timeout(Goal, Time) :-
			context(Context),
			MilliSeconds is truncate(Time * 1000),
			timed_call(Goal, [max(MilliSeconds,throw(error(timeout(Goal),Context)))]),
			!.

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		call_with_timeout(Goal, Time) :-
			context(Context),
			MilliSeconds is truncate(Time * 1000),
			timeout:time_out(Goal, MilliSeconds, Result),
			!,
			(	Result == time_out ->
				throw(error(timeout(Goal),Context))
			;	true
			).

	:- endif.

:- end_object.
