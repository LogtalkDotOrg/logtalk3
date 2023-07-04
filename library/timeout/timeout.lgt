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


:- object(timeout).

	:- info([
		version is 0:11:0,
		author is 'Paulo Moura',
		date is 2023-06-21,
		comment is 'Predicates for calling goal with a time limit.',
		remarks is [
			'Supported backend Prolog systems' - 'B-Prolog, ECLiPSe, LVM, SICStus Prolog, SWI-Prolog, Trealla Prolog, XSB, and YAP.'
		]
	]).

	:- public(call_with_timeout/2).
	:- meta_predicate(call_with_timeout(0, *)).
	:- mode(call_with_timeout(+callable, +positive_number), zero_or_one).
	:- info(call_with_timeout/2, [
		comment is 'Calls a goal deterministically with the given time limit (expressed in seconds). Note that the goal may fail or throw an error before exhausting the time limit.',
		argnames is ['Goal', 'Timeout'],
		exceptions is [
			'Goal does not complete in the allowed time' - timeout('Goal')
		]
	]).

	:- public(call_with_timeout/3).
	:- meta_predicate(call_with_timeout(0, *, *)).
	:- mode(call_with_timeout(+callable, +positive_number, --atom), one).
	:- info(call_with_timeout/3, [
		comment is 'Calls a goal deterministically with the given time limit (expressed in seconds) returning a reified result: ``true``, ``fail``, ``timeout``, or ``error(Error)``.',
		argnames is ['Goal', 'Timeout', 'Result']
	]).

	:- if(current_logtalk_flag(prolog_dialect, b)).

		call_with_timeout(Goal, Time) :-
			MilliSeconds is truncate(Time * 1000),
			time_out(Goal, MilliSeconds, Result),
			(	Result == time_out ->
				throw(timeout(Goal))
			;	true
			).

		call_with_timeout(Goal, Time, Result) :-
			MilliSeconds is truncate(Time * 1000),
			(	catch(time_out(Goal, MilliSeconds, Result0), Error, true) ->
				(	Result0 == time_out ->
					Result = timeout
				;	var(Error) ->
					Result = true
				;	Result = error(Error)
				)
			;	Result = fail
			).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		:- meta_predicate(timeout:timeout(0, *, *)).

		call_with_timeout(Goal, Time) :-
			timeout:timeout(Goal, Time, throw(timeout(Goal))),
			!.

		call_with_timeout(Goal, Time, Result) :-
			(	catch(timeout:timeout(Goal, Time, Result = timeout), Error, true) ->
				(	Result == timeout ->
					true
				;	var(Error) ->
					Result = true
				;	Result = error(Error)
				)
			;	Result = fail
			).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		call_with_timeout(Goal, Time) :-
			MilliSeconds is truncate(Time * 1000),
			timeout:time_out(Goal, MilliSeconds, Result),
			!,
			(	Result == time_out ->
				throw(timeout(Goal))
			;	true
			).

		call_with_timeout(Goal, Time, Result) :-
			MilliSeconds is truncate(Time * 1000),
			(	catch(timeout:time_out(Goal, MilliSeconds, Result0), Error, true) ->
				(	Result0 == time_out ->
					Result = timeout
				;	var(Error) ->
					Result = true
				;	Result = error(Error)
				)
			;	Result = fail
			).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		call_with_timeout(Goal, Time) :-
			catch(
				time:call_with_time_limit(Time, Goal),
				time_limit_exceeded,
				throw(timeout(Goal))
			).

		call_with_timeout(Goal, Time, Result) :-
			(	catch(time:call_with_time_limit(Time, Goal), Error, true) ->
				(	var(Error) ->
					Result = true
				;	Error == time_limit_exceeded ->
					Result = timeout
				;	Result = error(Error)
				)
			;	Result = fail
			).

	:- elif(current_logtalk_flag(prolog_dialect, trealla)).

		call_with_timeout(Goal, Time) :-
			catch(
				call_with_time_limit(Time, Goal),
				error(time_limit_exceeded(_, _), _),
				throw(timeout(Goal))
			).

		call_with_timeout(Goal, Time, Result) :-
			(	catch(call_with_time_limit(Time, Goal), Error, true) ->
				(	var(Error) ->
					Result = true
				;	Error = error(time_limit_exceeded(_, _), _) ->
					Result = timeout
				;	Result = error(Error)
				)
			;	Result = fail
			).

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		:- meta_predicate(timed_call(0, *)).

		call_with_timeout(Goal, Time) :-
			MilliSeconds is truncate(Time * 1000),
			timed_call(Goal, [max(MilliSeconds,throw(timeout(Goal)))]),
			!.

		call_with_timeout(Goal, Time, Result) :-
			MilliSeconds is truncate(Time * 1000),
			(	catch(timed_call(Goal, [max(MilliSeconds, throw(timeout))]), Error, true) ->
				(	Error == timeout ->
					Result = timeout
				;	var(Error) ->
					Result = true
				;	Result = error(Error)
				)
			;	Result = fail
			).

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		call_with_timeout(Goal, Time) :-
			MilliSeconds is truncate(Time * 1000),
			(	catch(timeout:time_out(Goal, MilliSeconds, _), Error, true) ->
				(	var(Error) ->
					true
				;	Error == timeout ->
					throw(timeout(Goal))
				;	throw(Error)
				)
			;	fail
			).

		call_with_timeout(Goal, Time, Result) :-
			MilliSeconds is truncate(Time * 1000),
			(	catch(timeout:time_out(Goal, MilliSeconds, _), Error, true) ->
				(	var(Error) ->
					Result = true
				;	Error == timeout ->
					Result = timeout
				;	Result = error(Error)
				)
			;	Result = fail
			).

	:- elif(current_logtalk_flag(prolog_dialect, arriba)).

		call_with_timeout(Goal, Time) :-
			call_with_timeout_aux(Goal, Goal, Time).

		:- meta_predicate(call_with_timeout_aux(0, *, *)).
		call_with_timeout_aux(TGoal, Goal, Time) :-
			catch(user::call_with_timeout(TGoal, Time), Error, true),
			(	var(Error) ->
				true
			;	Error = timeout(_) ->
				throw(timeout(Goal))
			;	throw(Error)
			).

		call_with_timeout(Goal, Time, Result) :-
			user::call_with_timeout(Goal, Time, Result).

	:- elif(current_logtalk_flag(prolog_dialect, lvm)).

		call_with_timeout(Goal, Time) :-
			call_with_timeout_aux(Goal, Goal, Time).

		:- meta_predicate(call_with_timeout_aux(0, *, *)).
		call_with_timeout_aux(TGoal, Goal, Time) :-
			catch(user::call_with_timeout(TGoal, Time), Error, true),
			(	var(Error) ->
				true
			;	Error = timeout(_) ->
				throw(timeout(Goal))
			;	throw(Error)
			).

		call_with_timeout(Goal, Time, Result) :-
			user::call_with_timeout(Goal, Time, Result).

	:- endif.

:- end_object.
