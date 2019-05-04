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
		version is 0.1,
		author is 'Paulo Moura',
		date is 2019/05/03,
		comment is 'Call goal with a time limit predicates.',
		remarks is [
			'Supported backend Prolog systems' - 'ECLiPSe, SICStus Prolog, SWI-Prolog, and YAP.'
		]
	]).

	:- public(call_with_time_limit/2).
	:- meta_predicate(call_with_time_limit(*, 0)).
	:- mode(call_with_time_limit(+integer, +callable), zero_or_one).
	:- info(call_with_time_limit/2, [
		comment is 'Calls a goal with the given time limit (expressed in seconds).',
		argnames is ['Time', 'Goal'],
		exceptions is [
			'Goal does not complete in the allowed time' - timeout('Goal')
		]
	]).

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).

		call_with_time_limit(Time, Goal) :-
			timeout:timeout(once(Goal), Time, throw(timeout(Goal))).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		call_with_time_limit(Time, Goal) :-
			Seconds is Time * 1000,
			timeout:time_out(once(Goal), Seconds, Result),
			(	Result == time_out ->
				throw(timeout(Goal))
			;	true
			).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		call_with_time_limit(Time, Goal) :-
			catch(
				time:call_with_time_limit(Time, Goal),
				time_limit_exceeded,
				throw(timeout(Goal))
			).

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		call_with_time_limit(Time, Goal) :-
			Seconds is Time * 1000,
			timeout:time_out(once(Goal), Seconds, Result),
			(	Result == time_out ->
				throw(timeout(Goal))
			;	true
			).

	:- endif.

:- end_object.
