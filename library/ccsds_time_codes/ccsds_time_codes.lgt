%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(ccsds_time_codes).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'Helpers for introspecting CCSDS time code terms.'
	]).

	:- public(valid/1).
	:- mode(valid(@compound), zero_or_one).
	:- info(valid/1, [
		comment is 'True if the argument is a known CCSDS time code term.',
		argnames is ['TimeCode']
	]).

	:- public(format/2).
	:- mode(format(+compound, -atom), one).
	:- info(format/2, [
		comment is 'Returns the CCSDS time code format for a known time code term.',
		argnames is ['TimeCode', 'Format']
	]).

	valid(cuc_time(Coarse, Fine)) :-
		integer(Coarse), Coarse >= 0,
		integer(Fine), Fine >= 0.
	valid(cds_time(Days, Milliseconds)) :-
		integer(Days), Days >= 0,
		integer(Milliseconds), Milliseconds >= 0.
	valid(cds_time(Days, Milliseconds, Submilliseconds)) :-
		integer(Days), Days >= 0,
		integer(Milliseconds), Milliseconds >= 0,
		integer(Submilliseconds), Submilliseconds >= 0.
	valid(ccs_calendar_time(Year, Month, Day, Hour, Minute, Second, Fraction)) :-
		integer(Year), Year >= 0,
		integer(Month), Month >= 0,
		integer(Day), Day >= 0,
		integer(Hour), Hour >= 0,
		integer(Minute), Minute >= 0,
		integer(Second), Second >= 0,
		integer(Fraction), Fraction >= 0.
	valid(ccs_ordinal_time(Year, DayOfYear, Hour, Minute, Second, Fraction)) :-
		integer(Year), Year >= 0,
		integer(DayOfYear), DayOfYear >= 0,
		integer(Hour), Hour >= 0,
		integer(Minute), Minute >= 0,
		integer(Second), Second >= 0,
		integer(Fraction), Fraction >= 0.

	format(TimeCode, Format) :-
		(	TimeCode = cuc_time(_, _) ->
			Format = cuc
		;	TimeCode = cds_time(_, _) ->
			Format = cds
		;	TimeCode = cds_time(_, _, _) ->
			Format = cds
		; TimeCode = ccs_calendar_time(_, _, _, _, _, _, _) ->
			Format = ccs
		; TimeCode = ccs_ordinal_time(_, _, _, _, _, _) ->
			Format = ccs
		;	var(TimeCode) ->
			instantiation_error
		;	domain_error(ccsds_time_code_term, TimeCode)
		).

:- end_object.
