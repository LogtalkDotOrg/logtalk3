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


:- category(ccsds_time_codes_types).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'Type definitions and arbitrary generators for CCSDS time codes.'
	]).

	:- multifile(type::type/1).
	type::type(ccsds_cuc(_, _, _)).
	type::type(ccsds_cuc_time(_, _)).
	type::type(ccsds_cds(_, _, _)).
	type::type(ccsds_cds_time(_, _)).
	type::type(ccsds_ccs(_, _)).
	type::type(ccsds_ccs_time(_, _)).

	:- multifile(type::check/2).
	type::check(ccsds_cuc(CoarseOctets, FineOctets, Epoch), Term) :-
		(	valid_ccsds_cuc_bytes(Term, CoarseOctets, FineOctets, Epoch) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(ccsds_cuc(CoarseOctets, FineOctets, Epoch), Term))
		).

	type::check(ccsds_cuc_time(CoarseOctets, FineOctets), Term) :-
		(	ccsds_cuc(CoarseOctets, FineOctets, ccsds_epoch)::valid(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(ccsds_cuc_time(CoarseOctets, FineOctets), Term))
		).

	type::check(ccsds_cds(DaySegmentOctets, SubmillisecondOctets, Epoch), Term) :-
		(	valid_ccsds_cds_bytes(Term, DaySegmentOctets, SubmillisecondOctets, Epoch) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(ccsds_cds(DaySegmentOctets, SubmillisecondOctets, Epoch), Term))
		).

	type::check(ccsds_cds_time(DaySegmentOctets, SubmillisecondOctets), Term) :-
		(	ccsds_cds(DaySegmentOctets, SubmillisecondOctets, ccsds_epoch)::valid(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(ccsds_cds_time(DaySegmentOctets, SubmillisecondOctets), Term))
		).

	type::check(ccsds_ccs(CalendarVariant, FractionOctets), Term) :-
		(	valid_ccsds_ccs_bytes(Term, CalendarVariant, FractionOctets) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(ccsds_ccs(CalendarVariant, FractionOctets), Term))
		).

	type::check(ccsds_ccs_time(CalendarVariant, FractionOctets), Term) :-
		(	ccsds_ccs(CalendarVariant, FractionOctets)::valid(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(ccsds_ccs_time(CalendarVariant, FractionOctets), Term))
		).

	valid_ccsds_cuc_bytes(Bytes, CoarseOctets, FineOctets, Epoch) :-
		type::valid(list(byte), Bytes),
		ccsds_cuc(CoarseOctets, FineOctets, Epoch)::parse(bytes(Bytes), _).

	valid_ccsds_cds_bytes(Bytes, DaySegmentOctets, SubmillisecondOctets, Epoch) :-
		type::valid(list(byte), Bytes),
		ccsds_cds(DaySegmentOctets, SubmillisecondOctets, Epoch)::parse(bytes(Bytes), _).

	valid_ccsds_ccs_bytes(Bytes, CalendarVariant, FractionOctets) :-
		type::valid(list(byte), Bytes),
		ccsds_ccs(CalendarVariant, FractionOctets)::parse(bytes(Bytes), _).

	:- multifile(arbitrary::arbitrary/1).
	arbitrary::arbitrary(ccsds_cuc(_, _, _)).
	arbitrary::arbitrary(ccsds_cuc_time(_, _)).
	arbitrary::arbitrary(ccsds_cds(_, _, _)).
	arbitrary::arbitrary(ccsds_cds_time(_, _)).
	arbitrary::arbitrary(ccsds_ccs(_, _)).
	arbitrary::arbitrary(ccsds_ccs_time(_, _)).

	:- multifile(arbitrary::arbitrary/2).
	arbitrary::arbitrary(ccsds_cuc(CoarseOctets, FineOctets, Epoch), Bytes) :-
		generate_ccsds_cuc_bytes(CoarseOctets, FineOctets, Epoch, Bytes).

	arbitrary::arbitrary(ccsds_cuc_time(CoarseOctets, FineOctets), TimeCode) :-
		generate_ccsds_cuc_time(CoarseOctets, FineOctets, TimeCode).

	arbitrary::arbitrary(ccsds_cds(DaySegmentOctets, SubmillisecondOctets, Epoch), Bytes) :-
		generate_ccsds_cds_bytes(DaySegmentOctets, SubmillisecondOctets, Epoch, Bytes).

	arbitrary::arbitrary(ccsds_cds_time(DaySegmentOctets, SubmillisecondOctets), TimeCode) :-
		generate_ccsds_cds_time(DaySegmentOctets, SubmillisecondOctets, TimeCode).

	arbitrary::arbitrary(ccsds_ccs(CalendarVariant, FractionOctets), Bytes) :-
		generate_ccsds_ccs_bytes(CalendarVariant, FractionOctets, Bytes).

	arbitrary::arbitrary(ccsds_ccs_time(CalendarVariant, FractionOctets), TimeCode) :-
		generate_ccsds_ccs_time(CalendarVariant, FractionOctets, TimeCode).

	generate_ccsds_cuc_bytes(CoarseOctets, FineOctets, Epoch, Bytes) :-
		generate_ccsds_cuc_time(CoarseOctets, FineOctets, TimeCode),
		ccsds_cuc(CoarseOctets, FineOctets, Epoch)::generate(bytes(Bytes), TimeCode).

	generate_ccsds_cuc_time(CoarseOctets, FineOctets, cuc_time(Coarse, Fine)) :-
		max_integer(CoarseOctets, MaxCoarse),
		max_integer(FineOctets, MaxFine),
		type::arbitrary(between(integer, 0, MaxCoarse), Coarse),
		type::arbitrary(between(integer, 0, MaxFine), Fine).

	generate_ccsds_cds_bytes(DaySegmentOctets, SubmillisecondOctets, Epoch, Bytes) :-
		generate_ccsds_cds_time(DaySegmentOctets, SubmillisecondOctets, TimeCode),
		ccsds_cds(DaySegmentOctets, SubmillisecondOctets, Epoch)::generate(bytes(Bytes), TimeCode).

	generate_ccsds_cds_time(DaySegmentOctets, 0, cds_time(Days, Milliseconds)) :-
		max_integer(DaySegmentOctets, MaxDays),
		type::arbitrary(between(integer, 0, MaxDays), Days),
		type::arbitrary(between(integer, 0, 86399999), Milliseconds).
	generate_ccsds_cds_time(DaySegmentOctets, 2, cds_time(Days, Milliseconds, Submilliseconds)) :-
		max_integer(DaySegmentOctets, MaxDays),
		type::arbitrary(between(integer, 0, MaxDays), Days),
		type::arbitrary(between(integer, 0, 86399999), Milliseconds),
		type::arbitrary(between(integer, 0, 999), Submilliseconds).
	generate_ccsds_cds_time(DaySegmentOctets, 4, cds_time(Days, Milliseconds, Submilliseconds)) :-
		max_integer(DaySegmentOctets, MaxDays),
		type::arbitrary(between(integer, 0, MaxDays), Days),
		type::arbitrary(between(integer, 0, 86399999), Milliseconds),
		type::arbitrary(between(integer, 0, 999999999), Submilliseconds).

	generate_ccsds_ccs_bytes(CalendarVariant, FractionOctets, Bytes) :-
		generate_ccsds_ccs_time(CalendarVariant, FractionOctets, TimeCode),
		ccsds_ccs(CalendarVariant, FractionOctets)::generate(bytes(Bytes), TimeCode).

	generate_ccsds_ccs_time(calendar, FractionOctets, ccs_calendar_time(Year, Month, Day, Hour, Minute, Second, Fraction)) :-
		max_fraction(FractionOctets, MaxFraction),
		type::arbitrary(between(integer, 0, 9999), Year),
		type::arbitrary(between(integer, 1, 12), Month),
		days_in_month(Year, Month, MaxDay),
		type::arbitrary(between(integer, 1, MaxDay), Day),
		type::arbitrary(between(integer, 0, 23), Hour),
		type::arbitrary(between(integer, 0, 59), Minute),
		type::arbitrary(between(integer, 0, 59), Second),
		type::arbitrary(between(integer, 0, MaxFraction), Fraction).
	generate_ccsds_ccs_time(day_of_year, FractionOctets, ccs_ordinal_time(Year, DayOfYear, Hour, Minute, Second, Fraction)) :-
		max_fraction(FractionOctets, MaxFraction),
		type::arbitrary(between(integer, 0, 9999), Year),
		max_day_of_year(Year, MaxDayOfYear),
		type::arbitrary(between(integer, 1, MaxDayOfYear), DayOfYear),
		type::arbitrary(between(integer, 0, 23), Hour),
		type::arbitrary(between(integer, 0, 59), Minute),
		type::arbitrary(between(integer, 0, 59), Second),
		type::arbitrary(between(integer, 0, MaxFraction), Fraction).

	max_integer(0, 0) :-
		!.
	max_integer(Octets, Max) :-
		Max is (1 << (8 * Octets)) - 1.

	max_fraction(FractionOctets, MaxFraction) :-
		Digits is FractionOctets * 2,
		power10(Digits, Scale),
		MaxFraction is Scale - 1.

	power10(0, 1) :-
		!.
	power10(Exponent, Value) :-
		Exponent > 0,
		Exponent1 is Exponent - 1,
		power10(Exponent1, Partial),
		Value is Partial * 10.

	max_day_of_year(Year, 366) :-
		leap_year(Year),
		!.
	max_day_of_year(_, 365).

	days_in_month(Year, 2, 29) :-
		leap_year(Year),
		!.
	days_in_month(_, 2, 28) :-
		!.
	days_in_month(_, 1, 31).
	days_in_month(_, 3, 31).
	days_in_month(_, 4, 30).
	days_in_month(_, 5, 31).
	days_in_month(_, 6, 30).
	days_in_month(_, 7, 31).
	days_in_month(_, 8, 31).
	days_in_month(_, 9, 30).
	days_in_month(_, 10, 31).
	days_in_month(_, 11, 30).
	days_in_month(_, 12, 31).

	leap_year(Year) :-
		0 =:= Year mod 400,
		!.
	leap_year(Year) :-
		0 =:= Year mod 4,
		Year mod 100 =\= 0.

:- end_category.
