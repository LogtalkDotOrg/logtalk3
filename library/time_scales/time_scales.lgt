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


:- object(time_scales,
	implements(time_scales_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Time scale conversions for UTC, TAI, TT, UT1, TDB, GPS, GST, TCG, and TCB using bundled and optional override data.',
		remarks is [
			'Supported UTC range' - 'From 1972-01-01T00:00:00Z onwards.',
			'Time representation' - 'Instants are represented as ``instant(Scale,Seconds,fraction(Numerator,Denominator))`` with normalized fractional part in the ``[0,1[`` interval.'
		],
		see_also is [time_scales_protocol, date]
	]).

	valid_scale(utc).
	valid_scale(tai).
	valid_scale(tt).
	valid_scale(ut1).
	valid_scale(tdb).
	valid_scale(gps).
	valid_scale(gst).
	valid_scale(tcg).
	valid_scale(tcb).

	supported_range(date_time(1972, 1, 1, 0, 0, 0), date_time(9999, 12, 31, 23, 59, 59)).

	leap_second_date(DateTime, OffsetSeconds) :-
		time_scales_data::leap_effective_date(DateTime, OffsetSeconds).

	load_leap_seconds_override(File) :-
		time_scales_data::load_leap_seconds_override(File).

	clear_leap_seconds_override :-
		time_scales_data::clear_leap_seconds_override.

	leap_seconds_source(Source) :-
		time_scales_data::leap_seconds_source(Source).

	leap_seconds_entries(Entries) :-
		time_scales_data::leap_seconds_entries(Entries).

	save_leap_seconds_entries(File) :-
		time_scales_data::save_leap_seconds_entries(File).

	load_dut1_override(File) :-
		time_scales_data::load_dut1_override(File).

	clear_dut1_override :-
		time_scales_data::clear_dut1_override.

	dut1_source(Source) :-
		time_scales_data::dut1_source(Source).

	dut1_entries(Entries) :-
		time_scales_data::dut1_entries(Entries).

	save_dut1_entries(File) :-
		time_scales_data::save_dut1_entries(File).

	utc_date_time_to_instant(UTCDateTime, instant(utc, UnixSeconds, fraction(0, 1))) :-
		date::valid_date_time(UTCDateTime, strict),
		date::date_time_to_unix(UTCDateTime, UnixSeconds),
		UnixSeconds >= 63072000.

	instant_to_utc_date_time(instant(utc, UnixSeconds, fraction(0, 1)), UTCDateTime) :-
		integer(UnixSeconds),
		UnixSeconds >= 63072000,
		date::unix_to_date_time(UnixSeconds, UTCDateTime).

	valid_instant(instant(Scale, Seconds, fraction(Numerator, Denominator))) :-
		valid_scale(Scale),
		check_instant(instant(Scale, Seconds, fraction(Numerator, Denominator)), Scale, _, _, _).
	valid_instant(Instant) :-
		catch(check_instant(Instant), _, fail).

	check_instant(Instant) :-
		check_instant_data(Instant, _Scale, _Seconds, _Numerator, _Denominator).

	valid_conversion(Instant, FromScale, ToScale) :-
		valid_scale(FromScale),
		valid_scale(ToScale),
		check_instant(Instant, FromScale, _, _, _).
	valid_conversion(Instant, FromScale, ToScale) :-
		catch(check_conversion(Instant, FromScale, ToScale), _, fail).

	check_conversion(Instant, FromScale, ToScale) :-
		check_scale(FromScale),
		check_scale(ToScale),
		check_instant_data(Instant, Scale, _Seconds, _Numerator, _Denominator),
		(   Scale == FromScale ->
			true
		;   domain_error(scale_mismatch, instant_scale(Scale, FromScale))
		).

	convert(Instant, FromScale, ToScale, ConvertedInstant) :-
		valid_conversion(Instant, FromScale, ToScale),
		check_instant(Instant, FromScale, Seconds, Numerator, Denominator),
		convert_checked(Seconds, Numerator, Denominator, FromScale, ToScale, ConvertedScale, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator),
		ConvertedInstant = instant(ConvertedScale, ConvertedSeconds, fraction(ConvertedNumerator, ConvertedDenominator)).

	check_convert(Instant, FromScale, ToScale, ConvertedInstant) :-
		check_conversion(Instant, FromScale, ToScale),
		check_instant(Instant, FromScale, Seconds, Numerator, Denominator),
		convert_checked(Seconds, Numerator, Denominator, FromScale, ToScale, ConvertedScale, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator),
		ConvertedInstant = instant(ConvertedScale, ConvertedSeconds, fraction(ConvertedNumerator, ConvertedDenominator)).

	offset(Instant, ToScale, rational(Numerator, Denominator)) :-
		valid_scale(ToScale),
		Instant = instant(FromScale, _, _),
		valid_scale(FromScale),
		convert(Instant, FromScale, ToScale, instant(_, ConvertedSeconds, fraction(ConvertedNumerator, ConvertedDenominator))),
		check_instant(Instant, FromScale, Seconds, Numerator0, Denominator0),
		difference_as_rational(ConvertedSeconds, ConvertedNumerator, ConvertedDenominator, Seconds, Numerator0, Denominator0, Numerator, Denominator).

	check_offset(Instant, ToScale, rational(Numerator, Denominator)) :-
		check_scale(ToScale),
		check_instant_data(Instant, FromScale, Seconds, Numerator0, Denominator0),
		check_convert(Instant, FromScale, ToScale, instant(_, ConvertedSeconds, fraction(ConvertedNumerator, ConvertedDenominator))),
		difference_as_rational(ConvertedSeconds, ConvertedNumerator, ConvertedDenominator, Seconds, Numerator0, Denominator0, Numerator, Denominator).

	convert_checked(Seconds, Numerator, Denominator, Scale, Scale, Scale, Seconds, Numerator, Denominator) :-
		!.
	convert_checked(Seconds, Numerator, Denominator, utc, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::leap_offset_at_utc_unix(Seconds, OffsetSeconds),
		add_rational(Seconds, Numerator, Denominator, OffsetSeconds, 1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tai, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::tai_minus_utc_for_tai_unix(Seconds, OffsetSeconds),
		add_rational(Seconds, Numerator, Denominator, -OffsetSeconds, 1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tai, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::tt_minus_tai(TTMinusTAINumerator, TTMinusTAIDenominator),
		add_rational(Seconds, Numerator, Denominator, TTMinusTAINumerator, TTMinusTAIDenominator, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tt, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::tt_minus_tai(TTMinusTAINumerator, TTMinusTAIDenominator),
		add_rational(Seconds, Numerator, Denominator, -TTMinusTAINumerator, TTMinusTAIDenominator, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, utc, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, utc, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tt, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tt, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, utc, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::dut1_offset_at_utc_unix(Seconds, DUT1Numerator, DUT1Denominator),
		add_rational(Seconds, Numerator, Denominator, DUT1Numerator, DUT1Denominator, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, ut1, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		ut1_to_utc(Seconds, Numerator, Denominator, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tai, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tai, utc, utc, UTCSeconds, UTCNumerator, UTCDenominator),
		convert_checked(UTCSeconds, UTCNumerator, UTCDenominator, utc, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, ut1, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, ut1, utc, utc, UTCSeconds, UTCNumerator, UTCDenominator),
		convert_checked(UTCSeconds, UTCNumerator, UTCDenominator, utc, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tt, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tt, utc, utc, UTCSeconds, UTCNumerator, UTCDenominator),
		convert_checked(UTCSeconds, UTCNumerator, UTCDenominator, utc, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, ut1, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, ut1, utc, utc, UTCSeconds, UTCNumerator, UTCDenominator),
		convert_checked(UTCSeconds, UTCNumerator, UTCDenominator, utc, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tt, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::tdb_minus_tt_approx(Seconds, fraction(Numerator, Denominator), OffsetSeconds),
		real_offset_to_rational(OffsetSeconds, OffsetNumerator, OffsetDenominator),
		add_rational(Seconds, Numerator, Denominator, OffsetNumerator, OffsetDenominator, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tdb, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::tdb_minus_tt_approx(Seconds, fraction(Numerator, Denominator), OffsetSeconds),
		real_offset_to_rational(OffsetSeconds, OffsetNumerator, OffsetDenominator),
		add_rational(Seconds, Numerator, Denominator, -OffsetNumerator, OffsetDenominator, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, utc, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, utc, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tdb, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tdb, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tai, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tai, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tdb, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tdb, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, ut1, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, ut1, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tdb, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tdb, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gps, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		add_rational(Seconds, Numerator, Denominator, 19, 1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tai, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		add_rational(Seconds, Numerator, Denominator, -19, 1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gps, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gps, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, utc, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, utc, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gps, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gps, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tt, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tt, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gps, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gps, utc, utc, UTCSeconds, UTCNumerator, UTCDenominator),
		convert_checked(UTCSeconds, UTCNumerator, UTCDenominator, utc, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, ut1, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, ut1, utc, utc, UTCSeconds, UTCNumerator, UTCDenominator),
		convert_checked(UTCSeconds, UTCNumerator, UTCDenominator, utc, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gps, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gps, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tdb, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tdb, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gst, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		add_rational(Seconds, Numerator, Denominator, 19, 1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tai, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		add_rational(Seconds, Numerator, Denominator, -19, 1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gst, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gst, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, utc, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, utc, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gst, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gst, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tt, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tt, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gst, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gst, utc, utc, UTCSeconds, UTCNumerator, UTCDenominator),
		convert_checked(UTCSeconds, UTCNumerator, UTCDenominator, utc, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, ut1, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, ut1, utc, utc, UTCSeconds, UTCNumerator, UTCDenominator),
		convert_checked(UTCSeconds, UTCNumerator, UTCDenominator, utc, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gst, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gst, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tdb, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tdb, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gps, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gps, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gst, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gst, tai, tai, TAISeconds, TAINumerator, TAIDenominator),
		convert_checked(TAISeconds, TAINumerator, TAIDenominator, tai, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tt, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::tcg_minus_tt_approx(Seconds, fraction(Numerator, Denominator), OffsetSeconds),
		real_offset_to_rational(OffsetSeconds, OffsetNumerator, OffsetDenominator),
		add_rational(Seconds, Numerator, Denominator, OffsetNumerator, OffsetDenominator, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcg, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::tcg_minus_tt_approx(Seconds, fraction(Numerator, Denominator), OffsetSeconds),
		real_offset_to_rational(OffsetSeconds, OffsetNumerator, OffsetDenominator),
		add_rational(Seconds, Numerator, Denominator, -OffsetNumerator, OffsetDenominator, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, utc, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, utc, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcg, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcg, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tai, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tai, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcg, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcg, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, ut1, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, ut1, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcg, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcg, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tdb, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tdb, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcg, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcg, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gps, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gps, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcg, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcg, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gst, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gst, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcg, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcg, tt, tt, TTSeconds, TTNumerator, TTDenominator),
		convert_checked(TTSeconds, TTNumerator, TTDenominator, tt, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tdb, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::tcb_minus_tdb_approx(Seconds, fraction(Numerator, Denominator), OffsetSeconds),
		real_offset_to_rational(OffsetSeconds, OffsetNumerator, OffsetDenominator),
		add_rational(Seconds, Numerator, Denominator, OffsetNumerator, OffsetDenominator, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcb, tdb, tdb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		time_scales_data::tcb_minus_tdb_approx(Seconds, fraction(Numerator, Denominator), OffsetSeconds),
		real_offset_to_rational(OffsetSeconds, OffsetNumerator, OffsetDenominator),
		add_rational(Seconds, Numerator, Denominator, -OffsetNumerator, OffsetDenominator, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, utc, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, utc, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcb, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcb, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, utc, utc, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tai, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tai, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcb, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcb, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, tai, tai, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tt, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tt, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcb, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcb, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, tt, tt, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, ut1, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, ut1, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcb, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcb, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, ut1, ut1, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gps, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gps, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcb, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcb, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, gps, gps, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, gst, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, gst, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcb, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcb, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, gst, gst, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcg, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcg, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, tcb, tcb, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).
	convert_checked(Seconds, Numerator, Denominator, tcb, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator) :-
		convert_checked(Seconds, Numerator, Denominator, tcb, tdb, tdb, TDBSeconds, TDBNumerator, TDBDenominator),
		convert_checked(TDBSeconds, TDBNumerator, TDBDenominator, tdb, tcg, tcg, ConvertedSeconds, ConvertedNumerator, ConvertedDenominator).

	ut1_to_utc(Seconds, Numerator, Denominator, UTCSeconds, UTCNumerator, UTCDenominator) :-
		ut1_to_utc(3, Seconds, Numerator, Denominator, UTCSeconds, UTCNumerator, UTCDenominator).

	ut1_to_utc(0, Seconds, Numerator, Denominator, UTCSeconds, UTCNumerator, UTCDenominator) :-
		time_scales_data::dut1_offset_at_utc_unix(Seconds, DUT1Numerator, DUT1Denominator),
		add_rational(Seconds, Numerator, Denominator, -DUT1Numerator, DUT1Denominator, UTCSeconds, UTCNumerator, UTCDenominator).
	ut1_to_utc(Iterations, Seconds, Numerator, Denominator, UTCSeconds, UTCNumerator, UTCDenominator) :-
		Iterations > 0,
		time_scales_data::dut1_offset_at_utc_unix(Seconds, DUT1Numerator, DUT1Denominator),
		add_rational(Seconds, Numerator, Denominator, -DUT1Numerator, DUT1Denominator, CandidateSeconds, CandidateNumerator, CandidateDenominator),
		NextIterations is Iterations - 1,
		ut1_to_utc(NextIterations, CandidateSeconds, CandidateNumerator, CandidateDenominator, UTCSeconds, UTCNumerator, UTCDenominator).

	real_offset_to_rational(OffsetSeconds, Numerator, 1000000000) :-
		Numerator is round(OffsetSeconds * 1000000000.0).

	check_scale(Scale) :-
		(   var(Scale) ->
			instantiation_error
		;   \+ atom(Scale) ->
			type_error(atom, Scale)
		;   valid_scale(Scale) ->
			true
		;   domain_error(time_scale, Scale)
		).

	check_instant_data(Instant, Scale, Seconds, NormalizedNumerator, NormalizedDenominator) :-
		(   var(Instant) ->
			instantiation_error
		;   Instant = instant(Scale0, Seconds0, Fraction) ->
			true
		;   domain_error(instant, Instant)
		),
		check_scale(Scale0),
		check_utc_unix_seconds(Seconds0, Seconds),
		check_fraction(Fraction, NormalizedNumerator, NormalizedDenominator),
		Scale = Scale0.

	check_utc_unix_seconds(Seconds0, Seconds) :-
		(   var(Seconds0) ->
			instantiation_error
		;   \+ integer(Seconds0) ->
			type_error(integer, Seconds0)
		;   Seconds0 < 63072000 ->
			domain_error(utc_unix_seconds, Seconds0)
		;   Seconds = Seconds0
		).

	check_fraction(Fraction, NormalizedNumerator, NormalizedDenominator) :-
		(   var(Fraction) ->
			instantiation_error
		;   Fraction = fraction(Numerator, Denominator) ->
			true
		;   domain_error(fraction, Fraction)
		),
		check_integer_value(Numerator),
		check_integer_value(Denominator),
		(   Denominator =< 0 ->
			domain_error(positive_denominator, Denominator)
		;   true
		),
		normalize_rational(Numerator, Denominator, NormalizedNumerator0, NormalizedDenominator0),
		(   (NormalizedNumerator0 < 0; NormalizedNumerator0 >= NormalizedDenominator0) ->
			domain_error(normalized_fraction, Fraction)
		;   NormalizedNumerator = NormalizedNumerator0,
			NormalizedDenominator = NormalizedDenominator0
		).

	check_integer_value(Value) :-
		(   var(Value) ->
			instantiation_error
		;   \+ integer(Value) ->
			type_error(integer, Value)
		;   true
		).

	check_instant(instant(Scale, Seconds, fraction(Numerator, Denominator)), Scale, Seconds, NormalizedNumerator, NormalizedDenominator) :-
		integer(Seconds),
		integer(Numerator),
		integer(Denominator),
		Denominator > 0,
		normalize_rational(Numerator, Denominator, NormalizedNumerator0, NormalizedDenominator0),
		NormalizedNumerator0 >= 0,
		NormalizedNumerator0 < NormalizedDenominator0,
		Seconds >= 63072000,
		NormalizedNumerator = NormalizedNumerator0,
		NormalizedDenominator = NormalizedDenominator0.

	difference_as_rational(Seconds1, Numerator1, Denominator1, Seconds2, Numerator2, Denominator2, Numerator, Denominator) :-
		Divisor is gcd(Denominator1, Denominator2),
		CommonDenominator is Denominator1 // Divisor * Denominator2,
		ScaledNumerator1 is Numerator1 * (CommonDenominator // Denominator1),
		ScaledNumerator2 is Numerator2 * (CommonDenominator // Denominator2),
		TotalNumerator is (Seconds1 - Seconds2) * CommonDenominator + ScaledNumerator1 - ScaledNumerator2,
		normalize_rational(TotalNumerator, CommonDenominator, Numerator, Denominator).

	add_rational(Seconds, Numerator, Denominator, OffsetNumerator, OffsetDenominator, ResultSeconds, ResultNumerator, ResultDenominator) :-
		Divisor is gcd(Denominator, OffsetDenominator),
		CommonDenominator is Denominator // Divisor * OffsetDenominator,
		ScaledNumerator is Numerator * (CommonDenominator // Denominator),
		ScaledOffsetNumerator is OffsetNumerator * (CommonDenominator // OffsetDenominator),
		TotalNumerator is Seconds * CommonDenominator + ScaledNumerator + ScaledOffsetNumerator,
		floor_division(TotalNumerator, CommonDenominator, ResultSeconds, ResidualNumerator),
		normalize_rational(ResidualNumerator, CommonDenominator, ResultNumerator, ResultDenominator).

	floor_division(Numerator, Denominator, Quotient, Remainder) :-
		(   Numerator >= 0 ->
			Quotient is Numerator // Denominator
		;   AbsNumerator is -Numerator,
			Quotient is -((AbsNumerator + Denominator - 1) // Denominator)
		),
		Remainder is Numerator - Quotient * Denominator.

	normalize_rational(Numerator, Denominator, NormalizedNumerator, NormalizedDenominator) :-
		(   Numerator =:= 0 ->
			NormalizedNumerator = 0,
			NormalizedDenominator = 1
		;   AbsoluteNumerator is abs(Numerator),
			Divisor is gcd(AbsoluteNumerator, Denominator),
			ReducedNumerator is Numerator // Divisor,
			ReducedDenominator is Denominator // Divisor,
			(   ReducedDenominator < 0 ->
				NormalizedNumerator is -ReducedNumerator,
				NormalizedDenominator is -ReducedDenominator
			;   NormalizedNumerator = ReducedNumerator,
				NormalizedDenominator = ReducedDenominator
			)
		).

:- end_object.
