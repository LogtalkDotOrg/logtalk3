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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Unit tests for the "time_scales" library.'
	]).

	cover(time_scales).

	test(time_scales_valid_scale_1_01, true) :-
		time_scales::valid_scale(utc).

	test(time_scales_valid_scale_1_02, false) :-
		time_scales::valid_scale(utx).

	test(time_scales_valid_scale_1_03, true) :-
		time_scales::valid_scale(ut1).

	test(time_scales_valid_scale_1_04, true) :-
		time_scales::valid_scale(tdb).

	test(time_scales_valid_scale_1_05, true) :-
		time_scales::valid_scale(gps).

	test(time_scales_valid_scale_1_06, true) :-
		time_scales::valid_scale(gst).

	test(time_scales_valid_scale_1_07, true) :-
		time_scales::valid_scale(tcg).

	test(time_scales_valid_scale_1_08, true) :-
		time_scales::valid_scale(tcb).

	test(time_scales_valid_instant_1_01, true) :-
		time_scales::valid_instant(instant(utc, 1483228800, fraction(0, 1))).

	test(time_scales_valid_instant_1_02, false) :-
		time_scales::valid_instant(instant(utc, 1483228800, fraction(1, 1))).

	test(time_scales_valid_conversion_3_01, true) :-
		time_scales::valid_conversion(instant(tai, 1483228837, fraction(0, 1)), tai, tdb).

	test(time_scales_valid_conversion_3_02, false) :-
		time_scales::valid_conversion(instant(utc, 1483228800, fraction(0, 1)), tai, utc).

	test(time_scales_valid_instant_1_03, true) :-
		time_scales::valid_instant(instant(utc, 63072000, fraction(0, 1))).

	test(time_scales_valid_instant_1_04, false) :-
		time_scales::valid_instant(instant(utc, 63071999, fraction(0, 1))).

	test(time_scales_valid_instant_1_05, true) :-
		time_scales::valid_instant(instant(utc, 1483228800, fraction(2, 4))).

	test(time_scales_valid_instant_1_06, false) :-
		time_scales::valid_instant(instant(utc, 1483228800, fraction(-1, 2))).

	test(time_scales_supported_range_2_01, true(Start-End == date_time(1972, 1, 1, 0, 0, 0)-date_time(9999, 12, 31, 23, 59, 59))) :-
		time_scales::supported_range(Start, End).

	test(time_scales_utc_date_time_to_instant_2_01, true(Instant == instant(utc, 63072000, fraction(0, 1)))) :-
		time_scales::utc_date_time_to_instant(date_time(1972, 1, 1, 0, 0, 0), Instant).

	test(time_scales_utc_date_time_to_instant_2_02, fail) :-
		time_scales::utc_date_time_to_instant(date_time(1971, 12, 31, 23, 59, 59), _).

	test(time_scales_convert_4_01, true(TAI == instant(tai, 63072010, fraction(0, 1)))) :-
		time_scales::convert(instant(utc, 63072000, fraction(0, 1)), utc, tai, TAI).

	test(time_scales_convert_4_02, true(UTC == instant(utc, 63072000, fraction(0, 1)))) :-
		time_scales::convert(instant(tai, 63072010, fraction(0, 1)), tai, utc, UTC).

	test(time_scales_convert_4_03, true(TT == instant(tt, 63072042, fraction(23, 125)))) :-
		time_scales::convert(instant(utc, 63072000, fraction(0, 1)), utc, tt, TT).

	test(time_scales_convert_4_04, true(TAI == instant(tai, 63072010, fraction(0, 1)))) :-
		time_scales::convert(instant(tt, 63072042, fraction(23, 125)), tt, tai, TAI).

	test(time_scales_convert_4_05, true(TAI == instant(tai, 1483228835, fraction(3, 4)))) :-
		time_scales::convert(instant(utc, 1483228799, fraction(3, 4)), utc, tai, TAI).

	test(time_scales_offset_3_01, true(Offset == rational(37, 1))) :-
		time_scales::offset(instant(utc, 1483228800, fraction(0, 1)), tai, Offset).

	test(time_scales_offset_3_02, true(Offset == rational(4023, 125))) :-
		time_scales::offset(instant(tai, 63072010, fraction(0, 1)), tt, Offset).

	test(time_scales_instant_to_utc_date_time_2_01, true(UTCDateTime == date_time(2017, 1, 1, 0, 0, 0))) :-
		time_scales::instant_to_utc_date_time(instant(utc, 1483228800, fraction(0, 1)), UTCDateTime).

	test(time_scales_instant_to_utc_date_time_2_02, fail) :-
		time_scales::instant_to_utc_date_time(instant(utc, 1483228800, fraction(1, 10)), _).

	test(time_scales_leap_second_date_2_01, true) :-
		time_scales::leap_second_date(date_time(2017, 1, 1, 0, 0, 0), 37).

	test(time_scales_convert_4_06, true(UT1 == instant(ut1, 1483228800, fraction(0, 1)))) :-
		time_scales::convert(instant(utc, 1483228800, fraction(0, 1)), utc, ut1, UT1).

	test(time_scales_convert_4_07, true(Scale == tdb), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tt, 1483228869, fraction(0, 1)), tt, tdb, TDB),
		TDB = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_07b, true(UTC == instant(utc, 1483228800, fraction(1, 2)))) :-
		time_scales::convert(instant(utc, 1483228800, fraction(1, 2)), utc, utc, UTC).

	test(time_scales_convert_4_08, true(UTC == instant(utc, 1483228800, fraction(0, 1)))) :-
		time_scales::convert(instant(ut1, 1483228800, fraction(0, 1)), ut1, utc, UTC).

	test(time_scales_convert_4_09, true(TAI == instant(tai, 1483228837, fraction(0, 1)))) :-
		time_scales::convert(instant(ut1, 1483228800, fraction(0, 1)), ut1, tai, TAI).

	test(time_scales_convert_4_10, true(TT == instant(tt, 1483228869, fraction(23, 125)))) :-
		time_scales::convert(instant(ut1, 1483228800, fraction(0, 1)), ut1, tt, TT).

	test(time_scales_convert_4_11, true(UT1 == instant(ut1, 1483228800, fraction(0, 1)))) :-
		time_scales::convert(instant(tai, 1483228837, fraction(0, 1)), tai, ut1, UT1).

	test(time_scales_convert_4_12, true(UT1 == instant(ut1, 1483228800, fraction(0, 1)))) :-
		time_scales::convert(instant(tt, 1483228869, fraction(23, 125)), tt, ut1, UT1).

	test(time_scales_convert_4_13, true(Scale == tdb), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(utc, 1483228800, fraction(0, 1)), utc, tdb, TDB),
		TDB = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_14, true(Scale == utc), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tdb, 1483228869, fraction(23, 125)), tdb, utc, UTC),
		UTC = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_15, true(Scale == tdb), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tai, 1483228837, fraction(0, 1)), tai, tdb, TDB),
		TDB = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_16, true(Scale == tai), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tdb, 1483228869, fraction(23, 125)), tdb, tai, TAI),
		TAI = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_17, true(Scale == tdb), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(ut1, 1483228800, fraction(0, 1)), ut1, tdb, TDB),
		TDB = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_18, true(Scale == ut1), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tdb, 1483228869, fraction(23, 125)), tdb, ut1, UT1),
		UT1 = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_19, true(GPS == instant(gps, 1483228818, fraction(0, 1)))) :-
		time_scales::convert(instant(tai, 1483228837, fraction(0, 1)), tai, gps, GPS).

	test(time_scales_convert_4_20, true(TAI == instant(tai, 1483228837, fraction(0, 1)))) :-
		time_scales::convert(instant(gps, 1483228818, fraction(0, 1)), gps, tai, TAI).

	test(time_scales_convert_4_21, true(GPS == instant(gps, 1483228818, fraction(0, 1)))) :-
		time_scales::convert(instant(utc, 1483228800, fraction(0, 1)), utc, gps, GPS).

	test(time_scales_convert_4_22, true(UTC == instant(utc, 1483228800, fraction(0, 1)))) :-
		time_scales::convert(instant(gps, 1483228818, fraction(0, 1)), gps, utc, UTC).

	test(time_scales_convert_4_23, true(Scale == gps), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tdb, 1483228869, fraction(23, 125)), tdb, gps, GPS),
		GPS = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_24, true(GST == instant(gst, 1483228818, fraction(0, 1)))) :-
		time_scales::convert(instant(tai, 1483228837, fraction(0, 1)), tai, gst, GST).

	test(time_scales_convert_4_25, true(TAI == instant(tai, 1483228837, fraction(0, 1)))) :-
		time_scales::convert(instant(gst, 1483228818, fraction(0, 1)), gst, tai, TAI).

	test(time_scales_convert_4_26, true(GST == instant(gst, 1483228818, fraction(0, 1)))) :-
		time_scales::convert(instant(utc, 1483228800, fraction(0, 1)), utc, gst, GST).

	test(time_scales_convert_4_27, true(UTC == instant(utc, 1483228800, fraction(0, 1)))) :-
		time_scales::convert(instant(gst, 1483228818, fraction(0, 1)), gst, utc, UTC).

	test(time_scales_convert_4_28, true(GST == instant(gst, 1483228818, fraction(0, 1)))) :-
		time_scales::convert(instant(gps, 1483228818, fraction(0, 1)), gps, gst, GST).

	test(time_scales_convert_4_29, true(GPS == instant(gps, 1483228818, fraction(0, 1)))) :-
		time_scales::convert(instant(gst, 1483228818, fraction(0, 1)), gst, gps, GPS).

	test(time_scales_convert_4_30, true(Scale == tcg), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tt, 1483228869, fraction(23, 125)), tt, tcg, TCG),
		TCG = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_31, true(Scale == tt), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tcg, 1483228870, fraction(13, 200)), tcg, tt, TT),
		TT = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_32, true(Scale == tcg), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(utc, 1483228800, fraction(0, 1)), utc, tcg, TCG),
		TCG = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_33, true(Scale == utc), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tcg, 1483228870, fraction(13, 200)), tcg, utc, UTC),
		UTC = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_34, true(Scale == tcg), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(gps, 1483228818, fraction(0, 1)), gps, tcg, TCG),
		TCG = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_35, true(Scale == gps), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tcg, 1483228870, fraction(13, 200)), tcg, gps, GPS),
		GPS = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_36, true(Scale == tcb), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tdb, 1483228869, fraction(23, 125)), tdb, tcb, TCB),
		TCB = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_37, true(Scale == tdb), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tcb, 1483228890, fraction(7, 20)), tcb, tdb, TDB),
		TDB = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_38, true(Scale == tcb), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(utc, 1483228800, fraction(0, 1)), utc, tcb, TCB),
		TCB = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_39, true(Scale == utc), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tcb, 1483228890, fraction(7, 20)), tcb, utc, UTC),
		UTC = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_40, true(Scale == tcg), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tcb, 1483228890, fraction(7, 20)), tcb, tcg, TCG),
		TCG = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_41, true(Scale == tcb), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tcg, 1483228870, fraction(13, 200)), tcg, tcb, TCB),
		TCB = instant(Scale, _, fraction(_, _)).

	test(time_scales_convert_4_42, true(TCBSeconds > 1483228869), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tdb, 1483228869, fraction(0, 1)), tdb, tcb, instant(tcb, TCBSeconds, fraction(_, _))).

	test(time_scales_convert_4_43, true(TDBSeconds < 1483228890), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tcb, 1483228890, fraction(0, 1)), tcb, tdb, instant(tdb, TDBSeconds, fraction(_, _))).

	test(time_scales_convert_4_44, true(TCGSeconds >= 1483228869), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::convert(instant(tt, 1483228869, fraction(0, 1)), tt, tcg, instant(tcg, TCGSeconds, fraction(_, _))).

	test(time_scales_convert_4_45, true(TCG == instant(tcg, 220924832, fraction(23, 125)))) :-
		time_scales::convert(instant(tt, 220924832, fraction(23, 125)), tt, tcg, TCG).

	test(time_scales_convert_4_46, true(TCB == instant(tcb, 220924832, fraction(23, 125)))) :-
		time_scales::convert(instant(tdb, 220924832, fraction(23, 125)), tdb, tcb, TCB).

	test(time_scales_offset_3_03, true(Offset2100Scaled > Offset2017Scaled), [condition(current_prolog_flag(bounded, false))]) :-
		time_scales::offset(instant(tdb, 1483228869, fraction(0, 1)), tcb, rational(Offset2017Numerator, Offset2017Denominator)),
		time_scales::offset(instant(tdb, 4102444800, fraction(0, 1)), tcb, rational(Offset2100Numerator, Offset2100Denominator)),
		Offset2100Scaled is Offset2100Numerator * Offset2017Denominator,
		Offset2017Scaled is Offset2017Numerator * Offset2100Denominator.

	test(time_scales_roundtrip_3_01, true(DeltaNanos =< 5000)) :-
		round_trip_delta_nanos(instant(tdb, 220924832, fraction(23, 125)), tdb, tcb, DeltaNanos).

	test(time_scales_roundtrip_3_02, true(DeltaNanos =< 5000), [condition(current_prolog_flag(bounded, false))]) :-
		round_trip_delta_nanos(instant(tdb, 1483228869, fraction(0, 1)), tdb, tcb, DeltaNanos).

	test(time_scales_roundtrip_3_03, true(DeltaNanos =< 5000), [condition(current_prolog_flag(bounded, false))]) :-
		round_trip_delta_nanos(instant(tdb, 4102444800, fraction(0, 1)), tdb, tcb, DeltaNanos).

	test(time_scales_roundtrip_3_04, true(DeltaNanos =< 5000)) :-
		round_trip_delta_nanos(instant(tt, 220924832, fraction(23, 125)), tt, tcg, DeltaNanos).

	test(time_scales_roundtrip_3_05, true(DeltaNanos =< 5000), [condition(current_prolog_flag(bounded, false))]) :-
		round_trip_delta_nanos(instant(tt, 1483228869, fraction(0, 1)), tt, tcg, DeltaNanos).

	test(time_scales_roundtrip_3_06, true(DeltaNanos =< 5000), [condition(current_prolog_flag(bounded, false))]) :-
		round_trip_delta_nanos(instant(tt, 4102444800, fraction(0, 1)), tt, tcg, DeltaNanos).

	test(time_scales_override_leap_seconds_1_01, true(Source == override)) :-
		^^file_path('test_files/leap_seconds_override.pl', Path),
		time_scales::load_leap_seconds_override(Path),
		once(time_scales::leap_seconds_source(Source)),
		time_scales::clear_leap_seconds_override.

	test(time_scales_override_leap_seconds_1_02, true(TAI == instant(tai, 63072012, fraction(0, 1)))) :-
		^^file_path('test_files/leap_seconds_override.pl', Path),
		time_scales::load_leap_seconds_override(Path),
		time_scales::convert(instant(utc, 63072000, fraction(0, 1)), utc, tai, TAI),
		time_scales::clear_leap_seconds_override.

	test(time_scales_override_dut1_1_01, true(Source == override)) :-
		^^file_path('test_files/dut1_override.pl', Path),
		time_scales::load_dut1_override(Path),
		once(time_scales::dut1_source(Source)),
		time_scales::clear_dut1_override.

	test(time_scales_override_dut1_1_02, true(UT1 == instant(ut1, 1483228800, fraction(1, 4)))) :-
		^^file_path('test_files/dut1_override.pl', Path),
		time_scales::load_dut1_override(Path),
		time_scales::convert(instant(utc, 1483228800, fraction(0, 1)), utc, ut1, UT1),
		time_scales::clear_dut1_override.

	test(time_scales_override_reset_1_01, true(Source == bundled)) :-
		time_scales::clear_leap_seconds_override,
		time_scales::clear_dut1_override,
		once(time_scales::leap_seconds_source(Source)).

	test(time_scales_leap_seconds_entries_1_01, true(Entries \== [])) :-
		time_scales::clear_leap_seconds_override,
		time_scales::leap_seconds_entries(Entries).

	test(time_scales_leap_seconds_entries_1_02, true(Entries == [leap(63072000, 12), leap(78796800, 13), leap(94694400, 14)])) :-
		^^file_path('test_files/leap_seconds_override.pl', Path),
		time_scales::load_leap_seconds_override(Path),
		time_scales::leap_seconds_entries(Entries),
		time_scales::clear_leap_seconds_override.

	test(time_scales_dut1_entries_1_01, true(Entries == [dut1(63072000, 0, 1)])) :-
		time_scales::clear_dut1_override,
		time_scales::dut1_entries(Entries).

	test(time_scales_dut1_entries_1_02, true(Entries == [dut1(1483228800, 1, 4)])) :-
		^^file_path('test_files/dut1_override.pl', Path),
		time_scales::load_dut1_override(Path),
		time_scales::dut1_entries(Entries),
		time_scales::clear_dut1_override.

	test(time_scales_save_leap_seconds_entries_1_01, true(Source == override)) :-
		Snapshot = '/tmp/logtalk_time_scales_leap_snapshot.pl',
		time_scales::clear_leap_seconds_override,
		time_scales::save_leap_seconds_entries(Snapshot),
		time_scales::load_leap_seconds_override(Snapshot),
		time_scales::leap_seconds_source(Source),
		time_scales::clear_leap_seconds_override.

	test(time_scales_save_dut1_entries_1_01, true(Source == override)) :-
		Snapshot = '/tmp/logtalk_time_scales_dut1_snapshot.pl',
		time_scales::clear_dut1_override,
		time_scales::save_dut1_entries(Snapshot),
		time_scales::load_dut1_override(Snapshot),
		time_scales::dut1_source(Source),
		time_scales::clear_dut1_override.

	throws(time_scales_check_instant_1_01, error(instantiation_error, _)) :-
		time_scales::check_instant(_).

	throws(time_scales_check_instant_1_02, error(domain_error(utc_unix_seconds, 63071999), _)) :-
		time_scales::check_instant(instant(utc, 63071999, fraction(0, 1))).

	throws(time_scales_check_conversion_3_01, error(domain_error(scale_mismatch, instant_scale(utc, tai)), _)) :-
		time_scales::check_conversion(instant(utc, 1483228800, fraction(0, 1)), tai, utc).

	throws(time_scales_check_convert_4_01, error(domain_error(time_scale, utx), _)) :-
		time_scales::check_convert(instant(utc, 1483228800, fraction(0, 1)), utc, utx, _).

	throws(time_scales_check_offset_3_01, error(domain_error(normalized_fraction, fraction(1, 1)), _)) :-
		time_scales::check_offset(instant(utc, 1483228800, fraction(1, 1)), tai, _).

	throws(time_scales_load_leap_override_1_03, error(existence_error(source_sink, '/tmp/logtalk_time_scales_missing_leap.pl'), _)) :-
		time_scales::load_leap_seconds_override('/tmp/logtalk_time_scales_missing_leap.pl').

	throws(time_scales_load_dut1_override_1_03, error(existence_error(source_sink, '/tmp/logtalk_time_scales_missing_dut1.pl'), _)) :-
		time_scales::load_dut1_override('/tmp/logtalk_time_scales_missing_dut1.pl').

	throws(time_scales_load_leap_override_1_04, error(domain_error(leap_override_term, oops(63072000, 12)), _)) :-
		^^file_path('test_files/leap_seconds_malformed_override.pl', Path),
		time_scales::load_leap_seconds_override(Path).

	throws(time_scales_load_dut1_override_1_04, error(domain_error(dut1_override_term, oops(1483228800, 1, 4)), _)) :-
		^^file_path('test_files/dut1_malformed_override.pl', Path),
		time_scales::load_dut1_override(Path).

	% auxiliary predicates

	round_trip_delta_nanos(Instant, FromScale, ThroughScale, DeltaNanos) :-
		time_scales::convert(Instant, FromScale, ThroughScale, ThroughInstant),
		time_scales::convert(ThroughInstant, ThroughScale, FromScale, RoundTripInstant),
		instant_delta_nanos(Instant, RoundTripInstant, DeltaNanos).

	instant_delta_nanos(instant(Scale, Seconds1, fraction(Numerator1, Denominator1)), instant(Scale, Seconds2, fraction(Numerator2, Denominator2)), DeltaNanos) :-
		GreatestCommonDivisor is gcd(Denominator1, Denominator2),
		CommonDenominator is Denominator1 // GreatestCommonDivisor * Denominator2,
		ScaledNumerator1 is Numerator1 * (CommonDenominator // Denominator1),
		ScaledNumerator2 is Numerator2 * (CommonDenominator // Denominator2),
		DifferenceNumerator is abs((Seconds1 - Seconds2) * CommonDenominator + ScaledNumerator1 - ScaledNumerator2),
		DeltaNanos is (DifferenceNumerator * 1000000000 + CommonDenominator - 1) // CommonDenominator.

:- end_object.
