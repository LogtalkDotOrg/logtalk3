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


:- protocol(time_scales_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
		comment is 'Time scales conversion protocol for UTC, TAI, TT, UT1, TDB, GPS, GST, TCG, and TCB.',
		see_also is [time_scales, date]
	]).

	:- public(valid_scale/1).
	:- mode(valid_scale(@atom), zero_or_one).
	:- info(valid_scale/1, [
		comment is 'True when the argument is a supported time scale (``utc``, ``tai``, ``tt``, ``ut1``, ``tdb``, ``gps``, ``gst``, ``tcg``, or ``tcb``).',
		argnames is ['Scale']
	]).

	:- public(supported_range/2).
	:- mode(supported_range(-compound, -compound), one).
	:- info(supported_range/2, [
		comment is 'Returns the supported UTC datetime range as ``date_time(Year,Month,Day,Hour,Minute,Second)`` terms.',
		argnames is ['StartUTC', 'EndUTC']
	]).

	:- public(leap_second_date/2).
	:- mode(leap_second_date(?compound, ?integer), zero_or_more).
	:- info(leap_second_date/2, [
		comment is 'Enumerates UTC effective dates for ``TAI-UTC`` step changes and their resulting offset in SI seconds.',
		argnames is ['UTCDateTime', 'OffsetSeconds']
	]).

	:- public(load_leap_seconds_override/1).
	:- mode(load_leap_seconds_override(+atom), one).
	:- info(load_leap_seconds_override/1, [
		comment is 'Loads leap-second override data from a user-provided file containing ``leap(UnixSeconds,OffsetSeconds).`` terms.',
		argnames is ['File']
	]).

	:- public(clear_leap_seconds_override/0).
	:- mode(clear_leap_seconds_override, one).
	:- info(clear_leap_seconds_override/0, [
		comment is 'Clears any previously loaded leap-second override data and reverts to bundled data.'
	]).

	:- public(leap_seconds_source/1).
	:- mode(leap_seconds_source(-atom), one).
	:- info(leap_seconds_source/1, [
		comment is 'Returns the active leap-seconds data source as either ``bundled`` or ``override``.',
		argnames is ['Source']
	]).

	:- public(leap_seconds_entries/1).
	:- mode(leap_seconds_entries(-list), one).
	:- info(leap_seconds_entries/1, [
		comment is 'Returns the active leap-seconds table as an ordered list of ``leap(UnixSeconds,OffsetSeconds)`` terms.',
		argnames is ['Entries']
	]).

	:- public(save_leap_seconds_entries/1).
	:- mode(save_leap_seconds_entries(+atom), one).
	:- info(save_leap_seconds_entries/1, [
		comment is 'Saves the active leap-seconds table to a file as ``leap(UnixSeconds,OffsetSeconds).`` terms.',
		argnames is ['File']
	]).

	:- public(load_dut1_override/1).
	:- mode(load_dut1_override(+atom), one).
	:- info(load_dut1_override/1, [
		comment is 'Loads DUT1 override data from a user-provided file containing ``dut1(UnixSeconds,Numerator,Denominator).`` terms.',
		argnames is ['File']
	]).

	:- public(clear_dut1_override/0).
	:- mode(clear_dut1_override, one).
	:- info(clear_dut1_override/0, [
		comment is 'Clears any previously loaded DUT1 override data and reverts to bundled data.'
	]).

	:- public(dut1_source/1).
	:- mode(dut1_source(-atom), one).
	:- info(dut1_source/1, [
		comment is 'Returns the active DUT1 data source as either ``bundled`` or ``override``.',
		argnames is ['Source']
	]).

	:- public(dut1_entries/1).
	:- mode(dut1_entries(-list), one).
	:- info(dut1_entries/1, [
		comment is 'Returns the active DUT1 table as an ordered list of ``dut1(UnixSeconds,Numerator,Denominator)`` terms.',
		argnames is ['Entries']
	]).

	:- public(save_dut1_entries/1).
	:- mode(save_dut1_entries(+atom), one).
	:- info(save_dut1_entries/1, [
		comment is 'Saves the active DUT1 table to a file as ``dut1(UnixSeconds,Numerator,Denominator).`` terms.',
		argnames is ['File']
	]).

	:- public(utc_date_time_to_instant/2).
	:- mode(utc_date_time_to_instant(+compound, -compound), zero_or_one).
	:- info(utc_date_time_to_instant/2, [
		comment is 'Converts a UTC datetime term to an instant represented as ``instant(utc,Seconds,fraction(Numerator,Denominator))`` where ``Seconds`` are Unix epoch seconds and the fractional component is normalized.',
		argnames is ['UTCDateTime', 'Instant']
	]).

	:- public(valid_instant/1).
	:- mode(valid_instant(+compound), zero_or_one).
	:- info(valid_instant/1, [
		comment is 'True when the argument is a valid supported instant term ``instant(Scale,Seconds,fraction(Numerator,Denominator))``.',
		argnames is ['Instant']
	]).

	:- public(check_instant/1).
	:- mode(check_instant(+compound), one_or_error).
	:- info(check_instant/1, [
		comment is 'Checks an instant term and throws an error instead of failing on invalid input.',
		argnames is ['Instant'],
		exceptions is [
			'``Instant`` is a variable' - instantiation_error,
			'``Instant`` is not a valid instant term' - domain_error(instant, 'Instant'),
			'``Scale`` in ``Instant`` is neither a variable nor an atom' - type_error(atom, 'Scale'),
			'``Scale`` in ``Instant`` is not supported' - domain_error(time_scale, 'Scale'),
			'``Seconds`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Seconds'),
			'``Seconds`` in ``Instant`` is before supported UTC epoch' - domain_error(utc_unix_seconds, 'Seconds'),
			'Fraction term in ``Instant`` is not of the form ``fraction(Numerator,Denominator)``' - domain_error(fraction, 'Fraction'),
			'``Numerator`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Numerator'),
			'``Denominator`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Denominator'),
			'``Denominator`` in ``Instant`` is not positive' - domain_error(positive_denominator, 'Denominator'),
			'Normalized fraction in ``Instant`` is not in the ``[0,1[`` interval' - domain_error(normalized_fraction, 'Fraction')
		]
	]).

	:- public(valid_conversion/3).
	:- mode(valid_conversion(+compound, +atom, +atom), zero_or_one).
	:- info(valid_conversion/3, [
		comment is 'True when an instant is valid for ``FromScale`` and conversion to ``ToScale`` is admissible.',
		argnames is ['Instant', 'FromScale', 'ToScale']
	]).

	:- public(check_conversion/3).
	:- mode(check_conversion(+compound, +atom, +atom), one_or_error).
	:- info(check_conversion/3, [
		comment is 'Checks a conversion request and throws an error instead of failing on invalid input.',
		argnames is ['Instant', 'FromScale', 'ToScale'],
		exceptions is [
			'``Instant``, ``FromScale``, or ``ToScale`` is a variable' - instantiation_error,
			'``FromScale`` is neither a variable nor an atom' - type_error(atom, 'FromScale'),
			'``ToScale`` is neither a variable nor an atom' - type_error(atom, 'ToScale'),
			'``FromScale`` is not supported' - domain_error(time_scale, 'FromScale'),
			'``ToScale`` is not supported' - domain_error(time_scale, 'ToScale'),
			'``Instant`` is not a valid instant term' - domain_error(instant, 'Instant'),
			'``Scale`` in ``Instant`` is neither a variable nor an atom' - type_error(atom, 'Scale'),
			'``Scale`` in ``Instant`` is not supported' - domain_error(time_scale, 'Scale'),
			'``Seconds`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Seconds'),
			'``Seconds`` in ``Instant`` is before supported UTC epoch' - domain_error(utc_unix_seconds, 'Seconds'),
			'Fraction term in ``Instant`` is not of the form ``fraction(Numerator,Denominator)``' - domain_error(fraction, 'Fraction'),
			'``Numerator`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Numerator'),
			'``Denominator`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Denominator'),
			'``Denominator`` in ``Instant`` is not positive' - domain_error(positive_denominator, 'Denominator'),
			'Normalized fraction in ``Instant`` is not in the ``[0,1[`` interval' - domain_error(normalized_fraction, 'Fraction'),
			'``Instant`` scale does not match ``FromScale``' - domain_error(scale_mismatch, instant_scale('InstantScale', 'FromScale'))
		]
	]).

	:- public(instant_to_utc_date_time/2).
	:- mode(instant_to_utc_date_time(+compound, -compound), zero_or_one).
	:- info(instant_to_utc_date_time/2, [
		comment is 'Converts an UTC instant represented as ``instant(utc,Seconds,fraction(Numerator,Denominator))`` to a UTC datetime term. Requires zero fractional part.',
		argnames is ['Instant', 'UTCDateTime']
	]).

	:- public(convert/4).
	:- mode(convert(+compound, +atom, +atom, -compound), zero_or_one).
	:- info(convert/4, [
		comment is 'Converts an ``instant(Scale,Seconds,fraction(Numerator,Denominator))`` from ``FromScale`` to ``ToScale``.',
		argnames is ['Instant', 'FromScale', 'ToScale', 'ConvertedInstant']
	]).

	:- public(check_convert/4).
	:- mode(check_convert(+compound, +atom, +atom, -compound), one_or_error).
	:- info(check_convert/4, [
		comment is 'Converts an instant from ``FromScale`` to ``ToScale`` and throws an error instead of failing on invalid input.',
		argnames is ['Instant', 'FromScale', 'ToScale', 'ConvertedInstant'],
		exceptions is [
			'``Instant``, ``FromScale``, or ``ToScale`` is a variable' - instantiation_error,
			'``FromScale`` is neither a variable nor an atom' - type_error(atom, 'FromScale'),
			'``ToScale`` is neither a variable nor an atom' - type_error(atom, 'ToScale'),
			'``FromScale`` is not supported' - domain_error(time_scale, 'FromScale'),
			'``ToScale`` is not supported' - domain_error(time_scale, 'ToScale'),
			'``Instant`` is not a valid instant term' - domain_error(instant, 'Instant'),
			'``Scale`` in ``Instant`` is neither a variable nor an atom' - type_error(atom, 'Scale'),
			'``Scale`` in ``Instant`` is not supported' - domain_error(time_scale, 'Scale'),
			'``Seconds`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Seconds'),
			'``Seconds`` in ``Instant`` is before supported UTC epoch' - domain_error(utc_unix_seconds, 'Seconds'),
			'Fraction term in ``Instant`` is not of the form ``fraction(Numerator,Denominator)``' - domain_error(fraction, 'Fraction'),
			'``Numerator`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Numerator'),
			'``Denominator`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Denominator'),
			'``Denominator`` in ``Instant`` is not positive' - domain_error(positive_denominator, 'Denominator'),
			'Normalized fraction in ``Instant`` is not in the ``[0,1[`` interval' - domain_error(normalized_fraction, 'Fraction'),
			'``Instant`` scale does not match ``FromScale``' - domain_error(scale_mismatch, instant_scale('InstantScale', 'FromScale'))
		]
	]).

	:- public(offset/3).
	:- mode(offset(+compound, +atom, -compound), zero_or_one).
	:- info(offset/3, [
		comment is 'Returns the offset required to convert an instant to a target scale as ``rational(Numerator,Denominator)``.',
		argnames is ['Instant', 'ToScale', 'Offset']
	]).

	:- public(check_offset/3).
	:- mode(check_offset(+compound, +atom, -compound), one_or_error).
	:- info(check_offset/3, [
		comment is 'Returns the conversion offset and throws an error instead of failing on invalid input.',
		argnames is ['Instant', 'ToScale', 'Offset'],
		exceptions is [
			'``Instant`` or ``ToScale`` is a variable' - instantiation_error,
			'``ToScale`` is neither a variable nor an atom' - type_error(atom, 'ToScale'),
			'``ToScale`` is not supported' - domain_error(time_scale, 'ToScale'),
			'``Instant`` is not a valid instant term' - domain_error(instant, 'Instant'),
			'``Scale`` in ``Instant`` is neither a variable nor an atom' - type_error(atom, 'Scale'),
			'``Scale`` in ``Instant`` is not supported' - domain_error(time_scale, 'Scale'),
			'``Seconds`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Seconds'),
			'``Seconds`` in ``Instant`` is before supported UTC epoch' - domain_error(utc_unix_seconds, 'Seconds'),
			'Fraction term in ``Instant`` is not of the form ``fraction(Numerator,Denominator)``' - domain_error(fraction, 'Fraction'),
			'``Numerator`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Numerator'),
			'``Denominator`` in ``Instant`` is neither a variable nor an integer' - type_error(integer, 'Denominator'),
			'``Denominator`` in ``Instant`` is not positive' - domain_error(positive_denominator, 'Denominator'),
			'Normalized fraction in ``Instant`` is not in the ``[0,1[`` interval' - domain_error(normalized_fraction, 'Fraction')
		]
	]).

:- end_protocol.
