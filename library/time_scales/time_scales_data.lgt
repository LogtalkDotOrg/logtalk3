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


:- object(time_scales_data).

	:- info([
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Bundled and override data plus constants for UTC/TAI/TT/UT1/TDB/TCG/TCB conversions.'
	]).

	:- dynamic(leap_override_data/2).
	:- dynamic(dut1_override_data/3).
	:- dynamic(leap_source_/1).
	:- dynamic(dut1_source_/1).

	:- initialization(reset_sources).

	:- public(load_leap_seconds_override/1).
	:- mode(load_leap_seconds_override(+atom), one).
	:- info(load_leap_seconds_override/1, [
		comment is 'Loads leap-second override data from a file containing ``leap(UnixSeconds,OffsetSeconds).`` terms.',
		remarks is [
			'Term format' - 'Each term must be of the form ``leap(UnixSeconds,OffsetSeconds).``.',
			'Term ordering' - 'Terms must be sorted by increasing ``UnixSeconds``.',
			'Value constraints' - '``UnixSeconds`` must be an integer greater than or equal to ``63072000`` and ``OffsetSeconds`` must be a non-decreasing integer sequence.'
		],
		argnames is ['File']
	]).

	:- public(clear_leap_seconds_override/0).
	:- mode(clear_leap_seconds_override, one).
	:- info(clear_leap_seconds_override/0, [
		comment is 'Clears leap-second override data and reverts to bundled data.'
	]).

	:- public(leap_seconds_source/1).
	:- mode(leap_seconds_source(-atom), one).
	:- info(leap_seconds_source/1, [
		comment is 'Returns the active leap-seconds data source as ``bundled`` or ``override``.',
		argnames is ['Source']
	]).

	:- public(leap_seconds_entries/1).
	:- mode(leap_seconds_entries(-list), one).
	:- info(leap_seconds_entries/1, [
		comment is 'Returns the active leap-seconds data as an ordered list of ``leap(UnixSeconds,OffsetSeconds)`` terms.',
		argnames is ['Entries']
	]).

	:- public(save_leap_seconds_entries/1).
	:- mode(save_leap_seconds_entries(+atom), one).
	:- info(save_leap_seconds_entries/1, [
		comment is 'Saves the active leap-seconds data to a file using ``leap(UnixSeconds,OffsetSeconds).`` terms.',
		argnames is ['File']
	]).

	:- public(load_dut1_override/1).
	:- mode(load_dut1_override(+atom), one).
	:- info(load_dut1_override/1, [
		comment is 'Loads DUT1 override data from a file containing ``dut1(UnixSeconds,Numerator,Denominator).`` terms.',
		remarks is [
			'Term format' - 'Each term must be of the form ``dut1(UnixSeconds,Numerator,Denominator).``.',
			'Term ordering' - 'Terms must be sorted by increasing ``UnixSeconds``.',
			'Value constraints' - '``UnixSeconds`` and ``Numerator`` must be integers; ``Denominator`` must be a positive integer; ``UnixSeconds`` must be greater than or equal to ``63072000``.'
		],
		argnames is ['File']
	]).

	:- public(clear_dut1_override/0).
	:- mode(clear_dut1_override, one).
	:- info(clear_dut1_override/0, [
		comment is 'Clears DUT1 override data and reverts to bundled data.'
	]).

	:- public(dut1_source/1).
	:- mode(dut1_source(-atom), one).
	:- info(dut1_source/1, [
		comment is 'Returns the active DUT1 data source as ``bundled`` or ``override``.',
		argnames is ['Source']
	]).

	:- public(dut1_entries/1).
	:- mode(dut1_entries(-list), one).
	:- info(dut1_entries/1, [
		comment is 'Returns the active DUT1 data as an ordered list of ``dut1(UnixSeconds,Numerator,Denominator)`` terms.',
		argnames is ['Entries']
	]).

	:- public(save_dut1_entries/1).
	:- mode(save_dut1_entries(+atom), one).
	:- info(save_dut1_entries/1, [
		comment is 'Saves the active DUT1 data to a file using ``dut1(UnixSeconds,Numerator,Denominator).`` terms.',
		argnames is ['File']
	]).

	:- public(leap_offset_at_utc_unix/2).
	:- mode(leap_offset_at_utc_unix(+integer, -integer), zero_or_one).
	:- info(leap_offset_at_utc_unix/2, [
		comment is 'Returns the ``TAI-UTC`` offset in SI seconds for a given UTC Unix epoch second within the supported range.',
		argnames is ['UnixSeconds', 'OffsetSeconds']
	]).

	:- public(leap_effective_date/2).
	:- mode(leap_effective_date(?compound, ?integer), zero_or_more).
	:- info(leap_effective_date/2, [
		comment is 'Enumerates UTC effective dates for ``TAI-UTC`` offset changes and their resulting offset in SI seconds.',
		argnames is ['UTCDateTime', 'OffsetSeconds']
	]).

	:- public(tt_minus_tai/2).
	:- mode(tt_minus_tai(-integer, -integer), one).
	:- info(tt_minus_tai/2, [
		comment is 'Returns the constant TT minus TAI offset as a rational value ``Numerator/Denominator`` seconds.',
		argnames is ['Numerator', 'Denominator']
	]).

	:- public(dut1_offset_at_utc_unix/3).
	:- mode(dut1_offset_at_utc_unix(+integer, -integer, -integer), zero_or_one).
	:- info(dut1_offset_at_utc_unix/3, [
		comment is 'Returns DUT1 (``UT1-UTC``) at a UTC Unix epoch second as a rational value ``Numerator/Denominator``.',
		argnames is ['UnixSeconds', 'Numerator', 'Denominator']
	]).

	:- public(tdb_minus_tt_approx/3).
	:- mode(tdb_minus_tt_approx(+integer, +compound, -float), one).
	:- info(tdb_minus_tt_approx/3, [
		comment is 'Returns an approximate ``TDB-TT`` offset in seconds for a TT instant represented by integer seconds and normalized fraction.',
		argnames is ['TTSeconds', 'Fraction', 'OffsetSeconds']
	]).

	:- public(tcg_minus_tt_approx/3).
	:- mode(tcg_minus_tt_approx(+integer, +compound, -float), one).
	:- info(tcg_minus_tt_approx/3, [
		comment is 'Returns an approximate ``TCG-TT`` offset in seconds for a TT instant represented by integer seconds and normalized fraction.',
		argnames is ['TTSeconds', 'Fraction', 'OffsetSeconds']
	]).

	:- public(tcb_minus_tdb_approx/3).
	:- mode(tcb_minus_tdb_approx(+integer, +compound, -float), one).
	:- info(tcb_minus_tdb_approx/3, [
		comment is 'Returns an approximate ``TCB-TDB`` offset in seconds for a TDB instant represented by integer seconds and normalized fraction.',
		argnames is ['TDBSeconds', 'Fraction', 'OffsetSeconds']
	]).

	:- public(tai_minus_utc_for_tai_unix/2).
	:- mode(tai_minus_utc_for_tai_unix(+integer, -integer), zero_or_one).
	:- info(tai_minus_utc_for_tai_unix/2, [
		comment is 'Returns the ``TAI-UTC`` offset in SI seconds for a given TAI instant represented as Unix-like integer seconds.',
		argnames is ['TAISeconds', 'OffsetSeconds']
	]).

	% TT - TAI = 32.184 seconds
	tt_minus_tai(32184, 1000).

	% Default bundled DUT1 approximation for full supported range.
	% More accurate values can be supplied through override files.
	bundled_dut1_entry(63072000, 0, 1).

	% Effective UTC dates when TAI-UTC changed and resulting offset in seconds.
	% Unix epoch seconds use POSIX semantics.
	leap_effective_date(date_time(1972, 1, 1, 0, 0, 0), 10).
	leap_effective_date(date_time(1972, 7, 1, 0, 0, 0), 11).
	leap_effective_date(date_time(1973, 1, 1, 0, 0, 0), 12).
	leap_effective_date(date_time(1974, 1, 1, 0, 0, 0), 13).
	leap_effective_date(date_time(1975, 1, 1, 0, 0, 0), 14).
	leap_effective_date(date_time(1976, 1, 1, 0, 0, 0), 15).
	leap_effective_date(date_time(1977, 1, 1, 0, 0, 0), 16).
	leap_effective_date(date_time(1978, 1, 1, 0, 0, 0), 17).
	leap_effective_date(date_time(1979, 1, 1, 0, 0, 0), 18).
	leap_effective_date(date_time(1980, 1, 1, 0, 0, 0), 19).
	leap_effective_date(date_time(1981, 7, 1, 0, 0, 0), 20).
	leap_effective_date(date_time(1982, 7, 1, 0, 0, 0), 21).
	leap_effective_date(date_time(1983, 7, 1, 0, 0, 0), 22).
	leap_effective_date(date_time(1985, 7, 1, 0, 0, 0), 23).
	leap_effective_date(date_time(1988, 1, 1, 0, 0, 0), 24).
	leap_effective_date(date_time(1990, 1, 1, 0, 0, 0), 25).
	leap_effective_date(date_time(1991, 1, 1, 0, 0, 0), 26).
	leap_effective_date(date_time(1992, 7, 1, 0, 0, 0), 27).
	leap_effective_date(date_time(1993, 7, 1, 0, 0, 0), 28).
	leap_effective_date(date_time(1994, 7, 1, 0, 0, 0), 29).
	leap_effective_date(date_time(1996, 1, 1, 0, 0, 0), 30).
	leap_effective_date(date_time(1997, 7, 1, 0, 0, 0), 31).
	leap_effective_date(date_time(1999, 1, 1, 0, 0, 0), 32).
	leap_effective_date(date_time(2006, 1, 1, 0, 0, 0), 33).
	leap_effective_date(date_time(2009, 1, 1, 0, 0, 0), 34).
	leap_effective_date(date_time(2012, 7, 1, 0, 0, 0), 35).
	leap_effective_date(date_time(2015, 7, 1, 0, 0, 0), 36).
	leap_effective_date(date_time(2017, 1, 1, 0, 0, 0), 37).

	utc_start_unix(  63072000, 10).
	utc_start_unix(  78796800, 11).
	utc_start_unix(  94694400, 12).
	utc_start_unix( 126230400, 13).
	utc_start_unix( 157766400, 14).
	utc_start_unix( 189302400, 15).
	utc_start_unix( 220924800, 16).
	utc_start_unix( 252460800, 17).
	utc_start_unix( 283996800, 18).
	utc_start_unix( 315532800, 19).
	utc_start_unix( 362793600, 20).
	utc_start_unix( 394329600, 21).
	utc_start_unix( 425865600, 22).
	utc_start_unix( 489024000, 23).
	utc_start_unix( 567993600, 24).
	utc_start_unix( 631152000, 25).
	utc_start_unix( 662688000, 26).
	utc_start_unix( 709948800, 27).
	utc_start_unix( 741484800, 28).
	utc_start_unix( 773020800, 29).
	utc_start_unix( 820454400, 30).
	utc_start_unix( 867715200, 31).
	utc_start_unix( 915148800, 32).
	utc_start_unix(1136073600, 33).
	utc_start_unix(1230768000, 34).
	utc_start_unix(1341100800, 35).
	utc_start_unix(1435708800, 36).
	utc_start_unix(1483228800, 37).

	leap_offset_at_utc_unix(UnixSeconds, OffsetSeconds) :-
		integer(UnixSeconds),
		UnixSeconds >= 63072000,
		leap_seconds_source(Source),
		last_leap_offset_at_or_before(Source, UnixSeconds, OffsetSeconds).

	tai_minus_utc_for_tai_unix(TAISeconds, OffsetSeconds) :-
		integer(TAISeconds),
		TAISeconds >= 63072010,
		leap_seconds_source(Source),
		leap_table_entry(Source, StartUTC, OffsetSeconds),
		StartTAI is StartUTC + OffsetSeconds,
		StartTAI =< TAISeconds,
		\+ (
			leap_table_entry(Source, NextStartUTC, NextOffset),
			NextStartTAI is NextStartUTC + NextOffset,
			NextStartTAI =< TAISeconds,
			NextStartTAI > StartTAI
		).

	dut1_offset_at_utc_unix(UnixSeconds, Numerator, Denominator) :-
		integer(UnixSeconds),
		UnixSeconds >= 63072000,
		dut1_source(Source),
		last_dut1_offset_at_or_before(Source, UnixSeconds, Numerator, Denominator).

	tdb_minus_tt_approx(TTSeconds, fraction(Numerator, Denominator), OffsetSeconds) :-
		TTDays is TTSeconds / 86400.0 + Numerator / Denominator / 86400.0,
		JulianDay is 2440587.5 + TTDays,
		MeanAnomalyDegrees is 357.53 + 0.9856003 * (JulianDay - 2451545.0),
		MeanAnomalyRadians is MeanAnomalyDegrees * pi / 180.0,
		OffsetSeconds is 0.001657 * sin(MeanAnomalyRadians) + 0.000022 * sin(2.0 * MeanAnomalyRadians).

	tcg_minus_tt_approx(TTSeconds, fraction(Numerator, Denominator), OffsetSeconds) :-
		ReferenceTT is 220924832.184,
		TTInstant is TTSeconds + Numerator / Denominator,
		OffsetSeconds is 6.969290134e-10 * (TTInstant - ReferenceTT).

	tcb_minus_tdb_approx(TDBSeconds, fraction(Numerator, Denominator), OffsetSeconds) :-
		ReferenceTDB is 220924832.184,
		TDBInstant is TDBSeconds + Numerator / Denominator,
		OffsetSeconds is 1.550519768e-8 * (TDBInstant - ReferenceTDB).

	load_leap_seconds_override(File) :-
		check_file_specification(File),
		load_terms_file(File, Terms),
		parse_leap_terms(Terms, Pairs),
		validate_leap_pairs(Pairs),
		retractall(leap_override_data(_, _)),
		assert_leap_pairs(Pairs),
		retractall(leap_source_(_)),
		assertz(leap_source_(override)).

	clear_leap_seconds_override :-
		retractall(leap_override_data(_, _)),
		retractall(leap_source_(_)),
		assertz(leap_source_(bundled)).

	leap_seconds_source(Source) :-
		leap_source_(Source),
		!.

	leap_seconds_entries(Entries) :-
		leap_seconds_source(Source),
		findall(leap(UnixSeconds, OffsetSeconds), leap_table_entry(Source, UnixSeconds, OffsetSeconds), Entries).

	save_leap_seconds_entries(File) :-
		check_output_file(File),
		leap_seconds_entries(Entries),
		save_entries(File, Entries).

	load_dut1_override(File) :-
		check_file_specification(File),
		load_terms_file(File, Terms),
		parse_dut1_terms(Terms, Entries),
		validate_dut1_entries(Entries),
		retractall(dut1_override_data(_, _, _)),
		assert_dut1_entries(Entries),
		retractall(dut1_source_(_)),
		assertz(dut1_source_(override)).

	clear_dut1_override :-
		retractall(dut1_override_data(_, _, _)),
		retractall(dut1_source_(_)),
		assertz(dut1_source_(bundled)).

	dut1_source(Source) :-
		dut1_source_(Source),
		!.

	dut1_entries(Entries) :-
		dut1_source(Source),
		findall(dut1(UnixSeconds, Numerator, Denominator), dut1_table_entry(Source, UnixSeconds, Numerator, Denominator), Entries).

	save_dut1_entries(File) :-
		check_output_file(File),
		dut1_entries(Entries),
		save_entries(File, Entries).

	reset_sources :-
		retractall(leap_source_(_)),
		retractall(dut1_source_(_)),
		assertz(leap_source_(bundled)),
		assertz(dut1_source_(bundled)).

	leap_table_entry(bundled, UnixStart, OffsetSeconds) :-
		utc_start_unix(UnixStart, OffsetSeconds).
	leap_table_entry(override, UnixStart, OffsetSeconds) :-
		leap_override_data(UnixStart, OffsetSeconds).

	dut1_table_entry(bundled, UnixStart, Numerator, Denominator) :-
		bundled_dut1_entry(UnixStart, Numerator, Denominator).
	dut1_table_entry(override, UnixStart, Numerator, Denominator) :-
		dut1_override_data(UnixStart, Numerator, Denominator).

	last_leap_offset_at_or_before(Source, UnixSeconds, OffsetSeconds) :-
		leap_table_entry(Source, Start, Offset),
		Start =< UnixSeconds,
		\+ (leap_table_entry(Source, Start2, _), Start2 =< UnixSeconds, Start2 > Start),
		OffsetSeconds = Offset.

	last_dut1_offset_at_or_before(bundled, UnixSeconds, Numerator, Denominator) :-
		bundled_dut1_entry(Start, N, D),
		Start =< UnixSeconds,
		\+ (bundled_dut1_entry(Start2, _, _), Start2 =< UnixSeconds, Start2 > Start),
		Numerator = N,
		Denominator = D.
	last_dut1_offset_at_or_before(override, UnixSeconds, Numerator, Denominator) :-
		dut1_override_data(Start, N, D),
		Start =< UnixSeconds,
		\+ (dut1_override_data(Start2, _, _), Start2 =< UnixSeconds, Start2 > Start),
		Numerator = N,
		Denominator = D.

	load_terms_file(File, Terms) :-
		open_override_file(File, Stream),
		read_terms(Stream, Terms),
		close(Stream).

	open_override_file(File, Stream) :-
		(   atom(File) ->
			catch(open(File, read, Stream), Error, handle_open_error(File, Error, Stream))
		;   logtalk::expand_library_path(File, Expanded) ->
			catch(open(Expanded, read, Stream), Error, rethrow_open_error(File, Error))
		;   existence_error(source_sink, File)
		).

	handle_open_error(File, error(existence_error(source_sink, _), _), Stream) :-
		!,
		(   logtalk::expand_library_path(time_scales(File), Expanded) ->
			catch(open(Expanded, read, Stream), Error, rethrow_open_error(File, Error))
		;   existence_error(source_sink, File)
		).
	handle_open_error(File, Error, _) :-
		rethrow_open_error(File, Error).

	rethrow_open_error(File, error(existence_error(source_sink, _), _)) :-
		!,
		existence_error(source_sink, File).
	rethrow_open_error(_, Error) :-
		throw(Error).

	read_terms(Stream, Terms) :-
		read_term(Stream, Term, []),
		(   Term == end_of_file ->
			Terms = []
		;   Terms = [Term| Rest],
			read_terms(Stream, Rest)
		).

	save_entries(File, Entries) :-
		open(File, write, Stream),
		write_entries(Entries, Stream),
		close(Stream).

	write_entries([], _).
	write_entries([Entry| Entries], Stream) :-
		write_term(Stream, Entry, [quoted(true)]),
		write(Stream, '.'),
		nl(Stream),
		write_entries(Entries, Stream).

	check_file_specification(File) :-
		(   var(File) ->
			instantiation_error
		;   (atom(File); compound(File)) ->
			true
		;   domain_error(file_specification, File)
		).

	check_output_file(File) :-
		(   var(File) ->
			instantiation_error
		;   atom(File) ->
			true
		;   type_error(atom, File)
		).

	parse_leap_terms([], []).
	parse_leap_terms([leap(UnixSeconds, OffsetSeconds)| Terms], [(UnixSeconds, OffsetSeconds)| Pairs]) :-
		!,
		parse_leap_terms(Terms, Pairs).
	parse_leap_terms([Term| _], _) :-
		domain_error(leap_override_term, Term).

	parse_dut1_terms([], []).
	parse_dut1_terms([dut1(UnixSeconds, Numerator, Denominator)| Terms], [(UnixSeconds, Numerator, Denominator)| Entries]) :-
		!,
		parse_dut1_terms(Terms, Entries).
	parse_dut1_terms([Term| _], _) :-
		domain_error(dut1_override_term, Term).

	validate_leap_pairs([(UnixSeconds, OffsetSeconds)| Pairs]) :-
		integer(UnixSeconds), UnixSeconds >= 63072000,
		integer(OffsetSeconds), OffsetSeconds >= 10,
		validate_leap_pairs(Pairs, UnixSeconds, OffsetSeconds).
	validate_leap_pairs([]).

	validate_leap_pairs([], _, _).
	validate_leap_pairs([(UnixSeconds, OffsetSeconds)| Pairs], PrevUnixSeconds, PrevOffsetSeconds) :-
		integer(UnixSeconds),
		integer(OffsetSeconds),
		UnixSeconds > PrevUnixSeconds,
		OffsetSeconds >= PrevOffsetSeconds,
		validate_leap_pairs(Pairs, UnixSeconds, OffsetSeconds).

	validate_dut1_entries([(UnixSeconds, Numerator, Denominator)| Entries]) :-
		integer(UnixSeconds), UnixSeconds >= 63072000,
		integer(Numerator),
		integer(Denominator), Denominator > 0,
		validate_dut1_entries(Entries, UnixSeconds).
	validate_dut1_entries([]).

	validate_dut1_entries([], _).
	validate_dut1_entries([(UnixSeconds, Numerator, Denominator)| Entries], PrevUnixSeconds) :-
		integer(UnixSeconds),
		UnixSeconds > PrevUnixSeconds,
		integer(Numerator),
		integer(Denominator), Denominator > 0,
		validate_dut1_entries(Entries, UnixSeconds).

	assert_leap_pairs([]).
	assert_leap_pairs([(UnixSeconds, OffsetSeconds)| Pairs]) :-
		assertz(leap_override_data(UnixSeconds, OffsetSeconds)),
		assert_leap_pairs(Pairs).

	assert_dut1_entries([]).
	assert_dut1_entries([(UnixSeconds, Numerator, Denominator)| Entries]) :-
		assertz(dut1_override_data(UnixSeconds, Numerator, Denominator)),
		assert_dut1_entries(Entries).

:- end_object.
