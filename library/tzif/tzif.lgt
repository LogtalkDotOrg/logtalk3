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


:- object(tzif,
	implements(tzif_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-07,
		comment is 'Loader, per-zone cache and snapshot persistence support, and zone-aware UTC lookup predicates for TZif v1/v2/v3 files.'
	]).

	:- private(cached_tzif_/2).
	:- dynamic(cached_tzif_/2).
	:- mode(cached_tzif_(?atom, ?compound), zero_or_more).
	:- info(cached_tzif_/2, [
		comment is 'Table holding the currently cached TZif terms keyed by zone identifier.',
		argnames is ['Zone', 'TZif']
	]).

	:- uses(date, [
		date_time_to_unix/2, day_of_year_date/3, leap_year/1, month_weekday_date/5, unix_to_date_time/2
	]).

	:- uses(list, [
		append/3, drop/3, last/2, length/2, member/2, take/4
	]).

	:- uses(logtalk, [
		expand_library_path/2
	]).

	:- uses(os, [
		directory_files/3, path_concat/3
	]).

	:- uses(reader, [
		stream_to_bytes/2
	]).

	:- uses(tzif_zone_ids, [
		known_zone_id/1
	]).

	load(Source) :-
		load(Source, TZifs),
		cache(TZifs).

	load(Source, _) :-
		var(Source),
		instantiation_error.
	load(Source, TZifs) :-
		load_source(Source, LoadedTZifs),
		normalize_tzif_list(LoadedTZifs, TZifs).

	cache(TZifs0) :-
		normalize_tzif_list(TZifs0, TZifs),
		cache_tzifs(TZifs).

	save(TZifs0, File) :-
		normalize_tzif_list(TZifs0, TZifs),
		check_output_file(File),
		open(File, write, Stream),
		save_snapshot_terms(TZifs, Stream),
		close(Stream).

	save(File) :-
		require_cached_tzifs(TZifs),
		save(TZifs, File).

	clear_cache :-
		retractall(cached_tzif_(_, _)).

	cache_source(Source) :-
		cached_tzif_(_, tzif(_, Source, _)).

	cached_tzif(TZif) :-
		cached_tzif_(_, TZif).

	zone(TZif, Zone, ZoneData) :-
		check_tzif_term(TZif),
		TZif = tzif(Zone, _, ZoneData).

	zones(TZifs0, Zones) :-
		normalize_tzif_list(TZifs0, TZifs),
		tzif_zones(TZifs, Zones).

	zones(Zones) :-
		require_cached_tzifs(TZifs),
		tzif_zones(TZifs, Zones).

	time_type(TZif, Zone, UTC, TimeType) :-
		normalize_query_utc(UTC, UnixSeconds),
		require_tzif_zone(TZif, Zone, ZoneData),
		lookup_time_type(ZoneData, UnixSeconds, TimeType).

	time_type(Zone, UTC, TimeType) :-
		atom(Zone),
		normalize_query_utc(UTC, UnixSeconds),
		require_cached_zone(Zone, ZoneData),
		lookup_time_type(ZoneData, UnixSeconds, TimeType).

	time_type(UTC, TimeType) :-
		require_single_cached_tzif(tzif(_, _, ZoneData)),
		normalize_query_utc(UTC, UnixSeconds),
		lookup_time_type(ZoneData, UnixSeconds, TimeType).

	offset(TZif, Zone, UTC, OffsetSeconds) :-
		time_type(TZif, Zone, UTC, time_type(OffsetSeconds, _, _, _)).

	offset(Zone, UTC, OffsetSeconds) :-
		time_type(Zone, UTC, time_type(OffsetSeconds, _, _, _)).

	offset(UTC, OffsetSeconds) :-
		time_type(UTC, time_type(OffsetSeconds, _, _, _)).

	daylight_saving_time(TZif, Zone, UTC, IsDST) :-
		time_type(TZif, Zone, UTC, time_type(_, IsDST, _, _)).

	daylight_saving_time(Zone, UTC, IsDST) :-
		time_type(Zone, UTC, time_type(_, IsDST, _, _)).

	daylight_saving_time(UTC, IsDST) :-
		time_type(UTC, time_type(_, IsDST, _, _)).

	abbreviation(TZif, Zone, UTC, Abbreviation) :-
		time_type(TZif, Zone, UTC, time_type(_, _, Abbreviation, _)).

	abbreviation(Zone, UTC, Abbreviation) :-
		time_type(Zone, UTC, time_type(_, _, Abbreviation, _)).

	abbreviation(UTC, Abbreviation) :-
		time_type(UTC, time_type(_, _, Abbreviation, _)).

	cache_tzifs([]).
	cache_tzifs([TZif| TZifs]) :-
		cache_tzif(TZif),
		cache_tzifs(TZifs).

	cache_tzif(TZif) :-
		TZif = tzif(Zone, _, _),
		retractall(cached_tzif_(Zone, _)),
		assertz(cached_tzif_(Zone, TZif)).

	require_cached_tzifs(TZifs) :-
		findall(TZif, cached_tzif_(_, TZif), TZifs),
		TZifs \== [],
		!.
	require_cached_tzifs(_) :-
		existence_error(tzif_cache, tzif).

	require_cached_zone(Zone, ZoneData) :-
		( cached_tzif_(Zone, tzif(Zone, _, ZoneData)) -> true ; existence_error(time_zone, Zone) ).

	require_single_cached_tzif(TZif) :-
		require_cached_tzifs(TZifs),
		( TZifs = [TZif] -> true ; domain_error(single_zone_tzif_cache, TZifs) ).

	require_tzif_zone(TZif, Zone, ZoneData) :-
		check_tzif_term(TZif),
		( TZif = tzif(Zone, _, ZoneData) -> true ; existence_error(time_zone, Zone) ).

	load_source(snapshot(File), TZifs) :-
		!,
		load_snapshot_terms(File, TZifs).
	load_source(file(File, ZoneId), [TZif]) :-
		!,
		check_source_zone_id(ZoneId, file(File, ZoneId)),
		parse_source(file(File), ZoneData),
		make_tzif(ZoneId, file(File, ZoneId), ZoneData, TZif).
	load_source(stream(Stream, ZoneId), [TZif]) :-
		!,
		check_source_zone_id(ZoneId, stream(Stream, ZoneId)),
		parse_source(stream(Stream), ZoneData),
		make_tzif(ZoneId, stream(Stream, ZoneId), ZoneData, TZif).
	load_source(bytes(Bytes, ZoneId), [TZif]) :-
		!,
		check_source_zone_id(ZoneId, bytes(Bytes, ZoneId)),
		parse_source(bytes(Bytes), ZoneData),
		make_tzif(ZoneId, bytes(Bytes, ZoneId), ZoneData, TZif).
	load_source(files(Root, Paths), TZifs) :-
		!,
		load_zone_files(Root, Paths, TZifs).
	load_source(directory(Root), TZifs) :-
		!,
		directory_zone_paths(Root, Paths0),
		filter_known_zone_paths(Paths0, Paths),
		load_zone_files(Root, Paths, TZifs).
	load_source(Source, _) :-
		domain_error(tzif_source, Source).

	make_tzif(Zone, Source, ZoneData, TZif) :-
		TZif = tzif(Zone, Source, ZoneData),
		check_tzif_term(TZif).

	load_zone_files(Root, Paths, TZifs) :-
		check_directory_root(Root),
		check_zone_paths(Paths),
		check_known_zone_paths(Paths),
		load_zone_paths(Paths, Root, TZifs).

	load_zone_paths([], _, []).
	load_zone_paths([Path| Paths], Root, [TZif| TZifs]) :-
		path_concat(Root, Path, File),
		parse_source(file(File), ZoneData),
		make_tzif(Path, file(File, Path), ZoneData, TZif),
		load_zone_paths(Paths, Root, TZifs).

	directory_zone_paths(Root, Paths) :-
		check_directory_root(Root),
		directory_zone_paths(Root, '', Paths).

	directory_zone_paths(Directory, Prefix, Paths) :-
		directory_files(Directory, Files, [type(regular), paths(relative), dot_files(false)]),
		prefix_relative_paths(Files, Prefix, FilePaths),
		directory_files(Directory, Directories, [type(directory), paths(relative), dot_files(false)]),
		directory_zone_paths_directories(Directories, Directory, Prefix, NestedPaths),
		append(FilePaths, NestedPaths, Paths).

	directory_zone_paths_directories([], _, _, []).
	directory_zone_paths_directories([Child| Children], Directory, Prefix, Paths) :-
		path_concat(Directory, Child, ChildDirectory),
		relative_path_concat(Prefix, Child, ChildPrefix),
		directory_zone_paths(ChildDirectory, ChildPrefix, ChildPaths),
		directory_zone_paths_directories(Children, Directory, Prefix, RemainingPaths),
		append(ChildPaths, RemainingPaths, Paths).

	prefix_relative_paths([], _, []).
	prefix_relative_paths([Path| Paths], Prefix, [PrefixedPath| PrefixedPaths]) :-
		relative_path_concat(Prefix, Path, PrefixedPath),
		prefix_relative_paths(Paths, Prefix, PrefixedPaths).

	relative_path_concat('', Path, Path) :-
		!.
	relative_path_concat(Prefix, Path, PrefixedPath) :-
		path_concat(Prefix, Path, PrefixedPath).

	normalize_tzif_list(TZifs0, TZifs) :-
		check_tzif_list(TZifs0),
		normalize_tzif_terms(TZifs0, TZifs).

	check_tzif_list(TZifs) :-
		var(TZifs),
		!,
		instantiation_error.
	check_tzif_list([]) :-
		!.
	check_tzif_list([TZif| TZifs]) :-
		!,
		check_tzif_term(TZif),
		check_tzif_list(TZifs).
	check_tzif_list(TZifs) :-
		type_error(list, TZifs).

	normalize_tzif_terms([], []).
	normalize_tzif_terms([TZif| TZifs], Normalized) :-
		normalize_tzif_terms(TZifs, TailNormalized),
		merge_tzif(TZif, TailNormalized, Normalized).

	merge_tzif(TZif, TZifs, TZifs) :-
		tzif_zone_id(TZif, Zone),
		member(tzif(Zone, _, _), TZifs),
		!.
	merge_tzif(TZif, TZifs, [TZif| TZifs]).

	tzif_zone_id(tzif(Zone, _, _), Zone).

	tzif_zones(TZifs, Zones) :-
		tzif_terms_zones(TZifs, Zones).

	tzif_terms_zones([], []).
	tzif_terms_zones([tzif(Zone, _, _)| TZifs], [Zone| Zones]) :-
		tzif_terms_zones(TZifs, Zones).

	check_directory_root(Root) :-
		( atom(Root) -> true ; domain_error(directory, Root) ).

	check_zone_paths([]).
	check_zone_paths([Path| Paths]) :-
		( atom(Path) -> true ; domain_error(file_name, Path) ),
		check_zone_paths(Paths).

	check_known_zone_paths([]).
	check_known_zone_paths([Path| Paths]) :-
		( known_zone_id(Path) -> true ; domain_error(time_zone, Path) ),
		check_known_zone_paths(Paths).

	filter_known_zone_paths([], []).
	filter_known_zone_paths([Path| Paths], [Path| KnownPaths]) :-
		known_zone_id(Path),
		!,
		filter_known_zone_paths(Paths, KnownPaths).
	filter_known_zone_paths([_| Paths], KnownPaths) :-
		filter_known_zone_paths(Paths, KnownPaths).

	check_source_zone_id(ZoneId, _) :-
		var(ZoneId),
		!,
		instantiation_error.
	check_source_zone_id(ZoneId, _) :-
		atom(ZoneId),
		( known_zone_id(ZoneId) -> true ; domain_error(time_zone, ZoneId) ),
		!.
	check_source_zone_id(_, Source) :-
		domain_error(tzif_source, Source).

	normalize_query_utc(UTC, UTC) :-
		integer(UTC),
		!.
	normalize_query_utc(date_time(Year, Month, Day, Hours, Minutes, Seconds), UnixSeconds) :-
		date_time_to_unix(date_time(Year, Month, Day, Hours, Minutes, Seconds), UnixSeconds).

	parse_source(file(File), ZoneData) :-
		!,
		read_binary_source(File, Bytes),
		parse_tzif_bytes(Bytes, ZoneData).
	parse_source(stream(Stream), ZoneData) :-
		!,
		stream_to_bytes(Stream, Bytes),
		parse_tzif_bytes(Bytes, ZoneData).
	parse_source(bytes(Bytes), ZoneData) :-
		!,
		parse_tzif_bytes(Bytes, ZoneData).
	parse_source(Source, _) :-
		domain_error(tzif_source, Source).

	parse_tzif_bytes(Bytes, ZoneData) :-
		parse_header(Bytes, Version1, Counts1, AfterHeader1),
		(	Version1 =:= 1 ->
			parse_block(4, Counts1, AfterHeader1, Block, Rest),
			Rest == [],
			block_zone_data(1, Block, footer(empty), ZoneData)
		;	Version1 >= 2,
			Version1 =< 3,
			compatibility_block_size(Counts1, 4, SkipSize),
			take(SkipSize, AfterHeader1, _, AfterCompatibility),
			parse_header(AfterCompatibility, Version2, Counts2, AfterHeader2),
			Version2 =:= Version1,
			parse_block(8, Counts2, AfterHeader2, ActiveBlock, FooterBytes),
			parse_footer_bytes(FooterBytes, Footer),
			block_zone_data(Version2, ActiveBlock, Footer, ZoneData)
		).

	block_zone_data(Version, block(Transitions, Types, Leaps, StandardWalls, UTLocals), footer(Footer),
				zone_data(version(Version), transitions(Transitions), local_time_types(Types), leap_seconds(Leaps), standard_walls(StandardWalls), ut_locals(UTLocals), footer(Footer))).

	parse_header(Bytes, Version, counts(IsUTCnt, IsStdCnt, LeapCnt, TimeCnt, TypeCnt, CharCnt), Rest) :-
		take(4, Bytes, [0'T, 0'Z, 0'i, 0'f], AfterMagic),
		take(1, AfterMagic, [VersionByte], AfterVersion),
		decode_version(VersionByte, Version),
		take(15, AfterVersion, Reserved, AfterReserved),
		all_zero_bytes(Reserved),
		read_u32(AfterReserved, IsUTCnt, AfterIsUTCnt),
		read_u32(AfterIsUTCnt, IsStdCnt, AfterIsStdCnt),
		read_u32(AfterIsStdCnt, LeapCnt, AfterLeapCnt),
		read_u32(AfterLeapCnt, TimeCnt, AfterTimeCnt),
		read_u32(AfterTimeCnt, TypeCnt, AfterTypeCnt),
		read_u32(AfterTypeCnt, CharCnt, Rest),
		TypeCnt > 0,
		CharCnt > 0,
		valid_indicator_count(IsUTCnt, TypeCnt),
		valid_indicator_count(IsStdCnt, TypeCnt).

	decode_version(0, 1).
	decode_version(0'2, 2).
	decode_version(0'3, 3).

	valid_indicator_count(0, _) :-
		!.
	valid_indicator_count(Count, Count).

	compatibility_block_size(counts(IsUTCnt, IsStdCnt, LeapCnt, TimeCnt, TypeCnt, CharCnt), TimeSize, Size) :-
		Size is TimeCnt * TimeSize + TimeCnt + TypeCnt * 6 + CharCnt + LeapCnt * (TimeSize + 4) + IsStdCnt + IsUTCnt.

	parse_block(TimeSize, counts(IsUTCnt, IsStdCnt, LeapCnt, TimeCnt, TypeCnt, CharCnt), Bytes,
				block(Transitions, Types, Leaps, StandardWalls, UTLocals), Rest) :-
		parse_signed_values(TimeCnt, TimeSize, Bytes, TransitionTimes, AfterTimes),
		parse_index_bytes(TimeCnt, TypeCnt, AfterTimes, TransitionTypeIndexes, AfterIndexes),
		parse_local_time_type_specs(TypeCnt, AfterIndexes, TypeSpecs, AfterTypes),
		take(CharCnt, AfterTypes, DesignationBytes, AfterDesignations),
		attach_abbreviations(TypeSpecs, DesignationBytes, 0, Types),
		parse_leap_records(LeapCnt, TimeSize, AfterDesignations, Leaps, AfterLeaps),
		parse_standard_walls(IsStdCnt, AfterLeaps, StandardWalls, AfterStandardWalls),
		parse_ut_locals(IsUTCnt, AfterStandardWalls, UTLocals, Rest),
		zip_transitions(TransitionTimes, TransitionTypeIndexes, Transitions),
		strictly_increasing_transition_times(Transitions),
		strictly_increasing_leaps(Leaps).

	parse_signed_values(0, _, Bytes, [], Bytes) :-
		!.
	parse_signed_values(Count, Size, Bytes, [Value| Values], Rest) :-
		Count > 0,
		take(Size, Bytes, ValueBytes, AfterValue),
		signed_integer(ValueBytes, Value),
		NextCount is Count - 1,
		parse_signed_values(NextCount, Size, AfterValue, Values, Rest).

	parse_index_bytes(0, _, Bytes, [], Bytes) :-
		!.
	parse_index_bytes(Count, TypeCnt, Bytes, [Index| Indexes], Rest) :-
		Count > 0,
		take(1, Bytes, [Index], AfterIndex),
		Index < TypeCnt,
		NextCount is Count - 1,
		parse_index_bytes(NextCount, TypeCnt, AfterIndex, Indexes, Rest).

	parse_local_time_type_specs(0, Bytes, [], Bytes) :-
		!.
	parse_local_time_type_specs(Count, Bytes, [type_spec(OffsetSeconds, IsDST, DesignationIndex)| Specs], Rest) :-
		Count > 0,
		read_s32(Bytes, OffsetSeconds, AfterOffset),
		take(1, AfterOffset, [IsDSTByte], AfterDST),
		boolean_byte(IsDSTByte, IsDST),
		take(1, AfterDST, [DesignationIndex], AfterDesignationIndex),
		NextCount is Count - 1,
		parse_local_time_type_specs(NextCount, AfterDesignationIndex, Specs, Rest).

	attach_abbreviations([], _, _, []).
	attach_abbreviations([type_spec(OffsetSeconds, IsDST, DesignationIndex)| Specs], DesignationBytes, Index,
					 [local_time_type(Index, OffsetSeconds, IsDST, Abbreviation, DesignationIndex)| Types]) :-
		abbreviation_from_index(DesignationBytes, DesignationIndex, Abbreviation),
		NextIndex is Index + 1,
		attach_abbreviations(Specs, DesignationBytes, NextIndex, Types).

	abbreviation_from_index(DesignationBytes, Index, Abbreviation) :-
		drop(Index, DesignationBytes, Suffix),
		take_until_nul(Suffix, Codes),
		atom_codes(Abbreviation, Codes).

	take_until_nul([0| _], []) :-
		!.
	take_until_nul([Code| Codes], [Code| Rest]) :-
		Code =\= 0,
		take_until_nul(Codes, Rest).

	parse_leap_records(0, _, Bytes, [], Bytes) :-
		!.
	parse_leap_records(Count, TimeSize, Bytes, [leap_second(Occurrence, Correction)| Leaps], Rest) :-
		Count > 0,
		take(TimeSize, Bytes, OccurrenceBytes, AfterOccurrence),
		signed_integer(OccurrenceBytes, Occurrence),
		read_s32(AfterOccurrence, Correction, AfterCorrection),
		NextCount is Count - 1,
		parse_leap_records(NextCount, TimeSize, AfterCorrection, Leaps, Rest).

	parse_standard_walls(0, Bytes, [], Bytes) :-
		!.
	parse_standard_walls(Count, Bytes, [Indicator| Indicators], Rest) :-
		Count > 0,
		take(1, Bytes, [Byte], AfterByte),
		(	Byte =:= 0 ->
			Indicator = wall
		;	Byte =:= 1,
			Indicator = standard
		),
		NextCount is Count - 1,
		parse_standard_walls(NextCount, AfterByte, Indicators, Rest).

	parse_ut_locals(0, Bytes, [], Bytes) :-
		!.
	parse_ut_locals(Count, Bytes, [Indicator| Indicators], Rest) :-
		Count > 0,
		take(1, Bytes, [Byte], AfterByte),
		(	Byte =:= 0 ->
			Indicator = local
		;	Byte =:= 1,
			Indicator = ut
		),
		NextCount is Count - 1,
		parse_ut_locals(NextCount, AfterByte, Indicators, Rest).

	zip_transitions([], [], []).
	zip_transitions([Time| Times], [TypeIndex| TypeIndexes], [transition(Time, TypeIndex)| Transitions]) :-
		zip_transitions(Times, TypeIndexes, Transitions).

	strictly_increasing_transition_times([]).
	strictly_increasing_transition_times([_]) :-
		!.
	strictly_increasing_transition_times([transition(Time1, _), transition(Time2, _)| Rest]) :-
		Time2 > Time1,
		strictly_increasing_transition_times([transition(Time2, _)| Rest]).

	strictly_increasing_leaps([]).
	strictly_increasing_leaps([_]) :-
		!.
	strictly_increasing_leaps([leap_second(Time1, _), leap_second(Time2, _)| Rest]) :-
		Time2 > Time1,
		strictly_increasing_leaps([leap_second(Time2, _)| Rest]).

	boolean_byte(0, false).
	boolean_byte(1, true).

	parse_footer_bytes([], footer(empty)).
	parse_footer_bytes([10, 10], footer(empty)) :-
		!.
	parse_footer_bytes([10| Bytes], Footer) :-
		split_last_byte(Bytes, Body, 10),
		(	Body == [] ->
			Footer = footer(empty)
		;	parse_posix_footer(Body, Footer)
		).

	split_last_byte([Last], [], Last) :-
		!.
	split_last_byte([Byte| Bytes], [Byte| Rest], Last) :-
		split_last_byte(Bytes, Rest, Last).

	parse_posix_footer(Codes, footer(posix(StandardAbbreviation, StandardOffset, none, none, none, none))) :-
		parse_tz_abbreviation(Codes, StandardAbbreviation, Rest1),
		parse_posix_offset(Rest1, StandardOffset, []),
		!.
	parse_posix_footer(Codes, footer(posix(StandardAbbreviation, StandardOffset, DSTAbbreviation, DSTOffset, StartRule, EndRule))) :-
		parse_tz_abbreviation(Codes, StandardAbbreviation, Rest1),
		parse_posix_offset(Rest1, StandardOffset, Rest2),
		parse_tz_abbreviation(Rest2, DSTAbbreviation, Rest3),
		parse_optional_dst_offset(Rest3, StandardOffset, DSTOffset, Rest4),
		parse_optional_rules(Rest4, StartRule, EndRule).

	parse_optional_dst_offset(Codes, _, Offset, Rest) :-
		Codes = [Code| _],
		offset_start_code(Code),
		!,
		parse_posix_offset(Codes, Offset, Rest).
	parse_optional_dst_offset(Rest, StandardOffset, DSTOffset, Rest) :-
		DSTOffset is StandardOffset + 3600.

	parse_optional_rules([], permanent, permanent).
	parse_optional_rules([0',| Codes], StartRule, EndRule) :-
		parse_rule(Codes, StartRule, AfterStart),
		AfterStart = [0',| AfterComma],
		parse_rule(AfterComma, EndRule, []).

	parse_rule(Codes, rule(Form, at(Seconds, Suffix)), Rest) :-
		parse_rule_form(Codes, Form, AfterForm),
		(	AfterForm = [0'/| AfterSlash] ->
			parse_time_of_day(AfterSlash, Seconds, Suffix, Rest)
		;	Seconds = 7200,
			Suffix = wall,
			Rest = AfterForm
		).

	parse_rule_form([0'J| Codes], julian_no_leap(Day), Rest) :-
		!,
		parse_unsigned_number(Codes, Day, Rest).
	parse_rule_form([0'M| Codes], month_weekday(Month, Week, WeekdayNumber), Rest) :-
		!,
		parse_unsigned_number(Codes, Month, [0'.| AfterMonth]),
		parse_unsigned_number(AfterMonth, Week, [0'.| AfterWeek]),
		parse_unsigned_number(AfterWeek, WeekdayNumber, Rest).
	parse_rule_form(Codes, day_of_year(Day), Rest) :-
		parse_unsigned_number(Codes, Day, Rest).

	parse_time_of_day(Codes, Seconds, Suffix, Rest) :-
		parse_signed_clock_time(Codes, Seconds, AfterClock),
		parse_optional_time_suffix(AfterClock, Suffix, Rest).

	parse_optional_minutes_seconds([0':| Codes], Minutes, Seconds, Rest) :-
		!,
		parse_unsigned_number(Codes, Minutes, AfterMinutes),
		parse_optional_seconds(AfterMinutes, Seconds, Rest).
	parse_optional_minutes_seconds(Rest, 0, 0, Rest).

	parse_optional_seconds([0':| Codes], Seconds, Rest) :-
		!,
		parse_unsigned_number(Codes, Seconds, Rest).
	parse_optional_seconds(Rest, 0, Rest).

	parse_optional_time_suffix([Code| Rest], Suffix, Rest) :-
		time_suffix(Code, Suffix),
		!.
	parse_optional_time_suffix(Rest, wall, Rest).

	time_suffix(0'w, wall).
	time_suffix(0's, standard).
	time_suffix(0'u, utc).
	time_suffix(0'g, utc).
	time_suffix(0'z, utc).

	parse_tz_abbreviation([0'<| Codes], Abbreviation, Rest) :-
		!,
		take_until_right_angle(Codes, AbbreviationCodes, Rest),
		AbbreviationCodes \== [],
		atom_codes(Abbreviation, AbbreviationCodes).
	parse_tz_abbreviation(Codes, Abbreviation, Rest) :-
		take_alpha_codes(Codes, AbbreviationCodes, Rest),
		at_least_three_codes(AbbreviationCodes),
		atom_codes(Abbreviation, AbbreviationCodes).

	take_until_right_angle([0'>| Rest], [], Rest) :-
		!.
	take_until_right_angle([Code| Codes], [Code| RestCodes], Rest) :-
		Code =\= 0'>,
		take_until_right_angle(Codes, RestCodes, Rest).

	take_alpha_codes([Code| Codes], [Code| AlphaCodes], Rest) :-
		alpha_code(Code),
		!,
		take_alpha_codes(Codes, AlphaCodes, Rest).
	take_alpha_codes(Rest, [], Rest).

	at_least_three_codes([_, _, _| _]).

	alpha_code(Code) :-
		(	Code >= 0'A, Code =< 0'Z ->
			true
		;	Code >= 0'a, Code =< 0'z
		).

	offset_start_code(0'+) :-
		!.
	offset_start_code(0'-) :-
		!.
	offset_start_code(Code) :-
		digit_code(Code).

	parse_posix_offset(Codes, OffsetSeconds, Rest) :-
		parse_signed_clock_time(Codes, PosixSeconds, Rest),
		OffsetSeconds is -PosixSeconds.

	parse_signed_clock_time([0'-| Codes], Number, Rest) :-
		!,
		parse_unsigned_clock_time(Codes, Unsigned, Rest),
		Number is -Unsigned.
	parse_signed_clock_time([0'+| Codes], Number, Rest) :-
		!,
		parse_unsigned_clock_time(Codes, Number, Rest).
	parse_signed_clock_time(Codes, Number, Rest) :-
		parse_unsigned_clock_time(Codes, Number, Rest).

	parse_unsigned_clock_time(Codes, Number, Rest) :-
		parse_unsigned_number(Codes, Hours, AfterHours),
		parse_optional_minutes_seconds(AfterHours, Minutes, Seconds, Rest),
		Number is Hours * 3600 + Minutes * 60 + Seconds.

	parse_unsigned_number([Code| Codes], Number, Rest) :-
		digit_code(Code),
		parse_unsigned_number_rest(Codes, [Code], Number, Rest).

	parse_unsigned_number_rest([Code| Codes], Digits0, Number, Rest) :-
		digit_code(Code),
		!,
		append(Digits0, [Code], Digits),
		parse_unsigned_number_rest(Codes, Digits, Number, Rest).
	parse_unsigned_number_rest(Rest, Digits, Number, Rest) :-
		number_codes(Number, Digits).

	digit_code(Code) :-
		Code >= 0'0,
		Code =< 0'9.

	lookup_time_type(zone_data(_, transitions([]), local_time_types([Type0| _]), leap_seconds(_), _, _, footer(empty)), _, TimeType) :-
		!,
		type_term_to_time_type(type_0, Type0, TimeType).
	lookup_time_type(zone_data(_, transitions([]), _, leap_seconds(_), _, _, footer(Footer)), UnixSeconds, TimeType) :-
		Footer \== empty,
		!,
		footer_time_type(Footer, UnixSeconds, TimeType).
	lookup_time_type(zone_data(_, transitions(Transitions), local_time_types(Types), leap_seconds(Leaps), _, _, footer(Footer)), UnixSeconds, TimeType) :-
		Types = [Type0| _],
		transition_comparison_time(UnixSeconds, Leaps, ComparisonTime),
		Transitions = [transition(FirstTransitionTime, _)| _],
		(	ComparisonTime < FirstTransitionTime ->
			type_term_to_time_type(type_0, Type0, TimeType)
		;	last(Transitions, transition(LastTransitionTime, _)),
			(	ComparisonTime > LastTransitionTime,
				Footer \== empty ->
				footer_time_type(Footer, UnixSeconds, TimeType)
			;	latest_transition(Transitions, ComparisonTime, transition(SourceTime, TypeIndex)),
				local_time_type(TypeIndex, Types, Type),
				type_term_to_time_type(transition(SourceTime), Type, TimeType)
			)
		).

	latest_transition([Transition], ComparisonTime, Transition) :-
		!,
		Transition = transition(Time, _),
		Time =< ComparisonTime.
	latest_transition([transition(Time, TypeIndex), transition(NextTime, NextTypeIndex)| Rest], ComparisonTime, Transition) :-
		Time =< ComparisonTime,
		(	NextTime =< ComparisonTime ->
			latest_transition([transition(NextTime, NextTypeIndex)| Rest], ComparisonTime, Transition)
		;	Transition = transition(Time, TypeIndex)
		).

	local_time_type(Index, [local_time_type(Index, OffsetSeconds, IsDST, Abbreviation, DesignationIndex)| _], local_time_type(Index, OffsetSeconds, IsDST, Abbreviation, DesignationIndex)) :-
		!.
	local_time_type(Index, [_| Types], Type) :-
		local_time_type(Index, Types, Type).

	type_term_to_time_type(Source, local_time_type(_, OffsetSeconds, IsDST, Abbreviation, _), time_type(OffsetSeconds, IsDST, Abbreviation, Source)).

	transition_comparison_time(UnixSeconds, Leaps, ComparisonTime) :-
		correction_for_utc_unix(UnixSeconds, Leaps, Correction),
		ComparisonTime is UnixSeconds + Correction.

	correction_for_utc_unix(_, [], 0) :-
		!.
	correction_for_utc_unix(UnixSeconds, [leap_second(Occurrence, Correction)| Leaps], Result) :-
		EffectiveUnix is Occurrence - Correction,
		(	UnixSeconds >= EffectiveUnix ->
			correction_for_utc_unix_after(UnixSeconds, Leaps, Correction, Result)
		;	Result = 0
		).

	correction_for_utc_unix_after(_, [], Correction, Correction) :-
		!.
	correction_for_utc_unix_after(UnixSeconds, [leap_second(Occurrence, Correction)| Leaps], _, Result) :-
		EffectiveUnix is Occurrence - Correction,
		UnixSeconds >= EffectiveUnix,
		!,
		correction_for_utc_unix_after(UnixSeconds, Leaps, Correction, Result).
	correction_for_utc_unix_after(_, _, Correction, Correction).

	footer_time_type(posix(StandardAbbreviation, StandardOffset, none, none, none, none), _,
			time_type(StandardOffset, false, StandardAbbreviation, footer)) :-
		!.
	footer_time_type(posix(_, _, DSTAbbreviation, DSTOffset, permanent, permanent), _,
			time_type(DSTOffset, true, DSTAbbreviation, footer)) :-
		!.
	footer_time_type(posix(StandardAbbreviation, StandardOffset, DSTAbbreviation, DSTOffset, StartRule, EndRule), UnixSeconds, TimeType) :-
		unix_to_date_time(UnixSeconds, date_time(Year, _, _, _, _, _)),
		rule_transition_utc(Year, StartRule, start, StandardOffset, DSTOffset, StartUTC),
		rule_transition_utc(Year, EndRule, end, StandardOffset, DSTOffset, EndUTC),
		(	StartUTC < EndUTC ->
			(	UnixSeconds >= StartUTC,
				UnixSeconds < EndUTC ->
				TimeType = time_type(DSTOffset, true, DSTAbbreviation, footer)
			;	TimeType = time_type(StandardOffset, false, StandardAbbreviation, footer)
			)
		;	(UnixSeconds >= StartUTC ; UnixSeconds < EndUTC) ->
			TimeType = time_type(DSTOffset, true, DSTAbbreviation, footer)
		;	TimeType = time_type(StandardOffset, false, StandardAbbreviation, footer)
		).

	rule_transition_utc(Year, rule(Form, at(Seconds, Suffix)), Kind, StandardOffset, DSTOffset, UTCUnixSeconds) :-
		rule_date(Form, Year, date(Year, Month, Day)),
		date_time_to_unix(date_time(Year, Month, Day, 0, 0, 0), MidnightUnixSeconds),
		ReferenceUnixSeconds is MidnightUnixSeconds + Seconds,
		rule_reference_offset(Suffix, Kind, StandardOffset, DSTOffset, ReferenceOffset),
		UTCUnixSeconds is ReferenceUnixSeconds - ReferenceOffset.

	rule_reference_offset(utc, _, _, _, 0).
	rule_reference_offset(standard, _, StandardOffset, _, StandardOffset).
	rule_reference_offset(wall, start, StandardOffset, _, StandardOffset) :-
		!.
	rule_reference_offset(wall, end, _, DSTOffset, DSTOffset).

	rule_date(julian_no_leap(Day), Year, Date) :-
		(	leap_year(Year),
			Day >= 60 ->
			ActualDay is Day + 1
		;	ActualDay = Day
		),
		day_of_year_date(Year, ActualDay, Date).
	rule_date(day_of_year(Day0), Year, Date) :-
		ActualDay is Day0 + 1,
		day_of_year_date(Year, ActualDay, Date).
	rule_date(month_weekday(Month, Week, WeekdayNumber), Year, Date) :-
		posix_weekday_date_weekday(WeekdayNumber, DateWeekday),
		month_weekday_date(Year, Month, Week, DateWeekday, Date).

	posix_weekday_date_weekday(0, 7) :-
		!.
	posix_weekday_date_weekday(WeekdayNumber, WeekdayNumber).

	check_tzif_term(TZif) :-
		valid_tzif_term(TZif),
		!.
	check_tzif_term(TZif) :-
		domain_error(tzif, TZif).

	valid_tzif_term(tzif(Zone, Source, ZoneData)) :-
		atom(Zone),
		known_zone_id(Zone),
		valid_source_term(Source),
		valid_zone_data_term(ZoneData).

	valid_source_term(file(File, ZoneId)) :-
		nonvar(File),
		atom(ZoneId).
	valid_source_term(stream(Stream, ZoneId)) :-
		nonvar(Stream),
		atom(ZoneId).
	valid_source_term(bytes(Bytes, ZoneId)) :-
		nonvar(Bytes),
		atom(ZoneId).

	valid_zone_data_term(zone_data(version(Version), transitions(Transitions), local_time_types(Types), leap_seconds(Leaps), standard_walls(StandardWalls), ut_locals(UTLocals), footer(Footer))) :-
		valid_version(Version),
		valid_local_time_types(Types, TypeCount),
		valid_transitions(Transitions, TypeCount),
		valid_leaps(Leaps),
		valid_indicator_list(StandardWalls, TypeCount, standard_wall),
		valid_indicator_list(UTLocals, TypeCount, ut_local),
		valid_footer_term(Footer).

	valid_version(1).
	valid_version(2).
	valid_version(3).

	valid_local_time_types(Types, Count) :-
		valid_local_time_types(Types, 0, Count).

	valid_local_time_types([], Count, Count).
	valid_local_time_types([local_time_type(Index, OffsetSeconds, IsDST, Abbreviation, DesignationIndex)| Types], ExpectedIndex, Count) :-
		Index =:= ExpectedIndex,
		integer(OffsetSeconds),
		valid_boolean(IsDST),
		atom(Abbreviation),
		integer(DesignationIndex),
		DesignationIndex >= 0,
		NextIndex is ExpectedIndex + 1,
		valid_local_time_types(Types, NextIndex, Count).

	valid_boolean(true).
	valid_boolean(false).

	valid_transitions([], _).
	valid_transitions([transition(Time, TypeIndex)| Transitions], TypeCount) :-
		integer(Time),
		integer(TypeIndex),
		TypeIndex >= 0,
		TypeIndex < TypeCount,
		valid_transition_order(Transitions, Time, TypeCount).

	valid_transition_order([], _, _).
	valid_transition_order([transition(Time, TypeIndex)| Transitions], PreviousTime, TypeCount) :-
		integer(Time),
		Time > PreviousTime,
		integer(TypeIndex),
		TypeIndex >= 0,
		TypeIndex < TypeCount,
		valid_transition_order(Transitions, Time, TypeCount).

	valid_leaps([]).
	valid_leaps([leap_second(Occurrence, Correction)| Leaps]) :-
		integer(Occurrence),
		integer(Correction),
		valid_leap_order(Leaps, Occurrence).

	valid_leap_order([], _).
	valid_leap_order([leap_second(Occurrence, Correction)| Leaps], PreviousOccurrence) :-
		integer(Occurrence),
		Occurrence > PreviousOccurrence,
		integer(Correction),
		valid_leap_order(Leaps, Occurrence).

	valid_indicator_list([], _, _) :-
		!.
	valid_indicator_list(Indicators, TypeCount, standard_wall) :-
		!,
		length(Indicators, TypeCount),
		valid_standard_walls(Indicators).
	valid_indicator_list(Indicators, TypeCount, ut_local) :-
		length(Indicators, TypeCount),
		valid_ut_locals(Indicators).

	valid_standard_walls([]).
	valid_standard_walls([Indicator| Indicators]) :-
		valid_standard_wall(Indicator),
		valid_standard_walls(Indicators).

	valid_standard_wall(standard).
	valid_standard_wall(wall).

	valid_ut_locals([]).
	valid_ut_locals([Indicator| Indicators]) :-
		valid_ut_local(Indicator),
		valid_ut_locals(Indicators).

	valid_ut_local(ut).
	valid_ut_local(local).

	valid_footer_term(empty).
	valid_footer_term(posix(StandardAbbreviation, StandardOffset, none, none, none, none)) :-
		!,
		atom(StandardAbbreviation),
		integer(StandardOffset).
	valid_footer_term(posix(StandardAbbreviation, StandardOffset, DSTAbbreviation, DSTOffset, StartRule, EndRule)) :-
		atom(StandardAbbreviation),
		integer(StandardOffset),
		atom(DSTAbbreviation),
		integer(DSTOffset),
		valid_rule_term(StartRule),
		valid_rule_term(EndRule).

	valid_rule_term(permanent).
	valid_rule_term(rule(Form, at(Seconds, Suffix))) :-
		valid_rule_form(Form),
		integer(Seconds),
		valid_rule_suffix(Suffix).

	valid_rule_suffix(wall).
	valid_rule_suffix(standard).
	valid_rule_suffix(utc).

	valid_rule_form(julian_no_leap(Day)) :-
		integer(Day),
		Day >= 1.
	valid_rule_form(day_of_year(Day)) :-
		integer(Day),
		Day >= 0.
	valid_rule_form(month_weekday(Month, Week, WeekdayNumber)) :-
		integer(Month),
		Month >= 1,
		Month =< 12,
		integer(Week),
		Week >= 1,
		Week =< 5,
		integer(WeekdayNumber),
		WeekdayNumber >= 0,
		WeekdayNumber =< 6.

	load_snapshot_terms(File, TZifs) :-
		open_snapshot_source(File, Stream),
		catch(
			read_snapshot_terms(Stream, TZifs0),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream),
		normalize_tzif_list(TZifs0, TZifs).

	read_snapshot_terms(Stream, TZifs) :-
		read_term(Stream, Term, []),
		( Term == end_of_file ->
			TZifs = []
		; check_tzif_term(Term),
			TZifs = [Term| Rest],
			read_snapshot_terms(Stream, Rest)
		).

	save_snapshot_terms([], _).
	save_snapshot_terms([TZif| TZifs], Stream) :-
		write_term(Stream, TZif, [quoted(true)]),
		write(Stream, '.'),
		nl(Stream),
		save_snapshot_terms(TZifs, Stream).

	read_binary_source(File, Bytes) :-
		open_binary_source(File, Stream),
		catch(
			stream_to_bytes(Stream, Bytes),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream).

	open_binary_source(File, Stream) :-
		(	atom(File) ->
			catch(open(File, read, Stream, [type(binary)]), Error, handle_input_open_error(File, Error, Stream, binary))
		;	expand_library_path(File, Expanded) ->
			catch(open(Expanded, read, Stream, [type(binary)]), Error, rethrow_open_error(File, Error))
		;	existence_error(source_sink, File)
		).

	open_snapshot_source(File, Stream) :-
		(	atom(File) ->
			catch(open(File, read, Stream), Error, handle_input_open_error(File, Error, Stream, text))
		;	expand_library_path(File, Expanded) ->
			catch(open(Expanded, read, Stream), Error, rethrow_open_error(File, Error))
		;	existence_error(source_sink, File)
		).

	handle_input_open_error(File, error(existence_error(source_sink, _), _), Stream, Type) :-
		!,
		(	expand_library_path(tzif(File), Expanded) ->
			open_expanded_file(Type, Expanded, File, Stream)
		;	existence_error(source_sink, File)
		).
	handle_input_open_error(File, Error, _, _) :-
		rethrow_open_error(File, Error).

	open_expanded_file(binary, Expanded, File, Stream) :-
		catch(open(Expanded, read, Stream, [type(binary)]), Error, rethrow_open_error(File, Error)).
	open_expanded_file(text, Expanded, File, Stream) :-
		catch(open(Expanded, read, Stream), Error, rethrow_open_error(File, Error)).

	rethrow_open_error(File, error(existence_error(source_sink, _), _)) :-
		!,
		existence_error(source_sink, File).
	rethrow_open_error(_, Error) :-
		throw(Error).

	check_output_file(File) :-
		(	var(File) ->
			instantiation_error
		;	atom(File) ->
			true
		;	domain_error(file_name, File)
		).

	all_zero_bytes([]).
	all_zero_bytes([0| Bytes]) :-
		all_zero_bytes(Bytes).

	read_u32(Bytes, Value, Rest) :-
		take(4, Bytes, ValueBytes, Rest),
		unsigned_integer(ValueBytes, Value).

	read_s32(Bytes, Value, Rest) :-
		take(4, Bytes, ValueBytes, Rest),
		signed_integer(ValueBytes, Value).

	unsigned_integer(Bytes, Value) :-
		unsigned_integer(Bytes, 0, Value).

	unsigned_integer([], Value, Value).
	unsigned_integer([Byte| Bytes], Accumulator, Value) :-
		NextAccumulator is Accumulator * 256 + Byte,
		unsigned_integer(Bytes, NextAccumulator, Value).

	signed_integer(Bytes, Value) :-
		unsigned_integer(Bytes, UnsignedValue),
		length(Bytes, Length),
		Bits is Length * 8,
		SignBit is 1 << (Bits - 1),
		(	UnsignedValue >= SignBit ->
			Value is UnsignedValue - (1 << Bits)
		;	Value = UnsignedValue
		).

:- end_object.
