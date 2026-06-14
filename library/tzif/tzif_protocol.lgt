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


:- protocol(tzif_protocol).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-06-14,
		comment is 'Protocol for loading TZif data sets, persisting loaded terms, and answering zone-aware UTC-based offset, DST, and abbreviation queries.'
	]).

	:- public(load/1).
	:- mode(load(+compound), zero_or_one).
	:- info(load/1, [
		comment is 'Loads a TZif source given as ``file(Path, ZoneId)``, ``files(Root, Paths)``, ``directory(Root)``, ``stream(Stream, ZoneId)``, ``bytes(Bytes, ZoneId)``, or ``snapshot(File)`` and caches the resulting per-zone ``tzif(...)`` terms, replacing cached entries with matching zone identifiers. For ``directory(Root)`` sources, regular files whose relative paths are not recognized zone identifiers are ignored, allowing system zoneinfo trees that contain metadata files such as ``leapseconds``. Zone identifiers are validated against bundled IANA TZDB 2026a canonical names plus backward-compatible aliases.',
		argnames is ['Source']
	]).

	:- public(load/2).
	:- mode(load(+compound, -list(compound)), zero_or_one).
	:- info(load/2, [
		comment is 'Loads a TZif source given as ``file(Path, ZoneId)``, ``files(Root, Paths)``, ``directory(Root)``, ``stream(Stream, ZoneId)``, ``bytes(Bytes, ZoneId)``, or ``snapshot(File)`` into a list of per-zone ``tzif(...)`` compound terms without caching them. For ``directory(Root)`` sources, regular files whose relative paths are not recognized zone identifiers are ignored, allowing system zoneinfo trees that contain metadata files such as ``leapseconds``. Zone identifiers are validated against bundled IANA TZDB 2026a canonical names plus backward-compatible aliases.',
		argnames is ['Source', 'TZifs']
	]).

	:- public(cache/1).
	:- mode(cache(+list(compound)), one).
	:- info(cache/1, [
		comment is 'Caches one or more loaded per-zone ``tzif(...)`` compound terms, replacing cached entries with matching zone identifiers.',
		argnames is ['TZifs']
	]).

	:- public(save/2).
	:- mode(save(+list(compound), +atom), one).
	:- info(save/2, [
		comment is 'Saves a list of per-zone ``tzif(...)`` terms to a plain Prolog snapshot file, writing one serialized term per line.',
		argnames is ['TZifs', 'File']
	]).

	:- public(save/1).
	:- mode(save(+atom), one_or_error).
	:- info(save/1, [
		comment is 'Saves all cached ``tzif(...)`` terms to a plain Prolog snapshot file.',
		argnames is ['File'],
		exceptions is [
			'No TZif terms are cached' - existence_error(tzif_cache, tzif),
			'``File`` is a variable' - instantiation_error,
			'``File`` is neither a variable nor a valid file name atom' - domain_error(file_name, 'File')
		]
	]).

	:- public(clear_cache/0).
	:- mode(clear_cache, one).
	:- info(clear_cache/0, [
		comment is 'Clears all cached TZif terms.'
	]).

	:- public(cache_source/1).
	:- mode(cache_source(-compound), zero_or_more).
	:- info(cache_source/1, [
		comment is 'Enumerates the source terms recorded in the cached TZif terms.',
		argnames is ['Source']
	]).

	:- public(cached_tzif/1).
	:- mode(cached_tzif(-compound), zero_or_more).
	:- info(cached_tzif/1, [
		comment is 'Enumerates the cached per-zone ``tzif(...)`` terms.',
		argnames is ['TZif']
	]).

	:- public(zone/3).
	:- mode(zone(+compound, ?atom, -compound), zero_or_one).
	:- info(zone/3, [
		comment is 'Returns the loaded zone identifier and its nested parsed zone-data term from a per-zone ``tzif(...)`` term.',
		argnames is ['TZif', 'Zone', 'ZoneData']
	]).

	:- public(zones/2).
	:- mode(zones(+list(compound), -list(atom)), one).
	:- info(zones/2, [
		comment is 'Returns the list of zone identifiers loaded in a list of per-zone ``tzif(...)`` terms.',
		argnames is ['TZifs', 'Zones']
	]).

	:- public(zones/1).
	:- mode(zones(-list(atom)), one_or_error).
	:- info(zones/1, [
		comment is 'Returns the list of zone identifiers loaded in the cached ``tzif(...)`` terms.',
		argnames is ['Zones'],
		exceptions is [
			'No TZif terms are cached' - existence_error(tzif_cache, tzif)
		]
	]).

	:- public(time_type/4).
	:- mode(time_type(+compound, +atom, +types([integer, compound]), -compound), zero_or_one).
	:- info(time_type/4, [
		comment is 'Returns the applicable local time type for a loaded zone and a UTC instant given either as Unix seconds or as a ``date_time/6`` term.',
		argnames is ['TZif', 'Zone', 'UTC', 'TimeType']
	]).

	:- public(time_type/3).
	:- mode(time_type(+atom, +types([integer, compound]), -compound), one_or_error).
	:- info(time_type/3, [
		comment is 'Returns the applicable local time type for a zone in the cached TZif terms.',
		argnames is ['Zone', 'UTC', 'TimeType'],
		exceptions is [
			'``Zone`` is not present in the cached TZif terms' - existence_error(time_zone, 'Zone')
		]
	]).

	:- public(time_type/2).
	:- mode(time_type(+types([integer, compound]), -compound), one_or_error).
	:- info(time_type/2, [
		comment is 'Cached single-zone convenience variant of ``time_type/3`` using the cached TZif terms; requires exactly one cached zone.',
		argnames is ['UTC', 'TimeType'],
		exceptions is [
			'No TZif terms are cached' - existence_error(tzif_cache, tzif),
			'The cached TZif terms do not contain exactly one zone' - domain_error(single_zone_tzif_cache, 'TZifs')
		]
	]).

	:- public(offset/4).
	:- mode(offset(+compound, +atom, +types([integer, compound]), -integer), zero_or_one).
	:- info(offset/4, [
		comment is 'Returns the UTC offset in seconds for a loaded zone and a UTC instant.',
		argnames is ['TZif', 'Zone', 'UTC', 'OffsetSeconds']
	]).

	:- public(offset/3).
	:- mode(offset(+atom, +types([integer, compound]), -integer), one_or_error).
	:- info(offset/3, [
		comment is 'Returns the UTC offset in seconds for a zone in the cached TZif terms.',
		argnames is ['Zone', 'UTC', 'OffsetSeconds'],
		exceptions is [
			'Any exception that can be thrown by ``time_type/3`` for the given zone and UTC instant' - error
		]
	]).

	:- public(offset/2).
	:- mode(offset(+types([integer, compound]), -integer), one_or_error).
	:- info(offset/2, [
		comment is 'Cached single-zone convenience variant of ``offset/3`` using the cached TZif terms; requires exactly one cached zone.',
		argnames is ['UTC', 'OffsetSeconds'],
		exceptions is [
			'Any exception that can be thrown by ``time_type/2`` for the given UTC instant' - error
		]
	]).

	:- public(daylight_saving_time/4).
	:- mode(daylight_saving_time(+compound, +atom, +types([integer, compound]), -atom), zero_or_one).
	:- info(daylight_saving_time/4, [
		comment is 'Returns ``true`` or ``false`` according to whether daylight saving time is active for a loaded zone and a UTC instant.',
		argnames is ['TZif', 'Zone', 'UTC', 'IsDST']
	]).

	:- public(daylight_saving_time/3).
	:- mode(daylight_saving_time(+atom, +types([integer, compound]), -atom), one_or_error).
	:- info(daylight_saving_time/3, [
		comment is 'Returns daylight-saving information for a zone in the cached TZif terms.',
		argnames is ['Zone', 'UTC', 'IsDST'],
		exceptions is [
			'Any exception that can be thrown by ``time_type/3`` for the given zone and UTC instant' - error
		]
	]).

	:- public(daylight_saving_time/2).
	:- mode(daylight_saving_time(+types([integer, compound]), -atom), one_or_error).
	:- info(daylight_saving_time/2, [
		comment is 'Cached single-zone convenience variant of ``daylight_saving_time/3`` using the cached TZif terms; requires exactly one cached zone.',
		argnames is ['UTC', 'IsDST'],
		exceptions is [
			'Any exception that can be thrown by ``time_type/2`` for the given UTC instant' - error
		]
	]).

	:- public(abbreviation/4).
	:- mode(abbreviation(+compound, +atom, +types([integer, compound]), -atom), zero_or_one).
	:- info(abbreviation/4, [
		comment is 'Returns the time-zone abbreviation for a loaded zone and a UTC instant.',
		argnames is ['TZif', 'Zone', 'UTC', 'Abbreviation']
	]).

	:- public(abbreviation/3).
	:- mode(abbreviation(+atom, +types([integer, compound]), -atom), one_or_error).
	:- info(abbreviation/3, [
		comment is 'Returns the time-zone abbreviation for a zone in the cached TZif terms.',
		argnames is ['Zone', 'UTC', 'Abbreviation'],
		exceptions is [
			'Any exception that can be thrown by ``time_type/3`` for the given zone and UTC instant' - error
		]
	]).

	:- public(abbreviation/2).
	:- mode(abbreviation(+types([integer, compound]), -atom), one_or_error).
	:- info(abbreviation/2, [
		comment is 'Cached single-zone convenience variant of ``abbreviation/3`` using the cached TZif terms; requires exactly one cached zone.',
		argnames is ['UTC', 'Abbreviation'],
		exceptions is [
			'Any exception that can be thrown by ``time_type/2`` for the given UTC instant' - error
		]
	]).

	:- public(local_time_type/4).
	:- mode(local_time_type(+compound, +atom, +compound, -compound), zero_or_one).
	:- info(local_time_type/4, [
		comment is 'Returns the applicable local time type for a loaded zone and a local civil time given as a ``date_time/6`` term. This strict variant fails unless the local civil time has a unique interpretation.',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'TimeType']
	]).

	:- public(local_time_type/3).
	:- mode(local_time_type(+atom, +compound, -compound), one_or_error).
	:- info(local_time_type/3, [
		comment is 'Returns the applicable local time type for a zone in the cached TZif terms. This strict variant fails unless the local civil time has a unique interpretation.',
		argnames is ['Zone', 'LocalDateTime', 'TimeType'],
		exceptions is [
			'``LocalDateTime`` is a variable' - instantiation_error,
			'``LocalDateTime`` is neither a variable nor a valid ``date_time/6`` term' - type_error(date_time, 'LocalDateTime'),
			'``Zone`` is not present in the cached TZif terms' - existence_error(time_zone, 'Zone')
		]
	]).

	:- public(local_time_type/2).
	:- mode(local_time_type(+compound, -compound), one_or_error).
	:- info(local_time_type/2, [
		comment is 'Cached single-zone convenience variant of strict local civil-time lookup; requires exactly one cached zone and fails unless the local civil time has a unique interpretation.',
		argnames is ['LocalDateTime', 'TimeType'],
		exceptions is [
			'``LocalDateTime`` is a variable' - instantiation_error,
			'``LocalDateTime`` is neither a variable nor a valid ``date_time/6`` term' - type_error(date_time, 'LocalDateTime'),
			'No TZif terms are cached' - existence_error(tzif_cache, tzif),
			'The cached TZif terms do not contain exactly one zone' - domain_error(single_zone_tzif_cache, 'TZifs')
		]
	]).

	:- public(local_time_type_with_resolution/5).
	:- mode(local_time_type_with_resolution(+compound, +atom, +compound, +atom, -compound), zero_or_more).
	:- info(local_time_type_with_resolution/5, [
		comment is 'Returns the applicable local time type for a loaded zone and a local civil time using the explicit resolution mode: ``strict`` (fail unless exactly one interpretation exists), ``first`` (prefer the earliest valid interpretation), ``second`` (prefer the latest valid interpretation), and ``all`` (enumerate all valid interpretations in chronological order).',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'ResolutionMode', 'TimeType']
	]).

	:- public(local_time_type_with_resolution/4).
	:- mode(local_time_type_with_resolution(+atom, +compound, +atom, -compound), zero_or_more).
	:- info(local_time_type_with_resolution/4, [
		comment is 'Returns the applicable local time type for a zone in the cached TZif terms using the explicit resolution mode: ``strict`` (fail unless exactly one interpretation exists), ``first`` (prefer the earliest valid interpretation), ``second`` (prefer the latest valid interpretation), and ``all`` (enumerate all valid interpretations in chronological order).',
		argnames is ['Zone', 'LocalDateTime', 'ResolutionMode', 'TimeType']
	]).

	:- public(local_time_type_with_resolution/3).
	:- mode(local_time_type_with_resolution(+compound, +atom, -compound), zero_or_more).
	:- info(local_time_type_with_resolution/3, [
		comment is 'Cached single-zone convenience variant of local civil-time lookup using the explicit resolution mode: ``strict`` (fail unless exactly one interpretation exists), ``first`` (prefer the earliest valid interpretation), ``second`` (prefer the latest valid interpretation), and ``all`` (enumerate all valid interpretations in chronological order). Requires exactly one cached zone.',
		argnames is ['LocalDateTime', 'ResolutionMode', 'TimeType']
	]).

	:- public(local_offset/4).
	:- mode(local_offset(+compound, +atom, +compound, -integer), zero_or_one).
	:- info(local_offset/4, [
		comment is 'Returns the UTC offset in seconds for a loaded zone and a local civil time. This strict variant fails unless the local civil time has a unique interpretation.',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'OffsetSeconds']
	]).

	:- public(local_offset/3).
	:- mode(local_offset(+atom, +compound, -integer), one_or_error).
	:- info(local_offset/3, [
		comment is 'Returns the UTC offset in seconds for a zone in the cached TZif terms. This strict variant fails unless the local civil time has a unique interpretation.',
		argnames is ['Zone', 'LocalDateTime', 'OffsetSeconds'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type/3`` for the given zone and local civil time' - error
		]
	]).

	:- public(local_offset/2).
	:- mode(local_offset(+compound, -integer), one_or_error).
	:- info(local_offset/2, [
		comment is 'Cached single-zone convenience variant of strict local civil-time offset lookup.',
		argnames is ['LocalDateTime', 'OffsetSeconds'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type/2`` for the given local civil time' - error
		]
	]).

	:- public(local_offset_with_resolution/5).
	:- mode(local_offset_with_resolution(+compound, +atom, +compound, +atom, -integer), zero_or_more).
	:- info(local_offset_with_resolution/5, [
		comment is 'Returns the UTC offset in seconds for a loaded zone and a local civil time using the explicit resolution mode: ``strict`` (fail unless exactly one interpretation exists), ``first`` (prefer the earliest valid interpretation), ``second`` (prefer the latest valid interpretation), and ``all`` (enumerate all valid interpretations in chronological order).',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'ResolutionMode', 'OffsetSeconds']
	]).

	:- public(local_offset_with_resolution/4).
	:- mode(local_offset_with_resolution(+atom, +compound, +atom, -integer), zero_or_more).
	:- info(local_offset_with_resolution/4, [
		comment is 'Returns the UTC offset in seconds for a zone in the cached TZif terms using the explicit resolution mode: ``strict`` (fail unless exactly one interpretation exists), ``first`` (prefer the earliest valid interpretation), ``second`` (prefer the latest valid interpretation), and ``all`` (enumerate all valid interpretations in chronological order).',
		argnames is ['Zone', 'LocalDateTime', 'ResolutionMode', 'OffsetSeconds']
	]).

	:- public(local_offset_with_resolution/3).
	:- mode(local_offset_with_resolution(+compound, +atom, -integer), zero_or_more).
	:- info(local_offset_with_resolution/3, [
		comment is 'Cached single-zone convenience variant of local civil-time offset lookup using the explicit resolution mode; requires exactly one cached zone.',
		argnames is ['LocalDateTime', 'ResolutionMode', 'OffsetSeconds']
	]).

	:- public(local_daylight_saving_time/4).
	:- mode(local_daylight_saving_time(+compound, +atom, +compound, -atom), zero_or_one).
	:- info(local_daylight_saving_time/4, [
		comment is 'Returns daylight-saving information for a loaded zone and a local civil time. This strict variant fails unless the local civil time has a unique interpretation.',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'IsDST']
	]).

	:- public(local_daylight_saving_time/3).
	:- mode(local_daylight_saving_time(+atom, +compound, -atom), one_or_error).
	:- info(local_daylight_saving_time/3, [
		comment is 'Returns daylight-saving information for a zone in the cached TZif terms. This strict variant fails unless the local civil time has a unique interpretation.',
		argnames is ['Zone', 'LocalDateTime', 'IsDST'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type/3`` for the given zone and local civil time' - error
		]
	]).

	:- public(local_daylight_saving_time/2).
	:- mode(local_daylight_saving_time(+compound, -atom), one_or_error).
	:- info(local_daylight_saving_time/2, [
		comment is 'Cached single-zone convenience variant of strict local daylight-saving lookup.',
		argnames is ['LocalDateTime', 'IsDST'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type/2`` for the given local civil time' - error
		]
	]).

	:- public(local_daylight_saving_time_with_resolution/5).
	:- mode(local_daylight_saving_time_with_resolution(+compound, +atom, +compound, +atom, -atom), zero_or_more).
	:- info(local_daylight_saving_time_with_resolution/5, [
		comment is 'Returns daylight-saving information for a loaded zone and a local civil time using the explicit resolution mode: ``strict`` (fail unless exactly one interpretation exists), ``first`` (prefer the earliest valid interpretation), ``second`` (prefer the latest valid interpretation), and ``all`` (enumerate all valid interpretations in chronological order).',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'ResolutionMode', 'IsDST']
	]).

	:- public(local_daylight_saving_time_with_resolution/4).
	:- mode(local_daylight_saving_time_with_resolution(+atom, +compound, +atom, -atom), zero_or_more).
	:- info(local_daylight_saving_time_with_resolution/4, [
		comment is 'Returns daylight-saving information for a zone in the cached TZif terms using the explicit resolution mode: ``strict`` (fail unless exactly one interpretation exists), ``first`` (prefer the earliest valid interpretation), ``second`` (prefer the latest valid interpretation), and ``all`` (enumerate all valid interpretations in chronological order).',
		argnames is ['Zone', 'LocalDateTime', 'ResolutionMode', 'IsDST']
	]).

	:- public(local_daylight_saving_time_with_resolution/3).
	:- mode(local_daylight_saving_time_with_resolution(+compound, +atom, -atom), zero_or_more).
	:- info(local_daylight_saving_time_with_resolution/3, [
		comment is 'Cached single-zone convenience variant of local civil-time daylight-saving lookup using the explicit resolution mode; requires exactly one cached zone.',
		argnames is ['LocalDateTime', 'ResolutionMode', 'IsDST']
	]).

	:- public(local_abbreviation/4).
	:- mode(local_abbreviation(+compound, +atom, +compound, -atom), zero_or_one).
	:- info(local_abbreviation/4, [
		comment is 'Returns the time-zone abbreviation for a loaded zone and a local civil time. This strict variant fails unless the local civil time has a unique interpretation.',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'Abbreviation']
	]).

	:- public(local_abbreviation/3).
	:- mode(local_abbreviation(+atom, +compound, -atom), one_or_error).
	:- info(local_abbreviation/3, [
		comment is 'Returns the time-zone abbreviation for a zone in the cached TZif terms. This strict variant fails unless the local civil time has a unique interpretation.',
		argnames is ['Zone', 'LocalDateTime', 'Abbreviation'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type/3`` for the given zone and local civil time' - error
		]
	]).

	:- public(local_abbreviation/2).
	:- mode(local_abbreviation(+compound, -atom), one_or_error).
	:- info(local_abbreviation/2, [
		comment is 'Cached single-zone convenience variant of strict local abbreviation lookup.',
		argnames is ['LocalDateTime', 'Abbreviation'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type/2`` for the given local civil time' - error
		]
	]).

	:- public(local_abbreviation_with_resolution/5).
	:- mode(local_abbreviation_with_resolution(+compound, +atom, +compound, +atom, -atom), zero_or_more).
	:- info(local_abbreviation_with_resolution/5, [
		comment is 'Returns the time-zone abbreviation for a loaded zone and a local civil time using the explicit resolution mode: ``strict`` (fail unless exactly one interpretation exists), ``first`` (prefer the earliest valid interpretation), ``second`` (prefer the latest valid interpretation), and ``all`` (enumerate all valid interpretations in chronological order).',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'ResolutionMode', 'Abbreviation']
	]).

	:- public(local_abbreviation_with_resolution/4).
	:- mode(local_abbreviation_with_resolution(+atom, +compound, +atom, -atom), zero_or_more).
	:- info(local_abbreviation_with_resolution/4, [
		comment is 'Returns the time-zone abbreviation for a zone in the cached TZif terms using the explicit resolution mode: ``strict`` (fail unless exactly one interpretation exists), ``first`` (prefer the earliest valid interpretation), ``second`` (prefer the latest valid interpretation), and ``all`` (enumerate all valid interpretations in chronological order).',
		argnames is ['Zone', 'LocalDateTime', 'ResolutionMode', 'Abbreviation']
	]).

	:- public(local_abbreviation_with_resolution/3).
	:- mode(local_abbreviation_with_resolution(+compound, +atom, -atom), zero_or_more).
	:- info(local_abbreviation_with_resolution/3, [
		comment is 'Cached single-zone convenience variant of local civil-time abbreviation lookup using the explicit resolution mode; requires exactly one cached zone.',
		argnames is ['LocalDateTime', 'ResolutionMode', 'Abbreviation']
	]).

	:- public(local_time_type_reified/4).
	:- mode(local_time_type_reified(+compound, +atom, +compound, -compound), one).
	:- info(local_time_type_reified/4, [
		comment is 'Returns a reified local civil-time lookup result for a loaded zone as one of ``unique(TimeType)``, ``ambiguous(TimeTypes)``, or ``nonexistent``.',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'Result']
	]).

	:- public(local_time_type_reified/3).
	:- mode(local_time_type_reified(+atom, +compound, -compound), one_or_error).
	:- info(local_time_type_reified/3, [
		comment is 'Returns a reified local civil-time lookup result for a zone in the cached TZif terms as one of ``unique(TimeType)``, ``ambiguous(TimeTypes)``, or ``nonexistent``.',
		argnames is ['Zone', 'LocalDateTime', 'Result'],
		exceptions is [
			'``LocalDateTime`` is a variable' - instantiation_error,
			'``LocalDateTime`` is neither a variable nor a valid ``date_time/6`` term' - type_error(date_time, 'LocalDateTime'),
			'``Zone`` is not present in the cached TZif terms' - existence_error(time_zone, 'Zone')
		]
	]).

	:- public(local_time_type_reified/2).
	:- mode(local_time_type_reified(+compound, -compound), one_or_error).
	:- info(local_time_type_reified/2, [
		comment is 'Cached single-zone convenience variant of reified local civil-time lookup; returns ``unique(TimeType)``, ``ambiguous(TimeTypes)``, or ``nonexistent``.',
		argnames is ['LocalDateTime', 'Result'],
		exceptions is [
			'``LocalDateTime`` is a variable' - instantiation_error,
			'``LocalDateTime`` is neither a variable nor a valid ``date_time/6`` term' - type_error(date_time, 'LocalDateTime'),
			'No TZif terms are cached' - existence_error(tzif_cache, tzif),
			'The cached TZif terms do not contain exactly one zone' - domain_error(single_zone_tzif_cache, 'TZifs')
		]
	]).

	:- public(local_offset_reified/4).
	:- mode(local_offset_reified(+compound, +atom, +compound, -compound), one).
	:- info(local_offset_reified/4, [
		comment is 'Returns a reified local offset lookup result for a loaded zone as one of ``unique(OffsetSeconds)``, ``ambiguous(OffsetSecondsList)``, or ``nonexistent``.',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'Result']
	]).

	:- public(local_offset_reified/3).
	:- mode(local_offset_reified(+atom, +compound, -compound), one_or_error).
	:- info(local_offset_reified/3, [
		comment is 'Returns a reified local offset lookup result for a zone in the cached TZif terms as one of ``unique(OffsetSeconds)``, ``ambiguous(OffsetSecondsList)``, or ``nonexistent``.',
		argnames is ['Zone', 'LocalDateTime', 'Result'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type_reified/3`` for the given zone and local civil time' - error
		]
	]).

	:- public(local_offset_reified/2).
	:- mode(local_offset_reified(+compound, -compound), one_or_error).
	:- info(local_offset_reified/2, [
		comment is 'Cached single-zone convenience variant of reified local offset lookup; returns ``unique(OffsetSeconds)``, ``ambiguous(OffsetSecondsList)``, or ``nonexistent``.',
		argnames is ['LocalDateTime', 'Result'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type_reified/2`` for the given local civil time' - error
		]
	]).

	:- public(local_daylight_saving_time_reified/4).
	:- mode(local_daylight_saving_time_reified(+compound, +atom, +compound, -compound), one).
	:- info(local_daylight_saving_time_reified/4, [
		comment is 'Returns a reified local daylight-saving lookup result for a loaded zone as one of ``unique(IsDST)``, ``ambiguous(IsDSTList)``, or ``nonexistent``.',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'Result']
	]).

	:- public(local_daylight_saving_time_reified/3).
	:- mode(local_daylight_saving_time_reified(+atom, +compound, -compound), one_or_error).
	:- info(local_daylight_saving_time_reified/3, [
		comment is 'Returns a reified local daylight-saving lookup result for a zone in the cached TZif terms as one of ``unique(IsDST)``, ``ambiguous(IsDSTList)``, or ``nonexistent``.',
		argnames is ['Zone', 'LocalDateTime', 'Result'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type_reified/3`` for the given zone and local civil time' - error
		]
	]).

	:- public(local_daylight_saving_time_reified/2).
	:- mode(local_daylight_saving_time_reified(+compound, -compound), one_or_error).
	:- info(local_daylight_saving_time_reified/2, [
		comment is 'Cached single-zone convenience variant of reified local daylight-saving lookup; returns ``unique(IsDST)``, ``ambiguous(IsDSTList)``, or ``nonexistent``.',
		argnames is ['LocalDateTime', 'Result'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type_reified/2`` for the given local civil time' - error
		]
	]).

	:- public(local_abbreviation_reified/4).
	:- mode(local_abbreviation_reified(+compound, +atom, +compound, -compound), one).
	:- info(local_abbreviation_reified/4, [
		comment is 'Returns a reified local abbreviation lookup result for a loaded zone as one of ``unique(Abbreviation)``, ``ambiguous(Abbreviations)``, or ``nonexistent``.',
		argnames is ['TZif', 'Zone', 'LocalDateTime', 'Result']
	]).

	:- public(local_abbreviation_reified/3).
	:- mode(local_abbreviation_reified(+atom, +compound, -compound), one_or_error).
	:- info(local_abbreviation_reified/3, [
		comment is 'Returns a reified local abbreviation lookup result for a zone in the cached TZif terms as one of ``unique(Abbreviation)``, ``ambiguous(Abbreviations)``, or ``nonexistent``.',
		argnames is ['Zone', 'LocalDateTime', 'Result'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type_reified/3`` for the given zone and local civil time' - error
		]
	]).

	:- public(local_abbreviation_reified/2).
	:- mode(local_abbreviation_reified(+compound, -compound), one_or_error).
	:- info(local_abbreviation_reified/2, [
		comment is 'Cached single-zone convenience variant of reified local abbreviation lookup; returns ``unique(Abbreviation)``, ``ambiguous(Abbreviations)``, or ``nonexistent``.',
		argnames is ['LocalDateTime', 'Result'],
		exceptions is [
			'Any exception that can be thrown by ``local_time_type_reified/2`` for the given local civil time' - error
		]
	]).

:- end_protocol.
