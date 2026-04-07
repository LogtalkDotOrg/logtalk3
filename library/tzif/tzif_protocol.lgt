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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-07,
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
		argnames is ['File']
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
		argnames is ['Zones']
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
		argnames is ['Zone', 'UTC', 'TimeType']
	]).

	:- public(time_type/2).
	:- mode(time_type(+types([integer, compound]), -compound), one_or_error).
	:- info(time_type/2, [
		comment is 'Cached single-zone convenience variant of ``time_type/3`` using the cached TZif terms; requires exactly one cached zone.',
		argnames is ['UTC', 'TimeType']
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
		argnames is ['Zone', 'UTC', 'OffsetSeconds']
	]).

	:- public(offset/2).
	:- mode(offset(+types([integer, compound]), -integer), one_or_error).
	:- info(offset/2, [
		comment is 'Cached single-zone convenience variant of ``offset/3`` using the cached TZif terms; requires exactly one cached zone.',
		argnames is ['UTC', 'OffsetSeconds']
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
		argnames is ['Zone', 'UTC', 'IsDST']
	]).

	:- public(daylight_saving_time/2).
	:- mode(daylight_saving_time(+types([integer, compound]), -atom), one_or_error).
	:- info(daylight_saving_time/2, [
		comment is 'Cached single-zone convenience variant of ``daylight_saving_time/3`` using the cached TZif terms; requires exactly one cached zone.',
		argnames is ['UTC', 'IsDST']
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
		argnames is ['Zone', 'UTC', 'Abbreviation']
	]).

	:- public(abbreviation/2).
	:- mode(abbreviation(+types([integer, compound]), -atom), one_or_error).
	:- info(abbreviation/2, [
		comment is 'Cached single-zone convenience variant of ``abbreviation/3`` using the cached TZif terms; requires exactly one cached zone.',
		argnames is ['UTC', 'Abbreviation']
	]).

:- end_protocol.
