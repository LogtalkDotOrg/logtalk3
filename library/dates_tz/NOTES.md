________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


``dates_tz``
============

Bridge library linking the ``dates`` and ``tzif`` libraries for zone-aware
date-time conversions. Provides predicates that convert UTC date-times to
civil local date-times and vice-versa, handling DST transitions including
ambiguous (fold) and non-existent (gap) times. Also provides cross-zone
conversion that routes through UTC as an intermediate representation.

This library requires unbounded integer arithmetic support from the backend
Prolog compiler (as does the ``tzif`` library it depends on). It is not
available on backends where the ``bounded`` Prolog flag is ``true``.


API documentation
-----------------

Open the [../../apis/library_index.html#dates_tz](../../apis/library_index.html#dates_tz)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(dates_tz(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(dates_tz(tester)).


Usage
-----

Zone data must be loaded separately into the ``tzif`` cache before calling
any ``dates_tz`` predicates. For example:

    :- initialization((
        logtalk_load(dates_tz(loader)),
        tzif::load(file('/usr/share/zoneinfo/America/New_York', 'America/New_York')),
        tzif::load(directory('/usr/share/zoneinfo'))
    )).


Examples
--------

Convert a UTC ``date_time/6`` compound to the civil local date-time in the
named IANA zone. The UTC instant always has exactly one local representation.

    |?- dates_tz::utc_to_local_tz(date_time(2024,1,15,12,0,0), 'America/New_York', Local).
    Local = date_time(2024,1,15,7,0,0).

Convert a civil local ``date_time/6`` compound in the named zone to UTC using
strict interpretation. Fails silently if the local time falls in a DST gap
(non-existent) or a DST fold (ambiguous). Throws an error if the zone is not
cached.

    | ?- dates_tz::local_to_utc_tz(date_time(2024,1,15,7,0,0), 'America/New_York', UTC).
    UTC = date_time(2024,1,15,12,0,0).

Convert a civil local date-time to UTC with an explicit resolution mode. The mode can be:

- ``strict`` — fail unless exactly one interpretation exists
- ``first`` — prefer the earliest valid interpretation (earliest UTC instant)
- ``second`` — prefer the latest valid interpretation (latest UTC instant)
- ``all`` — enumerate all valid interpretations in chronological order (non-deterministic)

Fall-back fold in New York: 2024-11-03 01:30 occurs twice:

    | ?- dates_tz::local_to_utc_tz_with_resolution(
         date_time(2024,11,3,1,30,0), 'America/New_York', first, UTC).
    UTC = date_time(2024,11,3,5,30,0).  % EDT interpretation

    | ?- dates_tz::local_to_utc_tz_with_resolution(
         date_time(2024,11,3,1,30,0), 'America/New_York', second, UTC).
    UTC = date_time(2024,11,3,6,30,0).  % EST interpretation

Convert a civil local date-time from one IANA zone to another, routing through
UTC. Uses strict interpretation for the source zone: fails if the source local
time is ambiguous or non-existent. Requires both zones to be cached.

    | ?- dates_tz::convert_zones(
         date_time(2024,1,15,7,0,0), 'America/New_York', 'Asia/Kathmandu', Result).
    Result = date_time(2024,1,15,17,45,0).

Convert a civil local date-time between zones with an explicit resolution mode
applied to the source zone. Useful when the source local time may be ambiguous
due to a DST transition.

    | ?- dates_tz::convert_zones_with_resolution(
         date_time(2024,11,3,1,30,0), 'America/New_York', first, 'Asia/Kathmandu', Result).
    Result = date_time(2024,11,3,11,15,0).


Notes
-----

- Date-time values are represented as ``date_time(Year, Month, Day, Hour, Minute, Second)``
compounds, consistent with the ``dates`` library.

- UTC offset arithmetic is performed via integer seconds using ``date::add_duration/3``
and ``date::subtract_duration/3``.

- This library does not provide a system clock or current-time predicate. Use
the ``dates`` library's ``date::now/6`` or equivalent for the current time.

- For performance when converting many instants in the same zone, load all
required zones once at startup and keep them in the ``tzif`` cache.
