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


`dates`
=======

The `date` object implements some useful calendar date predicates.

It also provides portable predicates for date-time handling, including:

- Unix epoch conversions (`date_time_to_unix/2`, `unix_to_date_time/2`)
- date-time arithmetic with durations (`add_duration/3`, `subtract_duration/3`, `duration_between/3`)
- UTC/local conversion using explicit offsets (`utc_to_local/3`, `local_to_utc/3`)
- date-time formatting with explicit offsets (`format_date_time/4`)
- calendar utilities (`day_of_year/2`, `week_of_year_iso/2`, `weekday/2`)
- reverse and positional calendar utilities (`day_of_year_date/3`, `month_weekday_date/5`)
- date-time normalization and validation (`normalize_date_time/2`, `valid_date_time/1`)
- date-time comparison (`before/2`, `after/2`, `same_instant/2`, `compare_date_time/3`)

The duration predicates accept two duration representations:

- `duration(Days, Hours, Minutes, Seconds)` — a fixed-length duration converted to seconds
- `duration(Years, Months, Days, Hours, Minutes, Seconds)` — a calendar-aware period where the year and month delta is applied first using calendar arithmetic, clamping the day to the last valid day of the resulting month when necessary (e.g. adding one month to January 31 gives February 28 or 29 depending on the year), with the remaining day and time delta applied via fixed-length arithmetic. The `duration_between/3` predicate returns this form when called with a 6-arity skeleton such as `duration(Yr, Mo, Da, Hr, Mi, Se)`.

Date-time values are represented using the `date_time/6` compound term:

- `date_time(Year, Month, Day, Hours, Minutes, Seconds)`

The `format_date_time/4` predicate currently supports the named formats
`rfc3339`, `iso8601`, `atom`, `rfc2822`, `rfc5322`, `rss`, `http_date`,
`rfc1123`, `unix_date`, `common_log`, `date_short`, `date_medium`,
`date_long`, `date_full`, `time_short`, `time_medium`, `time_long`,
`time_full`, `date_time_short`, `date_time_medium`, `date_time_long`,
and `date_time_full`.

The explicit offset argument is given in seconds. Formats that include
numeric offsets require offsets representable in whole minutes. The
`http_date` and `rfc1123` formats always normalize the output to `GMT`.

The style presets are currently English-only presentation formats:

- `date_short` -> `2026-04-08`
- `date_medium` -> `8 Apr 2026`
- `date_long` -> `April 8, 2026`
- `date_full` -> `Tuesday, April 8, 2026`
- `time_short` -> `13:45`
- `time_medium` -> `13:45:30`
- `time_long` -> `13:45:30 +01:00`
- `time_full` -> `1:45:30 PM +01:00`
- `date_time_short` -> `2026-04-08 13:45`
- `date_time_medium` -> `8 Apr 2026 13:45:30`
- `date_time_long` -> `April 8, 2026 1:45:30 PM +01:00`
- `date_time_full` -> `Tuesday, April 8, 2026 1:45:30 PM +01:00`

The `time` object implements some useful time predicates.

Please note that the functionality of these objects depends on the chosen
Prolog support for accessing the operating system time and date.


API documentation
-----------------

Open the [../../apis/library_index.html#dates](../../apis/library_index.html#dates)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(dates(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(dates(tester)).
