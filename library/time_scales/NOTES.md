________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


`time_scales`
=============

The `time_scales` library provides predicates for converting instants between
UTC, TAI, TT, UT1, TDB, GPS, GST, TCG, and TCB using bundled reference data and optional
user-provided override files.

The library is designed to complement (not replace) the `dates` and `iso8601`
libraries:

- use `dates` for civil date-time arithmetic and Unix epoch conversions;
- use `iso8601` for string parsing and formatting;
- use `time_scales` for physical time-scale conversions.


Features
--------

Current feature set:

- supported scales: `utc`, `tai`, `tt`, `ut1`, `tdb`, `gps`, `gst`, `tcg`, and `tcb`;
- UTC support starts at `1972-01-01T00:00:00Z`;
- leap seconds are provided by a bundled static table with optional override file;
- DUT1 (`UT1-UTC`) is provided by bundled data with optional override file;
- active leap-second and DUT1 tables can be queried as ordered term lists for reproducibility;
- active leap-second and DUT1 tables can be saved to deterministic term files and reloaded as overrides;
- fail-based validation helper predicates are available for checking instant terms and conversion requests;
- strict `check_*` predicates are available and throw typed errors for invalid arguments;
- TDB conversions use a practical TT/TDB approximation suitable for application-level use;
- real-valued approximation offsets are converted to rationals using high-resolution nanosecond scaling.


Limitations
-----------

Current non-implemented features:

- UTC coverage before 1972-01-01T00:00:00Z is not implemented;
- network-driven leap-second and DUT1 data updates are not implemented;
- high-precision ephemeris-based relativistic modeling (beyond the current practical approximation formulas) is not implemented.

For full functionality (including all high-precision conversion paths and their corresponding tests), a Prolog backend with support for unbounded integer arithmetic is required.


Representation
--------------

Instants are represented as:

    instant(Scale, Seconds, fraction(Numerator, Denominator))

Where:

- `Scale` is one of `utc`, `tai`, `tt`, `ut1`, `tdb`, `gps`, `gst`, `tcg`, or `tcb`;
- `Seconds` is integer epoch seconds;
- `fraction(Numerator, Denominator)` is a normalized fraction in the `[0,1[` interval.

Offsets are returned as:

    rational(Numerator, Denominator)


Examples
--------

Convert UTC to TAI:

    | ?- time_scales::convert(instant(utc, 1483228800, fraction(0,1)), utc, tai, TAI).

Strictly validate an instant term (throws on invalid input):

    | ?- time_scales::check_instant(instant(utc, 1483228800, fraction(0,1))).

Strict conversion (throws on invalid scale or mismatched instant scale):

    | ?- time_scales::check_convert(instant(utc, 1483228800, fraction(0,1)), utc, tai, TAI).

Convert TAI to TT:

    | ?- time_scales::convert(instant(tai, 1483228837, fraction(0,1)), tai, tt, TT).

Convert UTC to UT1:

    | ?- time_scales::convert(instant(utc, 1483228800, fraction(0,1)), utc, ut1, UT1).

Convert TT to TDB:

    | ?- time_scales::convert(instant(tt, 1483228869, fraction(0,1)), tt, tdb, TDB).

Convert UTC to GPS:

    | ?- time_scales::convert(instant(utc, 1483228800, fraction(0,1)), utc, gps, GPS).

Convert UTC to GST:

    | ?- time_scales::convert(instant(utc, 1483228800, fraction(0,1)), utc, gst, GST).

Convert UTC to TCG:

    | ?- time_scales::convert(instant(utc, 1483228800, fraction(0,1)), utc, tcg, TCG).

Convert UTC to TCB:

    | ?- time_scales::convert(instant(utc, 1483228800, fraction(0,1)), utc, tcb, TCB).

Lookup an effective leap-second date:

    | ?- time_scales::leap_second_date(Date, Offset).

Load leap-second and DUT1 override files:

    | ?- time_scales::load_leap_seconds_override('test_files/leap_seconds_override.pl').

    | ?- time_scales::load_dut1_override('test_files/dut1_override.pl').

Query active leap-second and DUT1 data tables:

    | ?- time_scales::leap_seconds_entries(LeapEntries).

    | ?- time_scales::dut1_entries(DUT1Entries).

Save active leap-second and DUT1 data tables:

    | ?- time_scales::save_leap_seconds_entries('/tmp/logtalk_time_scales_leap_snapshot.pl').

    | ?- time_scales::save_dut1_entries('/tmp/logtalk_time_scales_dut1_snapshot.pl').

Validate an instant or conversion request:

    | ?- time_scales::valid_instant(instant(utc, 1483228800, fraction(0,1))).

    | ?- time_scales::valid_conversion(instant(tai, 1483228837, fraction(0,1)), tai, tdb).


API documentation
-----------------

Open the [../../apis/library_index.html#time_scales](../../apis/library_index.html#time_scales)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(time_scales(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(time_scales(tester)).
