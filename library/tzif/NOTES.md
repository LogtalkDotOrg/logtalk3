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


`tzif`
======

The `tzif` library loads TZif v1, v2, and v3 files from the IANA time zone
database into inspectable terms and answers UTC-based lookup queries over one
or more zones.

**Requires a backend Prolog compiler with unbounded integer arithmetic.**

Current feature set:

- Loads sources given as `file(Path, ZoneId)`, `files(Root, Paths)`,
  `directory(Root)`, `stream(Stream, ZoneId)`, `bytes(Bytes, ZoneId)`, or `snapshot(File)`
  using `load/1` or `load/2`.
- Supports TZif v1, v2, and v3, including validated skipping of the v1
  compatibility block in v2/v3 files.
- Parses POSIX footers, including signed-hour and signed-minute offsets.
- Exposes zone-aware lookup predicates over loaded `tzif(...)` terms,
  plus cached convenience variants for zone and single-zone queries.
- Caches successful `load/1` calls automatically, while `load/2` returns a
  loaded term without changing the cache.
- Provides `cache/1` for making an already loaded `tzif(...)` term the active
  cached term.
- Saves and reloads `tzif(...)` terms as plain Prolog snapshot files, with
  `save/1` providing a convenience predicate for saving the cached term.
- Returns the source term recorded in the active cached term via `cache_source/1`.
- Exposes the active cached term using `cached_tzif/1`.
- Exposes the cached zone list using `zones/1`.
- Accepts UTC queries as Unix seconds or `date_time/6` terms.
- Validates zone identifiers against bundled IANA TZDB 2026a canonical names
  plus backward-compatible aliases.


API documentation
-----------------

Open the [../../apis/library_index.html#tzif](../../apis/library_index.html#tzif)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(tzif(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(tzif(tester)).


Representation
--------------

Loaded sources are returned by `load/2` as lists of `tzif(Zone, Source, ZoneData)`
terms, one term per loaded zone.

Each nested `ZoneData` term contains the parsed block data, leap-second
records, and parsed footer for a single zone. These per-zone `tzif(...)`
terms are stable enough to be serialized directly using `save/2` and restored
using `load(snapshot(File), TZifs)`.

For directory and file-set loads, zone identifiers are the relative paths used
to locate the TZif files inside the root directory, and each loaded term
records its own `file(File, ZoneId)` source.

When loading a `directory(Root)`, regular files whose relative paths are not
recognized zone identifiers are ignored. This allows loading system zoneinfo
trees that also contain metadata files such as `leapseconds`.

For single-zone file, stream, and byte-list loads, the caller must provide the
zone identifier explicitly using `file(Path, ZoneId)`, `stream(Stream, ZoneId)`,
or `bytes(Bytes, ZoneId)`.

The cache stores one `tzif(...)` term per zone id. `load/1` and `cache/1`
replace only cached entries whose zone ids match the newly loaded terms.

Zone identifier validation
--------------------------

The library validates zone identifiers using bundled data derived from the
official IANA TZDB 2026a release. Accepted identifiers include both canonical
zone names such as `America/New_York` and backward-compatible aliases such as
`US/Eastern`.

The bundled data is generated from the IANA `zone1970.tab` and `backward`
files and does not consult the host operating system's installed zoneinfo tree.

To refresh the bundled table for a newer IANA release, run:

    $ ./update_zone_ids.sh 2026a


Examples
--------

Load a single TZif payload without caching it:

    | ?- tzif::load(bytes(Bytes, 'America/New_York'), TZifs).

Cache an already loaded term explicitly:

      | ?- tzif::load(bytes(Bytes, 'America/New_York'), TZifs), tzif::cache(TZifs).

Load a directory tree of TZif files:

    | ?- tzif::load(directory('/usr/share/zoneinfo'), TZifs).

Query a specific zone in a loaded term:

    | ?- tzif::load(bytes(Bytes, 'America/New_York'), [TZif]).
    | ?- tzif::offset(TZif, 'America/New_York', date_time(2024, 7, 1, 12, 0, 0), Offset).

Query a specific zone in the cached term:

    | ?- tzif::load(directory('/usr/share/zoneinfo')).
    | ?- tzif::offset('America/New_York', date_time(2024, 7, 1, 12, 0, 0), Offset).

Persist and reload a snapshot:

    | ?- tzif::save(TZifs, 'snapshot.pl').
    | ?- tzif::load(snapshot('snapshot.pl'), ReloadedTZifs).

Save the cached terms directly:

    | ?- tzif::load(directory('/usr/share/zoneinfo')).
    | ?- tzif::save('snapshot.pl').

Load using a backward-compatible alias:

    | ?- tzif::load(bytes(Bytes, 'US/Eastern'), TZifs).

Use the automatic cache populated by `load/1`:

    | ?- tzif::load(snapshot('snapshot.pl')).
    | ?- tzif::abbreviation(date_time(2024, 7, 1, 12, 0, 0), Abbreviation).

List cached zones directly:

    | ?- tzif::load(directory('/usr/share/zoneinfo')).
    | ?- tzif::zones(Zones).


Limitations
-----------

- Local civil-time queries and ambiguity resolution are not yet implemented.
