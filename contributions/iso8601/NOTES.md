________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 2014-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-FileCopyrightText: 2004-2005 Daniel L. Dudley  
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


`iso8601`
=========

ISO 8601 (and European civil calendar) compliant library of date and time
(clock) related predicates. That is, an ISO 8601 handler.

The library supports date representations plus ISO 8601 duration and interval
string conversions using `duration_string/2` and `interval_string/2`.

An **ISO 8601 duration string** represents a time amount (e.g. `P3D` for three
days or `P1Y2M3DT4H5M6S` for a mixed date/time duration).

An **ISO 8601 interval string** represents a time interval using two parts
separated by `/`, where each part can be a date or a duration
(e.g. `2026-02-25/2026-03-01` or `2026-02-25/P3D`).

Author:  Daniel L. Dudley
Created: 2004-02-18

Modified: 2014-09-26 (to use the `os` library object to get the current date)  
Modified: 2026-02-25 (to add `duration_string/2` and `interval_string/2` predicates)


API documentation
-----------------

Open the [../../apis/library_index.html#iso8601](../../apis/library_index.html#iso8601)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(iso8601(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(iso8601(tester)).
