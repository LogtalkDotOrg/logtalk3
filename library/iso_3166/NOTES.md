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


`iso_3166`
==========

This library provides generated ISO 3166 country and subdivision code lookups.
The current worktree includes a real ISO 3166-1 fact table generated from the
official United Nations M49 overview page, which republishes ISO alpha-2 and
alpha-3 country codes together with the M49 numeric codes used here.
It requires Unicode support from the backend Prolog compiler.

ISO 3166-2 subdivision data is bundled in the public facade and generated from
the Debian `iso-codes` machine-readable JSON snapshot. Subdivision codes and
country alpha-2 codes are stored as lowercase atoms to match the conventions
used by the generated ISO 3166-1 country fact table, while names and categories
preserve the source spelling.

To refresh the generated subdivision table, run:

	$ library/iso_3166/scripts/update_iso_3166_2.sh


API documentation
-----------------

Open the [../../apis/library_index.html#iso_3166](../../apis/library_index.html#iso_3166)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(iso_3166(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(iso_3166(tester)).
