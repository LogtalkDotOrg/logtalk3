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


`iso_639`
=========

This library provides ISO 639 sets 1, 2, 3, and 5 lookups generated from the
Library of Congress and SIL maintenance-agency sources. It requires Unicode
support from the backend Prolog compiler.

The set 1 and set 2 data are generated from the Library of Congress ISO 639-2
code list. The set 3 data are generated from the SIL tab-delimited code set
download. The set 5 data are generated from the Library of Congress ISO 639-5
identifier list.

The refresh tooling lives in `library/iso_639/scripts/` together with the
checked-in source snapshots used to regenerate the fact tables using shell
scripts only.


API documentation
-----------------

Open the [../../apis/library_index.html#iso_639](../../apis/library_index.html#iso_639)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(iso_639(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(iso_639(tester)).
