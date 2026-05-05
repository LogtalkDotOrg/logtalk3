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


`iso_4217`
==========

This library provides ISO 4217 active non-fund currency code lookups generated
from the authoritative SIX maintenance-agency XML list together with active
fund currency code lookups. It requires Unicode support from the backend Prolog
compiler.

The generated fact table keeps one entry per XML row with a currency code,
therefore preserving entity-specific assignments for shared currencies such as
the Euro or the US Dollar. Rows marked by SIX as fund entries are exposed using
the `fund_currency/5` predicate while the `currency/5` predicate remains scoped
to non-fund entries. Minor units are represented either as integers or the atom
`na` when the source lists `N.A.`.


API documentation
-----------------

Open the [../../apis/library_index.html#iso_4217](../../apis/library_index.html#iso_4217)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(iso_4217(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(iso_4217(tester)).
