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


`iso_13616`
===========

This library provides ISO 13616 International Bank Account Number (IBAN)
structure parsing, checksum validation, and normalization based on the public
ISO 13616 and Swift IBAN registry documentation. It requires Unicode support
from the backend Prolog compiler.

The current public implementation includes a checked-in snapshot of the public
SWIFT IBAN registry for all currently registered IBAN countries. Validation
therefore checks:

- the ISO 3166-1 alpha-2 country code
- the country-specific total IBAN length
- the country-specific BBAN structure pattern
- the standard MOD-97 checksum

The checked-in registry facts use a derived Prolog segment representation for
BBAN patterns, e.g. ``[a-4, n-6, n-8]`` instead of the original SWIFT text
syntax ``4!a6!n8!n``. This keeps the published structure information while
making validation code simpler and more direct.

The public API remains structural. The library validates the published national
IBAN formats but does not attempt to ship any bank directory or account
existence data beyond the public registry pattern definitions.


API documentation
-----------------

Open the [../../apis/library_index.html#iso_13616](../../apis/library_index.html#iso_13616)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(iso_13616(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(iso_13616(tester)).
