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


`iso_9362`
==========

This library provides ISO 9362 Business Identifier Code (BIC) structure parsing
and normalization based on the public Swift and ISO 9362 documentation.
It requires Unicode support from the backend Prolog compiler.

The current public implementation focuses on the normative structure that Swift
and ISO 9362 publish openly: a BIC consists of a 4-character business party
prefix, a 2-character ISO 3166-1 alpha-2 country code, a 2-character business
party suffix, and an optional 3-character branch identifier. Eight-character
primary-office BICs are normalized to the branch code `XXX`.

This library does not currently ship the full ISO 9362 directory as facts. The
public SwiftRef TXT directory download is available only through a secure,
time-limited email link, so it is not treated as an automatable checked-in
source snapshot in the same way as the ISO 4217 and ISO 639 libraries.


API documentation
-----------------

Open the [../../apis/library_index.html#iso_9362](../../apis/library_index.html#iso_9362)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(iso_9362(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(iso_9362(tester)).
