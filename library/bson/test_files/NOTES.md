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


MongoDB BSON corpus test files
==============================

The JSON files and the `bsonview` utility in this directory are copied from
the MongoDB specifications repository BSON corpus:

- https://github.com/mongodb/specifications/tree/master/source/bson-corpus/tests

The files were retrieved on 2026-08-03 from commit:

	615e0f9ca5f554614636c098225fbbf1be55565d

They are maintained by the MongoDB project and retain their upstream content.
See the MongoDB specifications repository for their history, licensing, and
the BSON corpus specification describing the fixture schema and expected
behavior.

The Logtalk tests consume the `canonical_bson` entries as successful parser
cases and semantic generator round-trip cases, and the `decodeErrors` BSON
entries as parser error cases. Extended JSON conversion and Decimal128 text
parsing cases are outside the `bson` library byte-list parser API.

Generator checks use semantic rather than byte-for-byte round trips because
standard Prolog cannot portably distinguish IEEE 754 negative zero from
positive zero. A backend may therefore regenerate a parsed negative zero using
the positive-zero encoding.
