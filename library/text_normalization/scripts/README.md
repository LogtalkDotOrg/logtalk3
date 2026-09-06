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


# Text Normalization Data Refresh

Run `update_data.sh` to download the pinned Unicode 17.0.0 UCD files and the
WHATWG entity snapshot, verify their SHA-256 checksums, and regenerate the
numeric-only Logtalk tables in `../data`.

The Unicode data files are distributed under the Unicode License V3 at
<https://www.unicode.org/license.txt>. The entity data is sourced from the
WHATWG HTML Living Standard at <https://html.spec.whatwg.org/entities.json>.
Only the semicolon-terminated names listed in `common_entities.txt` are
included. The source files are retained under `sources` so that conformance
tests and byte-identical regeneration do not require another download.

Unicode 17.0.0 currently provides 20,034 normalization conformance vectors
and 1,585 full default case-fold mappings. The generator records these counts
in its outputs and rejects duplicate keys and invalid Unicode scalar values.

After refreshing, run the text normalization test suite with both SWI-Prolog
and SICStus Prolog. A Unicode version update requires reviewing expected fact
counts, conformance vectors, and any changes to generated mappings.
