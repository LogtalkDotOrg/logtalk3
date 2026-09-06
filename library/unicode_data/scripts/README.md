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


# Unicode Data Refresh

Run `update_data.sh` to download the pinned Unicode 17.0.0 UCD files, extracted
property files, Unihan variants, and normalization conformance data; verify
their SHA-256 checksums; and regenerate all runtime, compatibility, and
conformance tables in the parent directory.

The Unicode data files are distributed under the Unicode License V3 at
<https://www.unicode.org/license.txt>. The source files are downloaded to a
temporary directory and removed after generation so that they are not shipped
with the Logtalk distribution.

Unicode 17.0.0 currently provides 20,034 normalization conformance vectors
and 1,585 full default case-fold mappings. The generator records these counts
in its outputs and rejects duplicate keys and invalid Unicode scalar values.

After refreshing, run both the `unicode_data` and `text_normalization` test
suites with SWI-Prolog and SICStus Prolog, plus the Unicode conformance suite.
A Unicode version update requires reviewing expected fact counts, conformance
vectors, compatibility defaults, and any changes to generated mappings.
