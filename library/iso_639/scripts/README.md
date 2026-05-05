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


# ISO 639 Source Refresh

This directory contains the ISO 639 source refresh tooling for the `iso_codes`
worktree.

Workflow:

1. Fetch the LoC ISO 639-2 HTML page, the LoC ISO 639-5 identifier list page,
	 and the SIL ISO 639-3 tab-delimited download into `sources/`.
2. Run `generate_iso_639.sh` via `update_iso_639.sh`.
3. Regenerate the Logtalk objects under `library/iso_639/`.

Current status:

- The generator emits `iso_639_1.lgt`, `iso_639_2.lgt`, `iso_639_3.lgt`, and
	`iso_639_5.lgt`.
- The source snapshots are stored under `sources/`.
