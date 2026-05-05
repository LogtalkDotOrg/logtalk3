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


# ISO 3166 Source Refresh

This directory contains the ISO 3166 source refresh tooling for the `iso_3166`
library.

Workflow:

1. Run `update_iso_3166.sh` to fetch the official UN M49 overview page into
	`sources/m49_overview.html`.
2. `generate_iso_3166.sh` regenerates `library/iso_3166/iso_3166_1.lgt` from
	the embedded country table, using the published ISO alpha-2/alpha-3 codes and
	M49 numeric codes.
3. Run `update_iso_3166_2.sh` to fetch the Debian `iso-codes` ISO 3166-2 JSON
	snapshot into `sources/iso_3166_2.json`.
4. `generate_iso_3166_2.py` regenerates `library/iso_3166/iso_3166_2.lgt`
	from that snapshot, lowercasing the country and subdivision codes to match the
	repository conventions.

Current status:

- ISO 3166-1 country facts are generated from the UN M49 overview page.
- ISO 3166-2 subdivision facts are generated from the Debian `iso-codes` JSON
	snapshot.
