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


# ISO 9362 Source Notes

This directory documents the public source strategy for the `iso_9362` library.

Current status:

- The library implements the public ISO 9362 BIC structure rules that Swift and
	`iso9362.org` publish openly.
- The public SwiftRef PDF directory is downloadable without authentication, but
	no portable text-extraction tool is assumed in this worktree.
- The public SwiftRef TXT directory download is not a stable anonymous file
	endpoint. It requires submission of a download request and the use of a
	secure, time-limited token delivered by email.

Implications:

- This library does not currently ship a generated full BIC directory fact
	table.
- If a future change checks in an official SwiftRef TXT snapshot under this
	directory, generator tooling can be added on top of the current structural
	predicates without changing the public API.
