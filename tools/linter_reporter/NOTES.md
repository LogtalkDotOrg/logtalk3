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


`linter_reporter`
=================

This tool intercepts compiler linter warnings and caches them as
machine-readable diagnostics. These diagnostics can be queried directly or
serialized as SARIF using the standalone `sarif` tool.


API documentation
-----------------

This tool API documentation is available at:

[../../apis/library_index.html#linter_reporter](../../apis/library_index.html#linter_reporter)


Loading
-------

Load the tool before loading the code to be checked:

	| ?- logtalk_load(linter_reporter(loader)).
    ...

Enable collecting linter warnings data using default options:

    | ?- linter_reporter::enable.
    true.

Or using explicit options:

    | ?- linter_reporter::enable([explanations(true)]).
    true.

Load the code for which you want diagnostics collected:

    | ?- logtalk_load(my_application(loader)).
    ...

Disable further collecting of linter warnings:

    | ?- linter_reporter::disable.
    true.

Query the cached diagnostics directly:

    | ?- linter_reporter::diagnostics(all, Diagnostics).
    ...

Warnings originating in an included file are not always file-scoped. When the
`include/1` directive appears inside an entity, the diagnostic context can be
that entity; otherwise the context is typically the included file.

Or generate a SARIF report using the standalone `sarif` tool:

    | ?- logtalk_load(sarif(loader)).
    ...

    | ?- sarif::generate(linter_reporter, all, file('./linter_warnings.sarif'), []).
    true.


Testing
-------

To test this tool, load the `tester.lgt` file:

	| ?- logtalk_load(linter_reporter(tester)).

The test suite reuses `errors` example files to exercise representative built-in
linter warnings and validates both the diagnostics API and standalone SARIF
generation in explanation-disabled and explanation-enabled configurations.


Usage
-----

Load the tool, call `enable/0-1` before compiling the code to be checked, call
`disable/0` when warning collection is finished, and then query the cached
warnings using either the legacy warning predicates or the diagnostics protocol
predicates. To generate SARIF from the cached diagnostics, load the standalone
`sarif` tool and call
`sarif::generate(linter_reporter, all, file('./linter_warnings.sarif'), []).`


Options
-------

- `explanations(Boolean)`
  Boolean option accepted by `enable/1`. When set to `true` (default is
  `false`), the tool enriches warnings with explanations from the
  `tutor_explanations` category provided by the `tutor` tool.
