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

This tool intercepts compiler linter warnings and caches them to generate
SARIF reports that can be used in CI/CD pipelines.


API documentation
-----------------

This tool API documentation is available at:

[../../apis/library_index.html#linter_reporter](../../apis/library_index.html#linter_reporter)


Loading
-------

Load the tool before loading the code to be checked:

	| ?- logtalk_load(linter_reporter(loader)).
    ...

    | ?- linter_reporter::enable.
    true.

    | ?- logtalk_load(my_application(loader)).
    ...

    | ?- linter_reporter::disable.
    true.

    | ?- linter_reporter::report.


Testing
-------

To test this tool, load the `tester.lgt` file:

	| ?- logtalk_load(linter_reporter(tester)).

The test suite reuses `errors` example files to exercise representative built-in
linter warnings and validates SARIF export in both explanation-disabled and
explanation-enabled configurations.


Usage
-----

Load the tool, call `enable/0` before compiling the code to be checked, call
`disable/0` when warning collection is finished, and then call `report/0-1` to
generate the SARIF report from the cached warnings.


Runtime flags
-------------

- `linter_reporter_file`
  Path to the generated SARIF report file.

- `linter_reporter_include_explanations`
  Boolean flag. When set to `true` (default is `false`), the tool enriches
  warnings with explanations from the `tutor_explanations` category provided
  by the `tutor` tool.
