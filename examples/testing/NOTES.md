---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.1'
      jupytext_version: 1.16.6
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
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
-->

# testing

This folder contains an example of defining and running unit tests using
the test dialects supported by default by the Logtalk `lgtunit` tool. See
also the `quick_check` and `tests_dsl` examples.

To define tests for your own project, see the `lgtunit` tool documentation
and the `tests-sample.lgt` and `tester-sample.lgt` sample files in the root
of the distribution.

Start by loading the example:

```logtalk
logtalk_load(testing(loader)).
```

The test will auto-run, thanks to initialization directives.

You may also edit the initialization directives in the objects
defined in the "testing.lgt" file in order to divert the test
results to a "results.txt" file in the example directory.
