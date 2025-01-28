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

# haunted_wasteland

This folder contains a Logtalk solution for the Advent of Code 2023 Day 8
problem:

https://adventofcode.com/2023/day/8

The `test_files` directory contain sample inputs copied from the problem
description.

Start by loading the example:

```logtalk
logtalk_load(haunted_wasteland(loader)).
```

<!--
true.
-->

Compute the number of steps (part 1) for the `test_files/sample_1` file:

```logtalk
haunted_wasteland::steps_1('test_files/sample_1', Steps).
```

<!--
Steps = 2.
-->

Compute the number of steps (part 1) for the `test_files/input` file:

```logtalk
haunted_wasteland::steps_1('test_files/input', Steps).
```

<!--
Steps = 21409.
-->

Compute the total number of steps (part 2) for the `test_files/input` file:

```logtalk
haunted_wasteland::steps_2('test_files/input', Steps).
```

<!--
Steps = 21165830176709.
-->
