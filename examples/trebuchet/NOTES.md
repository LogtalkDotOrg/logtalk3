---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
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

# trebuchet

This folder contains a Logtalk solution for the Advent of Code 2023 Day 1
problem:

https://adventofcode.com/2023/day/1

The solution provided is for Part 2 of the problem. To solve just for
Part 1, comment out the grammar rules for digits as letters.

The `test_files` directory contain sample inputs for part 1 of the problem
copied from the problem description. For copyright reasons, the part 2 files
cannot be distributed here and **must** be downloaded from the Advent of Code
2023 website.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(trebuchet(loader)).
```

<!--
true.
-->

Compute the calibration for the `test_files/sample_1` file (part 1):

```logtalk
object_property(trebuchet, file(_, Directory)),
atom_concat(Directory, 'test_files/sample_1', File),
trebuchet::solution(File, Calibration).
```

<!--
Calibration = 142.
-->

Compute the calibration for the `test_files/sample_2` file (part 1):

```logtalk
object_property(trebuchet, file(_, Directory)),
atom_concat(Directory, 'test_files/sample_2', File),
trebuchet::solution(File, Calibration).
```

<!--
Calibration = 281.
-->

Compute the calibration for the `test_files/input` file (part 2):

```logtalk
object_property(trebuchet, file(_, Directory)),
atom_concat(Directory, 'test_files/input', File),
trebuchet::solution(File, Calibration).
```

<!--
Calibration = 53894.
-->
