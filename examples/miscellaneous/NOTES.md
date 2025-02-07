---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
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

# miscellaneous

- `hanoi.lgt`  
	Towers of Hanoi example

- `queens.lgt`  
	N-Queens example

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(miscellaneous(loader)).
```

Towers of Hanoi using three disks:

```logtalk
hanoi::run(3).
```

<!--
Move a disk from left to right.
Move a disk from left to middle.
Move a disk from right to middle.
Move a disk from left to right.
Move a disk from middle to left.
Move a disk from middle to right.
Move a disk from left to right.

true.
-->

Placing eight queens in a chess table:

```logtalk
queens::queens(8).
```

<!--
[1-5, 2-7, 3-2, 4-6, 5-3, 6-1, 7-4, 8-8] ;
[1-4, 2-7, 3-5, 4-2, 5-6, 6-1, 7-3, 8-8] ;
[1-6, 2-4, 3-7, 4-1, 5-3, 6-5, 7-2, 8-8] ;
[1-6, 2-3, 3-5, 4-7, 5-1, 6-4, 7-2, 8-8] ;
[1-4, 2-2, 3-8, 4-6, 5-1, 6-3, 7-5, 8-7] ;
...
-->
