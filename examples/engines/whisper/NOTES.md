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

# engines - whisper

This example creates a chain of engines passing a term. The original example was
written by Jan Wielemaker. Currently it runs on ECLiPSe, SWI-Prolog, and XVM. It
should run also on XSB and YAP if and when these systems bugs with coroutining
and/or threads get fixed.

When using XVM as the backend, this example must be run from the top-level
interpreter.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(whisper(loader)).
```

Some sample queries:

```logtalk
whisper::whisper(0, 1, Final).
```

<!--
1

Final = 1.
-->

```logtalk
whisper::whisper(1, 1, Final).
```

<!--
Sending 2 to engine 2
2

Final = 2.
-->

```logtalk
whisper::whisper(3, 1, Final).
```

<!--
Sending 2 to engine 6
Sending 3 to engine 5
Sending 4 to engine 4
4

Final = 4.
-->

```logtalk
whisper::whisper(5, 3, Final).
```

<!--
Sending 4 to engine 12
Sending 5 to engine 11
Sending 6 to engine 10
Sending 7 to engine 9
Sending 8 to engine 8
8

Final = 8.
-->
