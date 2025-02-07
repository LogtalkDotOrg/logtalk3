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

# threads - tak

This folder contains single-threaded and multi-threaded implementations 
of the Takeuchi function (recursive arithmetic). The multi-threaded version 
uses three threads per recursive call.

NOTE: some example queries below use a proprietary predicate `time/1` in
order to get accurate goal times. This predicate is found on several Prolog
systems. For other Prolog compilers, replace the `time/1` call by any
appropriate timing calls (e.g., `cputime/0`).

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(tak(loader)).
```

Single-threaded version:

```logtalk
time(tak(1)::tak(18, 12, 6, R)).
```

<!--
% 254,476 inferences, 0.06 CPU in 0.07 seconds (91% CPU, 4241267 Lips)

R = 7.
-->

Multi-threaded version:

```logtalk
time(tak(3)::tak(18, 12, 6, R)).
```

<!--
% 714 inferences, 0.06 CPU in 0.05 seconds (121% CPU, 11900 Lips)

R = 7.
-->

Single-threaded version:

```logtalk
time(tak(1)::tak(21, 14, 7, R)).
```

<!--
% 1,583,068 inferences, 0.52 CPU in 0.61 seconds (86% CPU, 3044362 Lips)

R = 14.
-->

Multi-threaded version:

```logtalk
time(tak(3)::tak(21, 14, 7, R)).
```

<!--
% 106 inferences, 0.48 CPU in 0.38 seconds (127% CPU, 221 Lips)

R = 14.
-->
