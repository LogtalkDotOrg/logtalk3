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

# engines - yield

This example illustrates fetching answers from an engine that returns them using
the `threaded_engine_yield/1` built-in predicate. The original example was written
by Jan Wielemaker. Currently it runs on ECLiPSe and SWI-Prolog. It should run also
on XSB and YAP if and when these systems bugs with coroutining and/or threads get
fixed.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(yield(loader)).
```

Some sample queries:

```logtalk
yield::yield(1, List).
```

<!--
List = [1].
-->

```logtalk
yield::yield(3, List).
```

<!--
List = [1, 2, 3]
-->
