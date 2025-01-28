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

# engines - sums

This example illustrates using engines to accumulate state. The original example
was written by Jan Wielemaker. Currently it runs on ECLiPSe and SWI-Prolog. It
should run also on XSB and YAP if and when these systems bugs with coroutining
and/or threads get fixed.

Load the example:

```logtalk
logtalk_load(sums(loader)).
```

Some sample queries:

```logtalk
sums::rd(1, Sums).
```

<!--
Sums = [1].
-->

```logtalk
sums::rd(3, Sums).
```

<!--
Sums = [1, 3, 6].
-->

```logtalk
sums::rd(5, Sums).
```

<!--
Sums = [1, 3, 6, 10, 15].
-->
