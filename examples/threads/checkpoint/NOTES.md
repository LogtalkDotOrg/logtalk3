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

# threads - checkpoint

This folder contains an implementation of multi-threading barrier
synchronization task and it was coded for a contribution to the
Rosetta Code website.

For more information see:

https://rosettacode.org/wiki/Checkpoint_synchronization

When using XVM as the backend, this example must be run from the top-level
interpreter.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(checkpoint(loader)).
```

Run example:

```logtalk
checkpoint::run.
```

<!--
Worker 1 item 3
Worker 2 item 3
Worker 5 item 3
Worker 3 item 3
Worker 4 item 3
Assembly of item 3 done.
Worker 1 item 2
Worker 5 item 2
Worker 2 item 2
Worker 3 item 2
Worker 4 item 2
Assembly of item 2 done.
Worker 2 item 1
Worker 5 item 1
Worker 3 item 1
Worker 1 item 1
Worker 4 item 1
Assembly of item 1 done.
All assemblies done.

true.
-->
