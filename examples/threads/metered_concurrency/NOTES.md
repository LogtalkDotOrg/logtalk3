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

# threads - metered_concurrency

This folder contains an implementation of a metered concurrency task and it
was coded for a contribution to the Rosetta Code website.

For more information see:

	https://rosettacode.org/wiki/Metered_concurrency#Logtalk

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(metered_concurrency(loader)).
```

Run example:

```logtalk
metered_concurrency::run.
```

<!--
Worker 1 acquired semaphore
Worker 6 acquired semaphore
Worker 1 releasing semaphore
Worker 2 acquired semaphore
Worker 6 releasing semaphore
Worker 5 acquired semaphore
Worker 2 releasing semaphore
Worker 7 acquired semaphore
Worker 5 releasing semaphore
Worker 3 acquired semaphore
Worker 7 releasing semaphore
Worker 4 acquired semaphore
Worker 3 releasing semaphore
Worker 4 releasing semaphore

true.
-->
