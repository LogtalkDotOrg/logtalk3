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

# threads - buckets

This folder contains an implementation of an atomic updates task and it
was coded for a contribution to the Rosetta Code website.

For more information see:

https://rosettacode.org/wiki/Atomic_updates

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required libraries:

```logtalk
logtalk_load(buckets(loader)).
```

Start shuffling randomly the buckets contents (note that the bucket
values are random numbers and thus their sum varies by run and by the
backend):

```logtalk
buckets::start.
```

<!--
Sum of all bucket values: 52

[4,6,9,5,3,5,9,7,4,0]
[4,7,6,3,7,5,6,7,4,3]
[7,5,5,4,1,6,9,3,2,10]
[9,4,4,1,4,3,5,8,7,7]
[8,6,6,4,3,3,7,5,1,9]
[4,3,7,0,11,6,5,3,7,6]
[7,2,14,3,3,3,2,5,4,9]
[0,7,7,1,8,8,2,7,6,6]
[6,7,2,3,6,9,0,8,6,5]
[3,18,5,4,10,2,1,2,4,3]

true.
-->
