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

This folder contains a Logtalk version of a free nodes example described
in the Net-Clause Language (NCL) manual, available at:

https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/prolog/impl/parallel/ncl/0.html

The Logtalk version uses parametric objects to represent the concept of
"net-variables" described in the paper as "global logical variables".

Load the example:

```logtalk
logtalk_load(free_nodes(loader)).
```

Run the example:

```logtalk
free(_,_,_)::(a(1), b(2), c(C)).
```

<!--
C = 1+2.
-->
