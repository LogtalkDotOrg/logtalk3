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

This folder contains a Logtalk version of a spreading activation nodes
example described in the Net-Clause Language (NCL) manual, available at:

https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/prolog/impl/parallel/ncl/0.html

The Logtalk version uses parametric objects to represent the concept of
"net-variables" described in the paper as "global logical variables" and
events to implement the functionality of spreading activation nodes.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(activation_nodes(loader)).
```

Run the example:

```logtalk
activation(_,_,_)::(a(a),b(b),c(c)).
```

<!--
T = 0
  a-_R2-_R1
T = 1
T = 0
  a-b-c

true.
-->
