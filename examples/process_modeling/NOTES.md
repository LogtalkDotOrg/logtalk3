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

# process_modeling

This folder contains an example of using parametric objects to represent
and restore shared variables between sets of constraints that are stored
in different objects. This solution is applied to process modeling where
constraints are used to represent run dependencies between processes.
Part of the process description is a set of finite domain constraints
describing how many times the process can or must be executed. An object
parameter is used to provide access to the process constraint variable.
See the comments in the `process_modeling.lgt` file for further details.

This example can be run using B-Prolog, ECLiPSe, GNU Prolog, SICStus
Prolog, SWI-Prolog, or YAP as the backend compiler (by using an ugly
solution thanks to the lack of standardization of CLP(FD) constraint
libraries).

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(process_modeling(loader)).
```

Compute the number of times that process `b` can be executed
and its dependencies:

```logtalk
%%table
process_model::solve([b(B)], Dependencies).
```

<!--
B = 2, Dependencies = [a(2),b(2)] ? ;
B = 3, Dependencies = [a(2),b(3)] ? ;
B = 3, Dependencies = [a(3),b(3)] ? ;
false.
-->

Compute the number of times that process `c` can be executed
and its dependencies:

```logtalk
%%table
process_model::solve([c(C)], Dependencies).
```

<!--
C = 3, Dependencies = [b(2),a(2),c(3)] ? ;
C = 4, Dependencies = [b(3),a(3),c(4)] ? ;
false.
-->
