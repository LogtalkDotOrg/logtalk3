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

# assign_parameters

This example illustrates the use of assignable variables and parametric 
objects as alternative implementation to dynamic object predicates for
storing (backtrackable) object state. For more information on assignable 
variables please consult the URL:

	http://www.kprolog.com/en/logical_assignment/

The objects in this example make use of the library category `assignvars`.
This category contains an adaptation of the pure logical subset implementation
of assignable variables by Nobukuni Kino, which can be found on the URL above.

Start by loading the "assign_parameters" category and the example:

```logtalk
logtalk_load(assign_parameters(loader)).
```

Rectangle example (don't use message broadcasting syntax in order to workaround a XSB parser bug):

```logtalk
rectangle(2, 3, S)::init, rectangle(2, 3, S)::position(X0, Y0), rectangle(2, 3, S)::move(3, 7), rectangle(2, 3, S)::position(X1, Y1), rectangle(2, 3, S)::move(2, 5), rectangle(2, 3, S)::position(X2, Y2).
```

<!--
X0 = 0, Y0 = 0, X1 = 3, Y1 = 7, X2 = 2, Y2 = 5.
-->

Finite state machine example:

```logtalk
{fsm(T, I, F)}::recognise([0,1,1,2,1,2,0]).
```

<!--
red-0-red
red-1-green
green-1-yellow
yellow-2-red
red-1-green
green-2-red
red-0-red

T = [red-0-red, red-1-green, red-2-red, yellow-0-red, yellow-1-green, yellow-2-red, green-0-yellow, ... -... -yellow, ... -...],
I = red,
F = [red]. 
-->

Finite state machine example:

```logtalk
{fsm(T, I, F)}::recognise([0,1,1,2,1,2,1,0]).
```

<!--
red-0-red
red-1-green
green-1-yellow
yellow-2-red
red-1-green
green-2-red
red-1-green
green-0-yellow
backtracking...
backtracking...
backtracking...
backtracking...
backtracking...
backtracking...
backtracking...
backtracking...

false.
-->
