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
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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

# diamonds

This example illustrates some variants of the "diamond problem" 
(multi-inheritance conflicts and ambiguities) and its respective 
solutions on Logtalk.

This classical problem can be simply described by constructing a 
"diamond" of objects and inheritance links as follows:

```text
    A       -- contains default definition for a predicate `m/0`
  /   \
B       C   -- contains redefinitions of the predicate `m/0`
  \   /
    D       -- inherits both redefinitions of the predicate `m/0`
```

As such, the object D inherits two conflicting definitions for the 
predicate `m/0`, one from object B and one from object C. If we send 
the message `m/0` to object D, is ambiguous which inherited definition 
should be used to answer it. Depending on the nature of the objects 
A, B, C, and D, the correct answer can be the redefinition of `m/0` in 
object B, the redefinition `m/0` in object C, or both redefinitions.
A programming language supporting multi-inheritance should provide 
programming mechanisms allowing easy implementation of each possible 
solution.

Note that, in the context of Logtalk, the diamond problem may occur with 
prototype hierarchies, class hierarchies, protocol hierarchies, or when 
using category composition.

This example deals with three variants of the diamond problem, illustrated 
using prototype hierarchies:

- `diamond1`  
	illustrates the inherited definition which is visible due to the
	Logtalk predicate lookup algorithm
- `diamond2`  
	presents a solution for making the overridden inherited definition 
	the visible one
- `diamond3`  
	presents a solution which allows both inherited definitions to be 
	used in D

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(diamonds(loader)).
```

First variant of the "diamond problem", defined in the `diamond1.lgt` source file:

```logtalk
d1::m.
```

<!--
Redefinition of method m/0 in object b1

true.
-->

Second variant of the "diamond problem", defined in the `diamond2.lgt` source file:

```logtalk
d2::m.
```

<!--
Redefinition of method m/0 in object c2

true.
-->

Third variant of the "diamond problem", defined in the `diamond3.lgt` source file:

```logtalk
d3::b3_m.
```

<!--
Redefinition of method m/0 in object b3

true.
-->

```logtalk
d3::c3_m.
```

<!--
Redefinition of method m/0 in object c3

true.
-->

```logtalk
d3::m.
```

<!--
Redefinition of method m/0 in object b3

true.
-->

```logtalk
d4::m.
```

<!--
Redefinition of method m/0 in object b3
Redefinition of method m/0 in object c3

true.
-->
