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

This folder contains a set of simple examples illustrating how to use the IC 
Constraint Solver library distributed with ECLiPSe with Logtalk.

These examples are adapted with permission from the examples found at:

http://www.eclipse-clp.org/examples

The examples code was changed to avoid using ECLiPSe special features (e.g., 
array notation or the `do/2` loop operator) as they do not work when the `iso` 
library is loaded (this library is loaded by the `eclipse*iso.pl` adapter 
file, which is used in the Logtalk integration scripts and shortcuts).

The Constraint Solver libraries are loaded from the `loader.lgt` auxiliary 
loader file. These libraries must always be loaded prior to compilation of 
the individual example files.

We must define an alias for the ECLiPSe `ic` library operator `(::)/2` in order 
to avoid conflicts with the `(::)/2` Logtalk message-sending operator. In the 
examples, the operator `ins/2` was chosen as the alias for the `(::)/2` operator.
ECLiPSE 6.0#78 adds an alias in_set_range/2 for `(::)/2` that could also be used.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(clp_eclipse(loader)).
```

```logtalk
puzzle::sendmore1(Digits).
```

<!--
Digits = [9, 5, 6, 7, 1, 0, 8, 2].
-->

```logtalk
puzzle::sendmore2(Digits).
```

<!--
Digits = [9, 5, 6, 7, 1, 0, 8, 2].
-->

```logtalk
steiner::steiner(9,X).
```

<!--
X = [[1, 2, 3], [1, 4, 5], [1, 6, 7], [1, 8, 9], [2, 4, 6], [2, 5, 8], [2, 7, 9], [3, 4, 9], [3, 5, 7], [3, 6, 8], [4, 7, 8], [5, 6, 9]] ;
X = [[1, 2, 3], [1, 4, 5], [1, 6, 7], [1, 8, 9], [2, 4, 6], [2, 5, 8], [2, 7, 9], [3, 4, 9], [3, 5, 7], [3, 6, 8], [4, 7, 8], [5, 6, 9]] ;
X = [[1, 2, 3], [1, 4, 5], [1, 6, 7], [1, 8, 9], [2, 4, 6], [2, 5, 8], [2, 7, 9], [3, 4, 9], [3, 5, 7], [3, 6, 8], [5, 6, 9], [4, 7, 8]] ;
(etc)
-->

```logtalk
tomography::go.
```

<!--
    0 0 7 1 6 3 4 5 2 7 0 0
 0                         
 0                         
 8      * * * * * * * *    
 2      *             *    
 6      *   * * * *   *    
 4      *   *     *   *    
 5      *   *   * *   *    
 3      *   *         *    
 7      *   * * * * * *    
 0                         
 0                         

true.
-->

```logtalk
zebra::zebra.
```

<!--
The japanese owns the zebra
The norwegian drinks water

true.
-->
