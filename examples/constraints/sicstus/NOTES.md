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

This folder contains a set of experimental examples illustrating how to use
the CLP(FD) library distributed with SICStus Prolog 4.x with Logtalk.

The CLP(FD) library is loaded from the `loader.lgt` auxiliary loader file.
This library must always be loaded prior to compilation of the individual 
example files.

In most cases, objects and categories containing CLP(FD) code must be
compiled using the hook file ("sicstus_clpfd_hook.lgt") provided in this
directory (see the "loader.lgt" file for an example). This hook file
provides support for compilation of indexicals and overrides the meta-
predicate directives of the CLP(FD) meta-predicates. When using the CLP(FD)
`labeling/2` enumeration predicate, the `value/1` and `variable/1` options
are currently not supported.

Within objects and categories, is strongly recommended that you use the
`use_module/2` directive for the CLP(FD) module.

Start by loading the example:

```logtalk
logtalk_load(clp_sicstus(loader)).
```

```logtalk
%%table
cars_ix::cars_ix([ff], X).
```

<!--
X = [1,2,6,3,5,4,4,5,3,6] ? ;
X = [1,3,6,2,5,4,3,5,4,6] ? ;
X = [1,3,6,2,6,4,5,3,4,5] ? ;
X = [5,4,3,5,4,6,2,6,3,1] ? ;
X = [6,3,5,4,4,5,3,6,2,1] ? ;
X = [6,4,5,3,4,5,2,6,3,1] ? ;
false.
-->

```logtalk
%%table
cars_ix::cars_ix2([ff], X).
```

<!--
X = [1,2,6,3,5,4,4,5,3,6] ? ;
X = [1,3,6,2,5,4,3,5,4,6] ? ;
X = [1,3,6,2,6,4,5,3,4,5] ? ;
X = [5,4,3,5,4,6,2,6,3,1] ? ;
X = [6,3,5,4,4,5,3,6,2,1] ? ;
X = [6,4,5,3,4,5,2,6,3,1] ? ;
false.
-->

```logtalk
%%table
cars_ix::cars_ix3([ff], X).
```

<!--
X = [1,2,6,3,5,4,4,5,3,6] ? ;
X = [1,3,6,2,5,4,3,5,4,6] ? ;
X = [1,3,6,2,6,4,5,3,4,5] ? ;
X = [5,4,3,5,4,6,2,6,3,1] ? ;
X = [6,3,5,4,4,5,3,6,2,1] ? ;
X = [6,4,5,3,4,5,2,6,3,1] ? ;
false.
-->

```logtalk
squares::squares(dual,card).
```

<!--
[1,1,5,7,7,9,9,9]
[1,7,7,1,5,5,7,9]

true.
-->

```logtalk
squares::squares(dual,spec).
```

<!--
[1,7,7,1,7,9,5,5]
[1,1,5,7,9,9,7,9]

true.
-->

```logtalk
squares::squares(dual,wcd).
```

<!--
[1,1,5,7,7,9,9,9]
[1,7,7,1,5,5,7,9]

true.
-->

```logtalk
squares::squares(dual,disjoint).
```

<!--
[1,1,5,7,7,9,9,9]
[1,7,7,1,5,5,7,9]

true.
-->

```logtalk
torpedo::torpedo(id113).
```

<!--
+----------+
| #  #     |
| #    ## #|
| # ##     |
| #     ## |
|   # #    |
|     # ###|
|     #    |
| #        |
|          |
|          |
+----------+

true.
-->

```logtalk
smm::smm([leftmost],value).
```

<!--
[9,5,6,7,1,0,8,2].

true.
-->

```logtalk
smm::smm_ix([leftmost],value).
```

<!--
[9,5,6,7,1,0,8,2].

true.
-->
