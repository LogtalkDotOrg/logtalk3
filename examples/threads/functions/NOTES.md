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

# threads - functions

This folder contains a simple multi-threading example illustrating how
to create threads for competing goals, which one performing the same
task using different methods. The first goal to complete leads to the
immediate termination of the threads running the remaining goals.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(functions(loader)).
```

Find the roots of some functions using each one of provided methods:

```logtalk
bisection::find_root(f1, 1.0, 2.3, 1.0e-15, Zero).
```

<!--
Zero = 2.0.
-->

```logtalk
newton::find_root(f1, 1.0, 2.3, 1.0e-15, Zero).
```

<!--
Zero = 2.0.
-->

```logtalk
muller::find_root(f1, 1.0, 2.3, 1.0e-15, Zero).
```

<!--
Zero = 2.0.
-->

```logtalk
bisection::find_root(f2, 1.0, 1.3, 1.0e-15, Zero).
```

<!--
Zero = 1.25809265664599.
-->

```logtalk
newton::find_root(f2, 1.0, 1.3, 1.0e-15, Zero).
```

<!--
Zero = 1.25809265664599.
-->

```logtalk
muller::find_root(f2, 1.0, 1.3, 1.0e-15, Zero).
```

<!--
Zero = 1.25809265664599.
-->

```logtalk
bisection::find_root(humps, -1.0, 2.0, 1.0e-15, Zero).
```

<!--
false.
-->

```logtalk
muller::find_root(humps, -1.0, 2.0, 1.0e-15, Zero).
```

<!--
Zero = 1.29954968258.
-->

```logtalk
newton::find_root(humps, -1.0, 2.0, 1.0e-15, Zero).
```

<!--
ERROR: is/2: Arithmetic: evaluation error: `float_overflow'
-->

Find the roots of some functions running all methods at once using multi-threading:

```logtalk
function_root::find_root(f1, 1.0, 2.3, 1.0e-15, Zero, Method).
```

<!--
Zero = 2.0, Method = bisection.
-->

```logtalk
function_root::find_root(f2, 1.0, 1.3, 1.0e-15, Zero, Method).
```

<!--
Zero = 1.25809265664599, Method = newton.
-->

```logtalk
function_root::find_root(f3, 0.0, 3.0, 1.0e-15, Zero, Method).
```

<!--
Zero = 1.4142135623731, Method = newton.
-->

```logtalk
function_root::find_root(f4, -1.0, 2.0, 1.0e-15, Zero, Method).
```

<!--
Zero = -8.88178419700125e-16, Method = bisection.
-->

```logtalk
function_root::find_root(humps, -1.0, 2.0, 1.0e-15, Zero, Method).
```

<!--
Zero = 1.29954968258482, Method = muller.
-->
