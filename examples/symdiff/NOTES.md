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

# symdiff

This folder contains an example of using parametric objects to implement
symbolic expression differentiation and simplification.

Current limitations:

- the expression that we intend to differentiate or simplify must be a
compound term. Accepted functors are `*`, `+`, `-`, `**`, and `log`.

- use as a variable the atom `x`.

- only integers can be used as constants.

This example is still incomplete. For example, using sum distribution 
property to simplify expressions is not yet implemented. 

Start by loading the example:

```logtalk
logtalk_load(symdiff(loader)).
```

Simplify the expression `x^1 + x0 - x1`:

```logtalk
(x**1 + x*0 - x*1)::simplify(S).
```

<!--
S = 0.
-->

Differentiate and then simplify the expression `2x^3 + x^2 - 4x`:

```logtalk
(2*x**3 + x**2 - 4*x)::diff(D), D::simplify(S).
```

<!--
D = 2*(3*x**2*1)+2*x**1*1-4*1, S = 2*(3*x**2)+2*x-4.
-->

Differentiate and then simplify the expression `ln(x^2 + 2x - 7) + 4x`:

```logtalk
(log(x**2 + 2*x - 7) + 4*x)::diff(D), D::simplify(S).
```

<!--
D = (2*x**1*1+2*1)*(x**2+2*x-7)** -1+4*1, S = (2*x+2)*(x**2+2*x-7)** -1+4.
-->
