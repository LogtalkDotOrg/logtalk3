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

# shapes - ph

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(shapes_ph(loader)).
```

Try some simple queries:

Objects playing the role of prototypes define predicates for themselves ...

```logtalk
square::nsides(N).
```

<!--
N = 4.
-->

... and for their descendants:

```logtalk
q2::area(A).
```

<!--
A = 9.
-->

Don't use message broadcasting syntax in order to workaround a XSB parser bug:

```logtalk
q1::color(Color), q1::side(Side), q1::position(X, Y).
```

<!--
Color = red, Side = 1, X = 0, Y = 0.
-->

Don't use message broadcasting syntax in order to workaround a XSB parser bug:

```logtalk
q2::side(Side), q2::area(Area), q2::perimeter(Perimeter).
```

<!--
Side = 3, Area = 9, Perimeter = 12.
-->
